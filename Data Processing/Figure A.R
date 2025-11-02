#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
  library(irr)
  library(ggplot2)
  library(scales)
  library(grid)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit == length(args)) return(default)
  args[hit + 1]
}

in_long      <- get_arg("--in_long", "out/aligned_long.csv")
human_truth  <- get_arg("--human_truth", "out/adjudicated.csv")
out_dir      <- get_arg("--out_dir", "out")
HUMAN_1      <- get_arg("--human1", "human1.csv")
HUMAN_2      <- get_arg("--human2", "human2.csv")
LLM_1        <- get_arg("--llm1", "llm1.csv")
LLM_2        <- get_arg("--llm2", "llm2.csv")
LLM_3        <- get_arg("--llm3", "llm3.csv")
ADJUD        <- get_arg("--adjudicated", "adjudicated.csv")
B            <- as.integer(get_arg("--boot", "1000"))
delta_margin <- as.numeric(get_arg("--delta_margin", "-0.03"))
seed_val     <- as.integer(get_arg("--seed", "2025"))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

safe_ac1 <- function(x, y) {
  x <- as.integer(ifelse(is.na(x), 0, x != 0))
  y <- as.integer(ifelse(is.na(y), 0, y != 0))
  tab <- table(factor(x, levels = c(0,1)), factor(y, levels = c(0,1)))
  n <- sum(tab); if (n == 0) return(NA_real_)
  Po <- sum(diag(tab)) / n
  p1 <- sum(rowSums(tab)[2]) / n
  p2 <- sum(colSums(tab)[2]) / n
  pbar <- (p1 + p2) / 2
  Pe <- 2 * pbar * (1 - pbar)
  if (isTRUE(all.equal(Pe, 1))) return(NA_real_)
  (Po - Pe) / (1 - Pe)
}

pred <- readr::read_csv(in_long, show_col_types = FALSE) %>%
  mutate(
    SegmentID = as.character(SegmentID),
    Code      = as.character(Code),
    Coder     = as.character(Coder),
    Value     = as.integer(ifelse(is.na(Value), 0, Value != 0))
  ) %>%
  filter(Coder != ADJUD)

truth_raw <- readr::read_csv(human_truth, show_col_types = FALSE)
truth_clean <- truth_raw %>% select(where(~ !all(is.na(.))))
artifact_cols <- names(truth_clean)[grepl("^Unnamed|^\\.\\.\\.", names(truth_clean))]
truth_clean <- truth_clean %>% select(-any_of(artifact_cols))
stopifnot("id" %in% names(truth_clean))
if (!("speaker_label" %in% names(truth_clean))) truth_clean$speaker_label <- NA_character_

code_cols <- setdiff(names(truth_clean), c("id","speaker_label"))
truth_long <- truth_clean %>%
  pivot_longer(all_of(code_cols), names_to = "Code", values_to = "HumanTruth_raw") %>%
  mutate(
    SegmentID  = as.character(id),
    Code       = as.character(Code),
    HumanTruth = as.integer(ifelse(is.na(HumanTruth_raw), 0, HumanTruth_raw != 0))
  ) %>%
  select(SegmentID, Code, HumanTruth) %>%
  arrange(SegmentID, Code)

H <- truth_long$HumanTruth

build_V <- function(coder_name) {
  pred %>%
    filter(Coder == coder_name) %>%
    select(SegmentID, Code, Value) %>%
    right_join(truth_long %>% select(SegmentID, Code), by = c("SegmentID","Code")) %>%
    arrange(SegmentID, Code) %>%
    mutate(Value = as.integer(ifelse(is.na(Value), 0, Value != 0))) %>%
    pull(Value)
}

V_h1 <- build_V(HUMAN_1)
V_h2 <- build_V(HUMAN_2)
V_l1 <- build_V(LLM_1)
V_l2 <- build_V(LLM_2)
V_l3 <- build_V(LLM_3)
V_h_list <- list(V_h1, V_h2)

set.seed(seed_val)

boot_delta <- function(H, V_h_list, V_m, B = 10000) {
  n <- length(H)
  out <- numeric(B)
  for (b in seq_len(B)) {
    idx <- sample.int(n, replace = TRUE)
    ac1_h <- mean(sapply(V_h_list, function(v) safe_ac1(v[idx], H[idx])), na.rm = TRUE)
    ac1_m <- safe_ac1(V_m[idx], H[idx])
    out[b] <- ac1_m - ac1_h
  }
  out
}

bd_chatgpt <- boot_delta(H, V_h_list, V_l1, B)
bd_claude  <- boot_delta(H, V_h_list, V_l2, B)
bd_quali   <- boot_delta(H, V_h_list, V_l3, B)

boot_deltas <- tibble(
  model = rep(c("ChatGPT-5", "Claude 4 Sonnet", "QualiGPT"), each = B),
  delta = c(bd_chatgpt, bd_claude, bd_quali)
)

fmt_p <- function(p) {
  out <- ifelse(p < 1e-4, "<0.0001", sprintf("%.4f", p))
  sub("\\.?0+$", "", out)
}

sumtab_raw <- boot_deltas %>%
  group_by(model) %>%
  summarise(
    delta_mean = mean(delta, na.rm = TRUE),
    ci_low     = quantile(delta, 0.025, na.rm = TRUE),
    ci_high    = quantile(delta, 0.975, na.rm = TRUE),
    p_NI_raw   = mean(delta <= delta_margin),
    p_SUP_raw  = mean(delta <= 0),
    .groups    = "drop"
  )

sumtab <- sumtab_raw %>%
  mutate(
    p_NI_adj  = p.adjust(p_NI_raw, method = "holm"),
    p_SUP_adj = p.adjust(p_SUP_raw, method = "holm"),
    p_NI_lbl  = fmt_p(p_NI_adj),
    p_SUP_lbl = fmt_p(p_SUP_adj),
    NI_met    = (ci_low > delta_margin) & (p_NI_adj  < 0.05),
    SUP_met   = (ci_low > 0)            & (p_SUP_adj < 0.05)
  ) %>%
  arrange(desc(delta_mean))

boot_deltas$model <- factor(boot_deltas$model, levels = sumtab$model)
sumtab$model      <- factor(sumtab$model,      levels = sumtab$model)

x_min    <- min(c(sumtab$ci_low, delta_margin)) - 0.01
x_max_ci <- max(sumtab$ci_high)

sumtab <- sumtab %>%
  mutate(
    y = model,
    x_NI_col  = x_max_ci + 0.035,
    x_SUP_col = x_max_ci + 0.065
  )

x_right_gutter <- 0.10
y_header <- length(levels(sumtab$model)) + 0.6

p_forest <- ggplot(sumtab, aes(y = model, x = delta_mean)) +
  geom_segment(aes(x = ci_low, xend = ci_high, y = model, yend = model), linewidth = 0.9) +
  geom_point(size = 2.8) +
  geom_vline(xintercept = delta_margin, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0,            linetype = "solid",  color = "black") +
  scale_x_continuous(
    limits = c(x_min, x_max_ci + x_right_gutter),
    breaks = seq(-0.04, 0.04, by = 0.02),
    labels = scales::number_format(accuracy = 0.02)
  ) +
  labs(
    title = "",
    x = expression(paste(" Difference in Gwet's AC1 (LLM \u2212 Human mean)")),
    y = NULL
  ) +
  theme_minimal(base_size = 12, base_family = "Ariel") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(linewidth = 0.6, colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.length = unit(3, "pt"),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(size = 10),
    plot.margin = margin(30, 200, 60, 10)
  ) +
  coord_cartesian(xlim = c(-0.04, 0.04), clip = "off")

p_forest <- p_forest +
  geom_text(
    data = sumtab,
    aes(x = x_NI_col,  y = y, label = p_NI_lbl),
    inherit.aes = FALSE, hjust = 0, size = 3
  ) +
  geom_text(
    data = sumtab,
    aes(x = x_SUP_col, y = y, label = p_SUP_lbl),
    inherit.aes = FALSE, hjust = 0, size = 3
  ) +
  annotate("text",
           x = unique(sumtab$x_NI_col),  y = y_header,
           label = "p (NI)", fontface = 2,
           hjust = 0, vjust = 0.2, size = 3) +
  annotate("text",
           x = unique(sumtab$x_SUP_col), y = y_header,
           label = "p (SUP)", fontface = 2,
           hjust = 0, vjust = 0.2, size = 3)

p_forest <- p_forest +
  annotation_custom(
    grob = textGrob(
      "Non-inferiority margin",
      x = unit(0.18, "npc"),
      y = unit(1.08, "npc"),
      gp = gpar(col = "grey20", fontsize = 9)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(0.45, "npc"), x1 = unit(0.18, "npc"),
      y0 = unit(-0.6, "npc"), y1 = unit(-0.6, "npc"),
      gp = gpar(col = "black", fill = "black"),
      arrow = arrow(type = "closed", length = unit(0.18, "cm"))
    )
  ) +
  annotation_custom(
    grob = textGrob(
      "Favours human",
      x = unit(0.30, "npc"),
      y = unit(-0.45, "npc"),
      gp = gpar(col = "grey20", fontsize = 9)
    )
  ) +
  annotation_custom(
    grob = segmentsGrob(
      x0 = unit(0.55, "npc"), x1 = unit(0.82, "npc"),
      y0 = unit(-0.6, "npc"), y1 = unit(-0.6, "npc"),
      gp = gpar(col = "black", fill = "black"),
      arrow = arrow(type = "closed", length = unit(0.18, "cm"))
    )
  ) +
  annotation_custom(
    grob = textGrob(
      "Favours LLM",
      x = unit(0.68, "npc"),
      y = unit(-0.45, "npc"),
      gp = gpar(col = "grey20", fontsize = 9)
    )
  )

fig_png <- file.path(out_dir, "figure_forest_delta_ac1.png")
ggsave(fig_png, p_forest, width = 7.4, height = 3.8, dpi = 600)


readr::write_csv(
  sumtab %>%
    transmute(Model = model,
              delta_mean, ci_low, ci_high,
              p_NI_raw,  p_NI_adj,  NI_met,
              p_SUP_raw, p_SUP_adj, SUP_met),
  file.path(out_dir, "forest_delta_ac1_linear_holm_summary.csv")
)

message("Wrote:\n  - ", fig_png,
        "\n  - ",
        "\n  - Table: ", file.path(out_dir, "forest_delta_ac1.csv"))
