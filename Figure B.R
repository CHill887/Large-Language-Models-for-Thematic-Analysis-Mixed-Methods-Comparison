#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
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

in_csv       <- get_arg("--in_csv", "out/inductive_comparative.csv")
out_dir      <- get_arg("--out_dir", "out")
delta_margin <- as.numeric(get_arg("--delta_margin", "-0.5"))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

raw <- readr::read_csv(in_csv, show_col_types = FALSE)
names(raw) <- trimws(names(raw))
raw <- raw %>% select(!matches("^Unnamed"))

ln <- tolower(names(raw))
fix <- function(target, new) {
  idx <- which(ln == tolower(target))
  if (length(idx) == 1) names(raw)[idx] <<- new
}
fix("Human analyst 1", "Human_1")
fix("Human analyst 2", "Human_2")
fix("ChatGPT",         "ChatGPT-5")
fix("Claude",          "Claude 4 Sonnet")
fix("QualiGPT",        "QualiGPT")

dat <- raw %>%
  mutate(across(everything(), ~ suppressWarnings(as.integer(.)))) %>%
  drop_na(Human_1, Human_2)

human_mean <- rowMeans(dat[, c("Human_1","Human_2")], na.rm = TRUE)
models <- intersect(c("ChatGPT-5", "Claude 4 Sonnet", "QualiGPT"), names(dat))
stopifnot(length(models) >= 1)

sum_rows <- purrr::map_df(models, \(m) {
  diff   <- dat[[m]] - human_mean
  diff   <- diff[!is.na(diff)]
  n      <- length(diff)
  d_mean <- mean(diff)
  d_sd   <- sd(diff)
  se     <- d_sd / sqrt(n)
  dfree  <- n - 1
  t_NI   <- (d_mean - delta_margin) / se
  p_NI   <- 1 - pt(t_NI, dfree)
  t_SUP  <- d_mean / se
  p_SUP  <- 1 - pt(t_SUP, dfree)
  tcrit  <- qt(0.95, dfree)
  ci_low <- d_mean - tcrit * se
  ci_high<- d_mean + tcrit * se
  tibble(
    model = m, n = n, delta_mean = d_mean, sd_diff = d_sd, se = se,
    ci_low = ci_low, ci_high = ci_high, p_NI_raw = p_NI, p_SUP_raw = p_SUP
  )
})

fmt_p <- function(p) ifelse(p < 1e-4, "<0.0001", sub("\\.?0+$","", sprintf("%.3f", p)))

sumtab <- sum_rows %>%
  mutate(
    p_NI_adj  = p.adjust(p_NI_raw,  method = "holm"),
    p_SUP_adj = p.adjust(p_SUP_raw, method = "holm"),
    p_NI_lbl  = fmt_p(p_NI_adj),
    p_SUP_lbl = fmt_p(p_SUP_adj),
    NI_met    = (ci_low > delta_margin) & (p_NI_adj  < 0.05),
    SUP_met   = (ci_low > 0)            & (p_SUP_adj < 0.05)
  ) %>%
  arrange(desc(delta_mean)) %>%
  mutate(
    delta_mean = round(delta_mean, 2),
    sd_diff    = round(sd_diff, 2),
    se         = round(se, 2),
    ci_low     = round(ci_low, 2),
    ci_high    = round(ci_high, 2),
    p_NI_raw   = signif(p_NI_raw, 2),
    p_SUP_raw  = signif(p_SUP_raw, 2),
    p_NI_adj   = signif(p_NI_adj, 2),
    p_SUP_adj  = signif(p_SUP_adj, 2)
  )

sumtab$model <- factor(sumtab$model, levels = sumtab$model)

x_min_ci <- min(c(sumtab$ci_low, delta_margin), na.rm = TRUE)
x_max_ci <- max(sumtab$ci_high, na.rm = TRUE)
y_header <- length(levels(sumtab$model)) + 0.6

sumtab <- sumtab %>%
  mutate(
    y = model,
    x_NI_col  = x_max_ci + 0.5,
    x_SUP_col = x_max_ci + 1.1
  )

x_scale_min <- x_min_ci - 0.01
x_scale_max <- max(sumtab$x_SUP_col, na.rm = TRUE) + 0.9
x_axis_min  <- x_min_ci - 0.01
x_axis_max  <- x_max_ci + 0.01

p_forest <- ggplot(sumtab, aes(y = model, x = delta_mean)) +
  geom_segment(aes(x = ci_low, xend = ci_high, y = y, yend = y), linewidth = 0.9) +
  geom_point(size = 2.8) +
  geom_vline(xintercept = delta_margin, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(
    limits = c(x_scale_min, x_scale_max),
    breaks = seq(-1, 1, 0.5),
    labels = number_format(accuracy = 0.1)
  ) +
  labs(
    x = expression(paste("Likert Scale Difference (LLM \u2212 Human mean)")),
    y = NULL
  ) +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(linewidth = 0.6, colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.length = unit(3, "pt"),
    axis.title.x = element_text(size = 10),
    plot.margin = margin(30, 210, 60, 10)
  ) +
  coord_cartesian(xlim = c(x_axis_min, x_axis_max), clip = "off") +
  geom_text(aes(x = x_NI_col,  y = y, label = p_NI_lbl),  inherit.aes = FALSE, hjust = 0, size = 3) +
  geom_text(aes(x = x_SUP_col, y = y, label = p_SUP_lbl), inherit.aes = FALSE, hjust = 0, size = 3) +
  annotate("text", x = unique(sumtab$x_NI_col),  y = y_header, label = "p (NI)",  fontface = 2, hjust = 0, vjust = 0.2, size = 3) +
  annotate("text", x = unique(sumtab$x_SUP_col), y = y_header, label = "p (SUP)", fontface = 2, hjust = 0, vjust = 0.2, size = 3) +
  annotation_custom(grob = textGrob("Non-inferiority margin", x = unit(0.22,"npc"), y = unit(1.08,"npc"), gp = gpar(col = "grey20", fontsize = 9))) +
  annotation_custom(grob = textGrob("Favours human", x = unit(0.3,"npc"), y = unit(-0.45,"npc"), gp = gpar(col = "grey20", fontsize = 9))) +
  annotation_custom(grob = textGrob("Favours LLM",   x = unit(0.68,"npc"), y = unit(-0.45,"npc"), gp = gpar(col = "grey20", fontsize = 9))) +
  annotation_custom(grob = segmentsGrob(x0 = unit(0.55, "npc"), x1 = unit(0.82, "npc"), y0 = unit(-0.57, "npc"), y1 = unit(-0.57, "npc"), gp = gpar(col = "black", fill="black"), arrow = arrow(type = "closed", length = unit(0.18, "cm")))) +
  annotation_custom(grob = segmentsGrob(x0 = unit(0.45, "npc"), x1 = unit(0.18, "npc"), y0 = unit(-0.57, "npc"), y1 = unit(-0.57, "npc"), gp = gpar(col = "black", fill="black"), arrow = arrow(type = "closed", length = unit(0.18, "cm"))))

fig_png <- file.path(out_dir, "inductive_forest_from_mean_sd.png")
fig_pdf <- file.path(out_dir, "inductive_forest_from_mean_sd.pdf")
ggsave(fig_png, p_forest, width = 7.4, height = 3.8, dpi = 600)
ggsave(fig_pdf, p_forest, width = 7.4, height = 3.8, device = grDevices::cairo_pdf)

readr::write_csv(
  sumtab %>%
    transmute(
      Model = model, n, delta_mean, sd_diff, se, ci_low, ci_high,
      p_NI_raw, p_NI_adj, NI_met, p_SUP_raw, p_SUP_adj, SUP_met
    ),
  file.path(out_dir, "inductive_from_mean_sd_summary.csv")
)

message("Wrote:\n  - ", fig_png,
        "\n  - ", fig_pdf,
        "\n  - Table: ", file.path(out_dir, "inductive_from_mean_sd_summary.csv"))
