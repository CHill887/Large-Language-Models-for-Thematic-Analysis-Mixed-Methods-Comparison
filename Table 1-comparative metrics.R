#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(tidyverse)
  library(irr)
  library(officer)
  library(flextable)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit == length(args)) return(default)
  args[hit + 1]
}

in_long     <- get_arg("--in_long", "out/aligned_long.csv")
human_truth <- get_arg("--human_truth", "out/adjudicated.csv")
out_dir     <- get_arg("--out_dir", "out")

HUMAN_1 <- get_arg("--human1", "human1.csv")
HUMAN_2 <- get_arg("--human2", "human2.csv")
LLM_1   <- get_arg("--llm1", "llm1.csv")
LLM_2   <- get_arg("--llm2", "llm2.csv")
LLM_3   <- get_arg("--llm3", "llm3.csv")
ADJUD   <- get_arg("--adjudicated", "adjudicated.csv")

n_boot <- as.integer(get_arg("--n_boot", "1000"))
conf   <- as.numeric(get_arg("--conf", "0.95"))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

percent_agree <- function(x, y) mean(x == y)

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

jaccard <- function(x, y) {
  TP <- sum(x == 1 & y == 1, na.rm = TRUE)
  FP <- sum(x == 0 & y == 1, na.rm = TRUE)
  FN <- sum(x == 1 & y == 0, na.rm = TRUE)
  if ((TP + FP + FN) == 0) return(NA_real_)
  TP / (TP + FP + FN)
}

f1_score <- function(x, y) {
  TP <- sum(x == 1 & y == 1, na.rm = TRUE)
  FP <- sum(x == 0 & y == 1, na.rm = TRUE)
  FN <- sum(x == 1 & y == 0, na.rm = TRUE)
  if (TP == 0 && (FP + FN) == 0) return(1)
  if (TP == 0) return(0)
  precision <- TP / (TP + FP)
  recall    <- TP / (TP + FN)
  if ((precision + recall) == 0) return(0)
  2 * precision * recall / (precision + recall)
}

sensitivity <- function(x, y) {
  TP <- sum(x == 1 & y == 1, na.rm = TRUE)
  FN <- sum(x == 1 & y == 0, na.rm = TRUE)
  den <- TP + FN
  if (den == 0) return(NA_real_)
  TP / den
}

specificity <- function(x, y) {
  TN <- sum(x == 0 & y == 0, na.rm = TRUE)
  FP <- sum(x == 0 & y == 1, na.rm = TRUE)
  den <- TN + FP
  if (den == 0) return(NA_real_)
  TN / den
}

kappa_fun <- function(x, y) {
  kfit <- tryCatch(irr::kappa2(data.frame(x, y), weight = "unweighted"),
                   error = function(e) NULL)
  if (is.null(kfit)) return(NA_real_)
  unname(kfit$value)
}

boot_ci_vec <- function(fun, x, y, n_boot = 1000, conf = 0.95) {
  n <- length(x); if (n == 0) return(c(NA, NA, NA))
  vals <- replicate(n_boot, {
    idx <- sample.int(n, replace = TRUE)
    tryCatch(fun(x[idx], y[idx]), error = function(e) NA_real_)
  })
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(c(NA, NA, NA))
  m  <- mean(vals)
  qs <- quantile(vals, probs = c((1 - conf)/2, 1 - (1 - conf)/2), na.rm = TRUE)
  c(mean = unname(m), low = unname(qs[1]), high = unname(qs[2]))
}

fmt_ci <- function(est, lo, hi, digits = 2, pct = FALSE) {
  if (all(is.na(c(est, lo, hi)))) return(NA_character_)
  scale <- if (pct) 100 else 1
  sprintf("%.*f (%.*f–%.*f)", digits, est*scale, digits, lo*scale, digits, hi*scale)
}

compute_metrics_one <- function(H, V, n_boot = 1000, conf = 0.95) {
  agree <- percent_agree(H, V)
  kap   <- kappa_fun(H, V)
  ac1   <- safe_ac1(H, V)
  jac   <- jaccard(H, V)
  sens  <- sensitivity(H, V)
  spec  <- specificity(H, V)
  f1    <- f1_score(H, V)
  agr_ci <- boot_ci_vec(percent_agree, H, V, n_boot, conf)
  kap_ci <- boot_ci_vec(kappa_fun,     H, V, n_boot, conf)
  ac1_ci <- boot_ci_vec(safe_ac1,      H, V, n_boot, conf)
  jac_ci <- boot_ci_vec(jaccard,       H, V, n_boot, conf)
  sen_ci <- boot_ci_vec(sensitivity,   H, V, n_boot, conf)
  spe_ci <- boot_ci_vec(specificity,   H, V, n_boot, conf)
  f1_ci  <- boot_ci_vec(f1_score,      H, V, n_boot, conf)
  tibble(
    agree_est = agree, agree_lo = agr_ci[2], agree_hi = agr_ci[3],
    kap_est   = kap,   kap_lo   = kap_ci[2], kap_hi   = kap_ci[3],
    ac1_est   = ac1,   ac1_lo   = ac1_ci[2], ac1_hi   = ac1_ci[3],
    jac_est   = jac,   jac_lo   = jac_ci[2], jac_hi   = jac_ci[3],
    sens_est  = sens,  sens_lo  = sen_ci[2], sens_hi  = sen_ci[3],
    spec_est  = spec,  spec_lo  = spe_ci[2], spec_hi  = spe_ci[3],
    f1_est    = f1,    f1_lo    = f1_ci[2],  f1_hi    = f1_ci[3]
  )
}

compute_metrics_group_mean <- function(H, V_list, n_boot = 1000, conf = 0.95) {
  m_agree <- mean(sapply(V_list, function(V) percent_agree(H, V)), na.rm = TRUE)
  m_kap   <- mean(sapply(V_list, function(V) kappa_fun(H, V)),     na.rm = TRUE)
  m_ac1   <- mean(sapply(V_list, function(V) safe_ac1(H, V)),      na.rm = TRUE)
  m_jac   <- mean(sapply(V_list, function(V) jaccard(H, V)),       na.rm = TRUE)
  m_sens  <- mean(sapply(V_list, function(V) sensitivity(H, V)),   na.rm = TRUE)
  m_spec  <- mean(sapply(V_list, function(V) specificity(H, V)),   na.rm = TRUE)
  m_f1    <- mean(sapply(V_list, function(V) f1_score(H, V)),      na.rm = TRUE)
  n <- length(H)
  reps <- replicate(n_boot, {
    idx <- sample.int(n, replace = TRUE)
    c(
      mean(sapply(V_list, function(V) percent_agree(H[idx], V[idx])), na.rm = TRUE),
      mean(sapply(V_list, function(V) kappa_fun(H[idx], V[idx])),     na.rm = TRUE),
      mean(sapply(V_list, function(V) safe_ac1(H[idx], V[idx])),      na.rm = TRUE),
      mean(sapply(V_list, function(V) jaccard(H[idx], V[idx])),       na.rm = TRUE),
      mean(sapply(V_list, function(V) sensitivity(H[idx], V[idx])),   na.rm = TRUE),
      mean(sapply(V_list, function(V) specificity(H[idx], V[idx])),   na.rm = TRUE),
      mean(sapply(V_list, function(V) f1_score(H[idx], V[idx])),      na.rm = TRUE)
    )
  })
  ci_lo <- apply(reps, 1, quantile, probs = (1 - conf)/2, na.rm = TRUE)
  ci_hi <- apply(reps, 1, quantile, probs = 1 - (1 - conf)/2, na.rm = TRUE)
  tibble(
    agree_est = m_agree, agree_lo = ci_lo[1], agree_hi = ci_hi[1],
    kap_est   = m_kap,   kap_lo   = ci_lo[2], kap_hi   = ci_hi[2],
    ac1_est   = m_ac1,   ac1_lo   = ci_lo[3], ac1_hi   = ci_hi[3],
    jac_est   = m_jac,   jac_lo   = ci_lo[4], jac_hi   = ci_hi[4],
    sens_est  = m_sens,  sens_lo  = ci_lo[5], sens_hi  = ci_hi[5],
    spec_est  = m_spec,  spec_lo  = ci_lo[6], spec_hi  = ci_hi[6],
    f1_est    = m_f1,    f1_lo    = ci_lo[7], f1_hi    = ci_hi[7]
  )
}

make_display_row <- function(name, m) {
  tibble(
    `Model/Coder`          = name,
    `Agreement % (95% CI)` = fmt_ci(m$agree_est, m$agree_lo, m$agree_hi, digits = 1, pct = TRUE),
    `Cohen's κ (95% CI)`   = fmt_ci(m$kap_est,   m$kap_lo,   m$kap_hi,   digits = 2),
    `Gwet's AC1 (95% CI)`  = fmt_ci(m$ac1_est,   m$ac1_lo,   m$ac1_hi,   digits = 2),
    `Jaccard (95% CI)`     = fmt_ci(m$jac_est,   m$jac_lo,   m$jac_hi,   digits = 2),
    `Sensitivity (95% CI)` = fmt_ci(m$sens_est,  m$sens_lo,  m$sens_hi,  digits = 2),
    `Specificity (95% CI)` = fmt_ci(m$spec_est,  m$spec_lo,  m$spec_hi,  digits = 2),
    `F1 (95% CI)`          = fmt_ci(m$f1_est,    m$f1_lo,    m$f1_hi,    digits = 2)
  )
}

pred <- readr::read_csv(in_long, show_col_types = FALSE) %>%
  mutate(
    SegmentID = as.character(SegmentID),
    Code      = as.character(Code),
    Coder     = as.character(Coder),
    Value     = as.integer(ifelse(is.na(Value), 0, Value != 0))
  )
stopifnot(all(pred$Value %in% c(0L, 1L)))
pred <- pred %>% filter(Coder != ADJUD)

needed <- c(HUMAN_1, HUMAN_2, LLM_1, LLM_2, LLM_3)
missing <- setdiff(needed, unique(pred$Coder))
if (length(missing) > 0) stop("Missing expected coders in aligned_long.csv: ", paste(missing, collapse = ", "))

truth_raw <- readr::read_csv(human_truth, show_col_types = FALSE)
truth_clean <- truth_raw %>% select(where(~ !all(is.na(.))))
artifact_cols <- names(truth_clean)[grepl("^Unnamed|^\\.\\.\\.", names(truth_clean))]
truth_clean <- truth_clean %>% select(-any_of(artifact_cols))
if (!("id" %in% names(truth_clean))) stop("Reference standard must contain 'id' column.")
if (!("speaker_label" %in% names(truth_clean))) {
  truth_clean <- truth_clean %>% mutate(speaker_label = NA_character_)
}
code_cols <- setdiff(names(truth_clean), c("id","speaker_label"))
if (length(code_cols) == 0) stop("No code/theme columns found in Reference standard.")

truth_long <- truth_clean %>%
  tidyr::pivot_longer(
    cols = all_of(code_cols),
    names_to = "Code",
    values_to = "HumanTruth_raw"
  ) %>%
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

rows <- list(
  make_display_row(HUMAN_1, compute_metrics_one(H, V_h1, n_boot, conf)),
  make_display_row(HUMAN_2, compute_metrics_one(H, V_h2, n_boot, conf)),
  make_display_row("Human mean", compute_metrics_group_mean(H, list(V_h1, V_h2), n_boot, conf)),
  make_display_row(LLM_1, compute_metrics_one(H, V_l1, n_boot, conf)),
  make_display_row(LLM_2, compute_metrics_one(H, V_l2, n_boot, conf)),
  make_display_row(LLM_3, compute_metrics_one(H, V_l3, n_boot, conf)),
  make_display_row("LLM mean", compute_metrics_group_mean(H, list(V_l1, V_l2, V_l3), n_boot, conf))
)

final_tbl <- bind_rows(rows)

csv_path <- file.path(out_dir, "overall_metrics_grouped.csv")
readr::write_csv(final_tbl, csv_path)

ft <- flextable::flextable(final_tbl)
ft <- flextable::set_table_properties(ft, width = 1, layout = "autofit")
ft <- flextable::theme_vanilla(ft)
ft <- flextable::fontsize(ft, size = 11, part = "all")
ft <- flextable::bold(ft, part = "header")
ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
ft <- flextable::align(ft, align = "left", j = 1)
ft <- flextable::align(ft, align = "center", j = 2:ncol(final_tbl))

human_mean_row <- which(final_tbl$`Model/Coder` == "Human mean")
llm_mean_row   <- which(final_tbl$`Model/Coder` == "LLM mean")
thick <- officer::fp_border(width = 2)

add_thick_box <- function(ft, row_idx) {
  if (!length(row_idx)) return(ft)
  ft <- flextable::border(ft, i = row_idx, j = 1:ncol(final_tbl),
                          border.top = thick, border.bottom = thick, part = "body")
  ft <- flextable::hline(ft, i = row_idx, border = thick, part = "body")
  if (row_idx > 1) ft <- flextable::hline(ft, i = row_idx - 1, border = thick, part = "body")
  ft
}
ft <- add_thick_box(ft, human_mean_row)
ft <- add_thick_box(ft, llm_mean_row)

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Overall agreement vs adjudicated Reference standard", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0("Order: ", HUMAN_1, ", ", HUMAN_2, ", Human mean; ",
         LLM_1, ", ", LLM_2, ", ", LLM_3, "; LLM mean. ",
         round(conf*100), "% percentile bootstrap CIs."),
  style = "Normal"
)
doc <- officer::body_add_par(doc, "", style = "Normal")
doc <- flextable::body_add_flextable(doc, ft)
doc <- officer::body_add_par(doc, "", style = "Normal")
doc <- officer::body_add_par(doc, paste0("Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")

docx_path <- file.path(out_dir, "overall_metrics_grouped.docx")
print(doc, target = docx_path)

message("Wrote:\n  - ", csv_path, "\n  - ", docx_path)
