
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
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

parse_list <- function(x) unlist(strsplit(x, ",\\s*"))
HUMANS <- parse_list(get_arg("--humans", "human1.csv,human2.csv"))
LLMS   <- parse_list(get_arg("--llms",   "llm1.csv,llm2.csv,llm3.csv"))

mk_labels <- function(humans, llms) {
  h_labs <- setNames(paste("Human", seq_along(humans)), humans)
  l_labs <- setNames(paste("LLM",   seq_along(llms)),   llms)
  c(h_labs, l_labs)
}
display_labels <- mk_labels(HUMANS, LLMS)

B    <- as.integer(get_arg("--boot", "1000"))
CONF <- as.numeric(get_arg("--conf", "0.95"))
set.seed(123)

safe_kappa <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  df <- data.frame(x = as.integer(x), y = as.integer(y))
  if (length(unique(df$x)) < 2 && length(unique(df$y)) < 2) return(NA_real_)
  out <- tryCatch(irr::kappa2(df, weight = "unweighted")$value, error = function(e) NA_real_)
  unname(out)
}

boot_kappa_one <- function(reference_vec, coder_vec, B = 1000, conf = 0.95) {
  n <- length(reference_vec)
  if (n == 0) return(c(NA_real_, NA_real_, NA_real_))
  vals <- replicate(B, {
    idx <- sample.int(n, replace = TRUE)
    safe_kappa(reference_vec[idx], coder_vec[idx])
  })
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(c(NA_real_, NA_real_, NA_real_))
  c(mean(vals, na.rm = TRUE),
    quantile(vals, probs = (1 - conf)/2, na.rm = TRUE, type = 8),
    quantile(vals, probs = 1 - (1 - conf)/2, na.rm = TRUE, type = 8))
}

boot_kappa_group_mean <- function(reference_vec, coder_mat, B = 1000, conf = 0.95) {
  n <- length(reference_vec)
  if (n == 0 || is.null(dim(coder_mat))) return(c(NA_real_, NA_real_, NA_real_))
  k <- ncol(coder_mat)
  vals <- replicate(B, {
    idx <- sample.int(n, replace = TRUE)
    kappas <- sapply(seq_len(k), function(j) safe_kappa(reference_vec[idx], coder_mat[idx, j]))
    if (all(is.na(kappas))) NA_real_ else mean(kappas, na.rm = TRUE)
  })
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(c(NA_real_, NA_real_, NA_real_))
  c(mean(vals, na.rm = TRUE),
    quantile(vals, probs = (1 - conf)/2, na.rm = TRUE, type = 8),
    quantile(vals, probs = 1 - (1 - conf)/2, na.rm = TRUE, type = 8))
}

fmt_ci <- function(est, lo, hi, digits = 2) {
  if (any(is.na(c(est, lo, hi)))) return(NA_character_)
  sprintf("%.*f (%.*f–%.*f)", digits, est, digits, lo, digits, hi)
}

pred <- readr::read_csv(in_long, show_col_types = FALSE) %>%
  mutate(
    SegmentID = as.character(SegmentID),
    Code      = as.character(Code),
    Coder     = as.character(Coder),
    Value     = as.integer(ifelse(is.na(Value), 0, Value != 0))
  )
stopifnot(all(pred$Value %in% c(0L, 1L)))

truth_raw <- readr::read_csv(human_truth, show_col_types = FALSE)
truth_clean <- truth_raw %>%
  select(where(~ !all(is.na(.)))) %>%
  select(-matches("^Unnamed|^\\.\\.\\."))

if (!("id" %in% names(truth_clean))) stop("reference standard file must contain an 'id' column.")
if (!("speaker_label" %in% names(truth_clean))) {
  truth_clean <- truth_clean %>% mutate(speaker_label = NA_character_)
}

code_cols <- setdiff(names(truth_clean), c("id","speaker_label"))
if (length(code_cols) == 0) stop("No code/theme columns found in reference standard.")

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
  select(SegmentID, Code, HumanTruth)

missing_h <- setdiff(HUMANS, unique(pred$Coder))
missing_l <- setdiff(LLMS,   unique(pred$Coder))
if (length(missing_h) || length(missing_l)) {
  stop("Missing coders in aligned_long.csv:\n  Humans: ",
       paste(missing_h, collapse = ", "),
       "\n  LLMs: ",
       paste(missing_l, collapse = ", "))
}

codes <- sort(unique(truth_long$Code))
aligned_by_code <- lapply(codes, function(cd) {
  reference <- truth_long %>% filter(Code == cd) %>% select(SegmentID, HumanTruth)
  get_coder <- function(name) {
    pred %>% filter(Code == cd, Coder == name) %>%
      select(SegmentID, Value) %>% rename(!!name := Value)
  }
  human_list <- lapply(HUMANS, get_coder)
  llm_list   <- lapply(LLMS,   get_coder)
  df <- reference
  for (d in c(human_list, llm_list)) df <- df %>% left_join(d, by = "SegmentID")
  df[HUMANS] <- lapply(df[HUMANS], function(v) as.integer(ifelse(is.na(v), 0L, v != 0)))
  df[LLMS]   <- lapply(df[LLMS],   function(v) as.integer(ifelse(is.na(v), 0L, v != 0)))
  list(
    Code = cd,
    reference = as.integer(df$HumanTruth),
    humans_mat = as.matrix(df[HUMANS]),
    llms_mat   = as.matrix(df[LLMS])
  )
})
names(aligned_by_code) <- codes

rows <- lapply(aligned_by_code, function(item) {
  cd   <- item$Code
  reference <- item$reference
  human_cols <- setNames(seq_along(HUMANS), HUMANS)
  human_out <- lapply(names(human_cols), function(nm) {
    estlohi <- boot_kappa_one(reference, item$humans_mat[, human_cols[[nm]]], B = B, conf = CONF)
    tibble(coder = nm, est = estlohi[1], lo = estlohi[2], hi = estlohi[3])
  }) %>% bind_rows()
  llm_cols <- setNames(seq_along(LLMS), LLMS)
  llm_out <- lapply(names(llm_cols), function(nm) {
    estlohi <- boot_kappa_one(reference, item$llms_mat[, llm_cols[[nm]]], B = B, conf = CONF)
    tibble(coder = nm, est = estlohi[1], lo = estlohi[2], hi = estlohi[3])
  }) %>% bind_rows()
  h_mean <- boot_kappa_group_mean(reference, item$humans_mat, B = B, conf = CONF)
  l_mean <- boot_kappa_group_mean(reference, item$llms_mat,   B = B, conf = CONF)
  out <- tibble(Code = cd)
  for (nm in HUMANS) {
    lab <- paste0(display_labels[[nm]], " κ")
    rec <- human_out %>% filter(coder == nm)
    val <- if (nrow(rec) == 0) NA_character_ else fmt_ci(rec$est, rec$lo, rec$hi, 2)
    out[[lab]] <- val
  }
  for (nm in LLMS) {
    lab <- paste0(display_labels[[nm]], " κ")
    rec <- llm_out %>% filter(coder == nm)
    val <- if (nrow(rec) == 0) NA_character_ else fmt_ci(rec$est, rec$lo, rec$hi, 2)
    out[[lab]] <- val
  }
  out[["Human mean κ"]] <- fmt_ci(h_mean[1], h_mean[2], h_mean[3], 2)
  out[["LLM mean κ"]]   <- fmt_ci(l_mean[1], l_mean[2], l_mean[3], 2)
  out
})

kappa_by_theme <- bind_rows(rows)

csv_path <- file.path(out_dir, "kappa_by_theme_vs_referencestandard_bootCIs.csv")
readr::write_csv(kappa_by_theme, csv_path)

ft <- flextable::flextable(kappa_by_theme)
ft <- flextable::theme_vanilla(ft)
ft <- flextable::set_table_properties(ft, width = 1, layout = "autofit")
ft <- flextable::set_caption(
  ft,
  caption = paste0(
    "Table 1. Cohen’s kappa (κ) by theme vs adjudicated reference standard with ",
    round(CONF*100), "% bootstrap CIs (B=", B, ")."
  )
)
ft <- flextable::fontsize(ft, size = 10, part = "all")
ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
ft <- flextable::bold(ft, part = "header")
ft <- flextable::align(ft, align = "left",  j = 1)
ft <- flextable::align(ft, align = "center", j = 2:ncol(kappa_by_theme))
ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2)
ft <- flextable::width(ft, j = 1, width = 1.8)
ft <- flextable::width(ft, j = 2:ncol(kappa_by_theme), width = 1.6)
ft <- flextable::add_footer_lines(
  ft,
  values = "κ values are unweighted Cohen’s kappa. Group means are bootstrap means across coders per theme."
)
ft <- flextable::align(ft, align = "left", part = "footer")

doc <- officer::read_docx()
doc <- officer::body_add_par(doc, "Cohen’s kappa by theme vs adjudicated reference standard", style = "heading 1")
doc <- officer::body_add_par(
  doc,
  paste0(
    "Each cell shows κ (", round(CONF*100), "% CI) comparing the coder to the adjudicated reference standard for that theme. ",
    "Human/LLM means are bootstrap means across coders (B=", B, ")."
  ),
  style = "Normal"
)
doc <- officer::body_add_par(doc, "", style = "Normal")
doc <- officer::body_end_section_landscape(doc)
doc <- flextable::body_add_flextable(doc, ft)
doc <- officer::body_add_par(doc, paste0("Generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "Normal")

docx_path <- file.path(out_dir, "kappa_by_theme_vs_referencestandard_bootCIs.docx")
print(doc, target = docx_path)

message("Wrote:\n  - ", csv_path, "\n  - ", docx_path)
