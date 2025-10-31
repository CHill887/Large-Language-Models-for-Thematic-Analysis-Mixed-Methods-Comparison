
suppressPackageStartupMessages({
  library(tidyverse)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default=NULL) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit == length(args)) return(default)
  args[hit + 1]
}
data_dir <- get_arg("--data_dir", "./data")
out_dir  <- get_arg("--out_dir",  "./out")
human_pat <- get_arg("--human", "human")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

norm_names <- function(x) {
  x %>% tolower() %>% gsub("[^a-z0-9]+","", .)
}

is_seg_col <- function(nm) {
  nm2 <- tolower(nm)
  nm %in% c("SegmentID","segment_id","segment id","id") || grepl("segment", nm2)
}

looks_like_label_row <- function(df, code_cols) {
  if (nrow(df) == 0 || length(code_cols) == 0) return(FALSE)
  first <- df[1, code_cols, drop = FALSE]
  ratio_alpha <- mean(sapply(first, function(x) grepl("[A-Za-z]", as.character(x))))
  isTRUE(ratio_alpha > 0.5)
}

validate_binary01 <- function(x, colname, file_path) {
  vals <- unique(na.omit(as.character(x)))
  vals <- trimws(vals)
  vals <- vals[vals != ""]
  if (length(vals) == 0) return(invisible(TRUE))
  nums <- suppressWarnings(as.numeric(vals))
  bad_idx <- is.na(nums) | !(nums %in% c(0, 1))
  if (any(bad_idx)) {
    bad_vals <- unique(vals[bad_idx])
    stop(
      sprintf(
        "Non-binary values found in column '%s' of file '%s': %s\nOnly 0/1 (or blank/NA) are allowed.",
        colname, file_path, paste(bad_vals, collapse = ", ")
      )
    )
  }
  invisible(TRUE)
}

to_binary01 <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  num[is.na(num)] <- 0
  as.integer(num != 0)
}

load_sheet <- function(path) {
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  if (nrow(df) == 0) stop(sprintf("File '%s' appears to be empty.", path))
  seg_candidates <- which(sapply(names(df), is_seg_col))
  seg_col <- names(df)[ifelse(length(seg_candidates) >= 1, seg_candidates[1], 1)]
  exclude <- c(seg_col, names(df)[grepl("speaker", tolower(names(df)))])
  code_cols <- setdiff(names(df), exclude)
  if (length(code_cols) == 0) {
    stop(sprintf("No code columns detected in '%s' (after excluding '%s' and speaker columns).",
                 path, seg_col))
  }
  if (looks_like_label_row(df, code_cols)) {
    df <- df[-1, , drop = FALSE]
  }
  for (c in code_cols) {
    validate_binary01(df[[c]], c, path)
  }
  df[[seg_col]] <- as.character(df[[seg_col]])
  for (c in code_cols) {
    df[[c]] <- to_binary01(df[[c]])
  }
  tibble::as_tibble(df) |>
    transmute(
      SegmentID = .data[[seg_col]],
      !!!rlang::syms(code_cols)
    )
}

load_align <- function(paths, human_pattern = "human") {
  sheets <- purrr::map(paths, load_sheet)
  coder_labels <- basename(paths)
  code_sets <- purrr::map(sheets, ~ setdiff(names(.x), "SegmentID"))
  norm_sets <- purrr::map(code_sets, norm_names)
  common_norm <- Reduce(intersect, norm_sets)
  if (length(common_norm) == 0) stop("No common codes found across files.")
  pick_cols <- function(df) {
    nm <- names(df); nm_norm <- norm_names(nm)
    keep <- nm[nm_norm %in% common_norm & nm != "SegmentID"]
    c("SegmentID", keep)
  }
  sheets2 <- purrr::map(sheets, ~ .x[, pick_cols(.x), drop = FALSE])
  template_codes <- setdiff(names(sheets2[[1]]), "SegmentID")
  reord <- function(df) {
    codes <- setdiff(names(df), "SegmentID")
    m <- match(norm_names(template_codes), norm_names(codes))
    keep_codes <- codes[m[!is.na(m)]]
    df |> dplyr::select(SegmentID, dplyr::all_of(keep_codes))
  }
  sheets3 <- purrr::map(sheets2, reord)
  all_ids <- sort(unique(unlist(purrr::map(sheets3, ~ .x$SegmentID))))
  sheets_f <- purrr::imap(sheets3, function(df, i) {
    coder <- coder_labels[i]
    df |>
      dplyr::right_join(tibble::tibble(SegmentID = all_ids), by = "SegmentID") |>
      dplyr::mutate(dplyr::across(-SegmentID, ~ tidyr::replace_na(., 0L))) |>
      dplyr::mutate(dplyr::across(-SegmentID, as.integer)) |>
      dplyr::mutate(.coder = coder)
  })
  bind <- purrr::map_dfr(sheets_f, function(df) {
    coder <- unique(df$.coder)[1]
    df |>
      dplyr::select(-.coder) |>
      tidyr::pivot_longer(-SegmentID, names_to = "Code", values_to = "Value") |>
      dplyr::mutate(Coder = coder) |>
      dplyr::relocate(Coder, .before = Code)
  })
  human_idx <- which(grepl(human_pattern, tolower(unique(bind$Coder))))
  human_label <- unique(bind$Coder)[ifelse(length(human_idx) >= 1, human_idx[1], 1)]
  bad_vals <- bind %>% filter(!is.na(Value) & !(Value %in% c(0L, 1L)))
  if (nrow(bad_vals) > 0) {
    example <- bad_vals %>% head(10)
    stop(
      "Non-binary values found after processing (expected only 0/1). ",
      "Check input files. Showing first few offending rows:\n",
      paste(capture.output(print(example)), collapse = "\n")
    )
  }
  list(
    long   = bind,
    codes  = unique(bind$Code),
    coders = unique(bind$Coder),
    human  = human_label
  )
}

paths <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(paths) < 2) {
  stop("No CSV files found in --data_dir (need at least 2).")
}
aligned <- load_align(paths, human_pattern = human_pat)

readr::write_csv(aligned$long,  file.path(out_dir, "aligned_long.csv"))
readr::write_lines(aligned$codes,  file.path(out_dir, "codes.txt"))
readr::write_lines(aligned$coders, file.path(out_dir, "coders.txt"))
readr::write_lines(aligned$human,  file.path(out_dir, "human_label.txt"))
message("Saved: aligned_long.csv, codes.txt, coders.txt, human_label.txt")
