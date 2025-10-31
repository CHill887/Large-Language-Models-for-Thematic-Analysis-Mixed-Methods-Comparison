#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(flextable)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit == length(args)) return(default)
  args[hit + 1]
}
out_dir <- get_arg("--out_dir", "out")
out_file <- get_arg("--out_file", "model_coding_summary_with_error_rates.docx")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

raw <- tibble::tribble(
  ~Metric, ~ChatGPT, ~QualiGPT, ~Claude,
  "Exact match", 31, 45, 71,
  "Partial Match", 2, 3, 0,
  "No Match", 0, 2, 0,
  "Researcher segments coded", 1, 5, 8,
  "Total Segments Coded", 34, 55, 79
)

den <- raw |>
  dplyr::filter(Metric == "Total Segments Coded") |>
  tidyr::pivot_longer(-Metric, names_to = "Model", values_to = "Total") |>
  dplyr::select(-Metric)

long <- raw |>
  tidyr::pivot_longer(-Metric, names_to = "Model", values_to = "Count") |>
  dplyr::left_join(den, by = "Model") |>
  dplyr::mutate(Pct = ifelse(Metric == "Total Segments Coded", 1, Count / Total))

main_df <- long |>
  dplyr::mutate(Value = ifelse(Metric == "Total Segments Coded",
                               sprintf("%d (100%%)", Count),
                               sprintf("%d (%.1f%%)", Count, 100 * Pct))) |>
  dplyr::select(Metric, Model, Value) |>
  tidyr::pivot_wider(names_from = Model, values_from = Value) |>
  dplyr::left_join(
    long |>
      dplyr::group_by(Metric) |>
      dplyr::summarise(Mean = mean(Count), SD = sd(Count), .groups = "drop") |>
      dplyr::mutate(`Mean (SD)` = sprintf("%.1f (%.1f)", Mean, SD)) |>
      dplyr::select(Metric, `Mean (SD)`),
    by = "Metric"
  ) |>
  dplyr::mutate(Metric = factor(Metric, levels = c("Exact match","Partial Match","No Match",
                                                   "Researcher segments coded","Total Segments Coded"))) |>
  dplyr::arrange(Metric)

get_row <- function(name) raw |> dplyr::filter(Metric == name) |> dplyr::select(-Metric) |> as.list()
tot <- get_row("Total Segments Coded")
no <- get_row("No Match")
part <- get_row("Partial Match")
res <- get_row("Researcher segments coded")

rate_row <- function(label, nlist, dlist){
  nC <- nlist$ChatGPT; nQ <- nlist$QualiGPT; nL <- nlist$Claude
  dC <- dlist$ChatGPT; dQ <- dlist$QualiGPT; dL <- dlist$Claude
  p <- c(nC/dC, nQ/dQ, nL/dL)
  msd <- sprintf("%.1f%% (%.1f%%)", 100 * mean(p), 100 * sd(p))
  tibble::tibble(
    Metric = label,
    ChatGPT = sprintf("%d/%d (%.1f%%)", nC, dC, 100 * p[1]),
    QualiGPT = sprintf("%d/%d (%.1f%%)", nQ, dQ, 100 * p[2]),
    Claude = sprintf("%d/%d (%.1f%%)", nL, dL, 100 * p[3]),
    `Mean (SD)` = msd
  )
}

err_df <- dplyr::bind_rows(
  rate_row("Strict Hallucination Rate (No Match / Total)", no, tot),
  rate_row(
    "Expanded Hallucination Rate (incl. policy violations)",
    list(ChatGPT = no$ChatGPT + res$ChatGPT,
         QualiGPT = no$QualiGPT + res$QualiGPT,
         Claude = no$Claude + res$Claude),
    tot
  ),
  rate_row(
    "Comprehensive Error Rate (incl. partial matches)",
    list(ChatGPT = no$ChatGPT + res$ChatGPT + part$ChatGPT,
         QualiGPT = no$QualiGPT + res$QualiGPT + part$QualiGPT,
         Claude = no$Claude + res$Claude + part$Claude),
    tot
  )
)

tbl <- dplyr::bind_rows(
  main_df,
  tibble::tibble(Metric = "--- Overall Error Rates ---", ChatGPT = "", QualiGPT = "", Claude = "", `Mean (SD)` = ""),
  err_df
)

ft <- flextable::flextable(tbl) |>
  flextable::add_header_lines("Model Coding Summary (per-model: n and %; fourth column: mean(SD))") |>
  flextable::theme_vanilla() |>
  flextable::bold(part = "header") |>
  flextable::align(j = 1, align = "left", part = "all") |>
  flextable::align(j = 2:5, align = "center", part = "all") |>
  flextable::bold(i = ~ Metric == "Total Segments Coded", bold = TRUE) |>
  flextable::bold(i = ~ Metric == "--- Overall Error Rates ---", bold = TRUE) |>
  flextable::autofit() |>
  flextable::add_footer_lines(c(
    "Percentages in the main block are column-wise (denominator = each model's Total Segments Coded).",
    "Strict = No Match/Total. Expanded = (No Match + Researcher-segments-coded)/Total. Comprehensive = (No Match + Researcher-segments-coded + Partial Match)/Total.",
    "Fourth column: Mean(SD) of counts (main block) and of percentages (error-rate block)."
  )) |>
  flextable::align(part = "footer", align = "left") |>
  flextable::fontsize(part = "footer", size = 8)

flextable::save_as_docx("Model Coding Summary" = ft, path = file.path(out_dir, out_file))
