# Large Language Models for Thematic Analysis: A Mixed-Methods Comparison

This repository contains R scripts used for "Large Language Models for Thematic Analysis in Healthcare Research: A Blinded Mixed-Methods Comparison with Human Analysts" 
All scripts are written in base R (≥ 4.3) and rely on the tidyverse framework for data manipulation and visualization.

## Files

| Script                            | Description                                                                                                            |
| --------------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `dataprocessing.R`                | Loads, validates, and aligns coder data from CSV files.                                                                |
| `Table 1 - comparative metrics.R` | Computes inter-rater reliability metrics (Cohen’s κ, Gwet’s AC1, F1, sensitivity, specificity) and summary statistics. |
| `Table 2 - Cohen's k by theme.R`  | Calculates κ for each theme and exports formatted table.                                                              |
| `Figure A.R`                      | Generates forest plots of ΔAC1                                        |
| `Figure B.R`                      | Produces forest plots for inductive (Likert-scale) data.                                                               |
| `Table 4 - Quote verification.R`  | Summarizes coding agreement and quote-level verification outcomes.                                                     |

## Requirements

R version 4.3 or higher, with the following packages:
`tidyverse`, `irr`, `ggplot2`, `officer`, `flextable`, `scales`, `grid`



## Citation

Hill, C. et al (2025). Large Language Models for Thematic Analysis in Healthcare Research: A Blinded Mixed-Methods Comparison with Human Analysts 
GitHub: [https://github.com/CHill887/Large-Language-Models-for-Thematic-Analysis-Mixed-Methods-Comparison](https://github.com/CHill887/Large-Language-Models-for-Thematic-Analysis-Mixed-Methods-Comparison)

## License

Released under the MIT License.
