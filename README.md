# Birth Control Analysis in Papua New Guinea (2018)

## Overview
This repository contains R scripts for analyzing modern birth control method usage among women in Papua New Guinea. The analysis includes demographic characteristics, prevalence calculations, and odds ratio analyses.

## Software and Dependencies
- **R version**: 4.x
- **Key R Packages**:
  - `tidyverse`: Data manipulation and visualization
  - `gtsummary`: Creating publication-ready analytical tables
  - `gt`: Table formatting and styling
  - `webshot2`: Converting HTML tables to PNG format
  - `readxl`: Reading Excel data files
  - `broom`: Tidying statistical outputs

## Data Description
- **Source**: Birth control usage data from Papua New Guinea (2018)
- **Sample Size**: N = 15,198 women
- **Key Variables**:
  - Place of residence (binary: rural/urban)
  - Age (categorized: 15-24, 25-34, 35-44, 45+ years)
  - Educational status (binary: never attended/has attended)
  - Number of children (categorized: no children, 1-2 children, 3+ children)
  - Modern birth control method use (binary: yes/no)

## Analysis Methods
1. **Prevalence Analysis**
   - Current use of modern birth control methods
   - Stratified by demographic characteristics
   - Presented with 95% confidence intervals

2. **Association Analysis**
   - Primary exposure: Place of residence (urban vs rural)
   - Crude odds ratios calculated for all variables
   - Adjusted odds ratios calculated controlling for potential confounders:
     - Age
     - Educational status
     - Number of children
   - All analyses include 95% confidence intervals and p-values

## Table Formatting
- Tables formatted according to JAMA (Journal of the American Medical Association) style guidelines
- Using `gt` package for professional table presentation
- Tables include:
  - Clear titles and labels
  - Appropriate footnotes explaining statistical methods
  - Reference categories marked with "-"
  - Standard formatting for p-values and confidence intervals

## Output Files
- HTML and PNG formats for all tables
- High-resolution images suitable for publication
- Standardized naming convention for easy reference

## Scripts
1. `prevalence_and_odds_ratios.R`: Prevalence calculations with confidence intervals
2. `place_odds_ratios.R`: Crude and adjusted odds ratio analyses

## Notes
- Statistical significance set at p < 0.05
- Categorical variables presented as counts and percentages with 95% CIs
- Odds ratios presented with 95% confidence intervals