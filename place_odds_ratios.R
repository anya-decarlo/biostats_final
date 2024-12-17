# Analysis of Odds Ratios for Modern Birth Control Method Use
library(readxl)
library(tidyverse)
library(gtsummary)
library(gt)
library(webshot2)
library(broom)

# Read data from the second sheet
birth_control_data <- read_excel("~/Desktop/BirthControlData.xls", sheet = 2)

# Prepare the data
analysis_data <- birth_control_data %>%
  mutate(
    birthc = factor(birthc, levels = c(0, 1), labels = c("No", "Yes")),
    place = factor(place, levels = c(0, 1), labels = c("Rural", "Urban")),
    everschool = factor(everschool, levels = c(0, 1), 
                       labels = c("Never attended", "Has attended")),
    age_cat = cut(age, 
                 breaks = c(14, 24, 34, 44, Inf),
                 labels = c("15-24", "25-34", "35-44", "45+"),
                 right = FALSE),
    children_cat = case_when(
      totalchildren == 0 ~ "No children",
      totalchildren <= 2 ~ "1-2 children",
      totalchildren >= 3 ~ "3 or more children"
    ) %>% factor(levels = c("No children", "1-2 children", "3 or more children"))
  )

# Function to format odds ratio and CI
format_or_ci <- function(estimate, conf.low, conf.high) {
  sprintf("%0.2f (%0.2f-%0.2f)", estimate, conf.low, conf.high)
}

# Function to format p-value according to JAMA style
format_p_value <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<.001")
  sprintf(".%03d", round(p * 1000))
}

# Calculate crude odds ratios
crude_models <- list(
  age_cat = glm(birthc ~ age_cat, data = analysis_data, family = binomial),
  everschool = glm(birthc ~ everschool, data = analysis_data, family = binomial),
  place = glm(birthc ~ place, data = analysis_data, family = binomial),
  children_cat = glm(birthc ~ children_cat, data = analysis_data, family = binomial)
)

# Calculate adjusted odds ratios
adjusted_model <- glm(birthc ~ place + age_cat + everschool + children_cat,
                     data = analysis_data, family = binomial)

# Function to extract odds ratios and CIs
get_or_results <- function(model, var_name) {
  tidy_results <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  
  # Filter results for the specific variable
  if(var_name == "place") {
    results <- tidy_results[grep("^place", tidy_results$term), ]
  } else if(var_name == "age_cat") {
    results <- tidy_results[grep("^age_cat", tidy_results$term), ]
  } else if(var_name == "everschool") {
    results <- tidy_results[grep("^everschool", tidy_results$term), ]
  } else if(var_name == "children_cat") {
    results <- tidy_results[grep("^children_cat", tidy_results$term), ]
  }
  
  results %>%
    mutate(
      or_ci = format_or_ci(estimate, conf.low, conf.high),
      p_value = sapply(p.value, format_p_value)
    )
}

# Get results for each variable
variables <- c("place", "age_cat", "everschool", "children_cat")
crude_results <- map(variables, ~get_or_results(crude_models[[.x]], .x))
adjusted_results <- map(variables, ~get_or_results(adjusted_model, .x))

# Create data frame for table
table_rows <- tibble(
  Characteristic = c(
    "Place of residence",
    "Rural",
    "Urban",
    "Age, y",
    "15-24",
    "25-34",
    "35-44",
    "45+",
    "Educational status",
    "Never attended",
    "Has attended",
    "Number of children",
    "No children",
    "1-2 children",
    "3 or more children"
  ),
  `Crude OR (95% CI)` = c(
    "",
    "-",
    crude_results[[1]]$or_ci,
    "",
    "-",
    crude_results[[2]]$or_ci[1:3],
    "",
    "-",
    crude_results[[3]]$or_ci,
    "",
    "-",
    crude_results[[4]]$or_ci[1:2]
  ),
  `P Value` = c(
    "",
    "",
    crude_results[[1]]$p_value,
    "",
    "",
    crude_results[[2]]$p_value[1:3],
    "",
    "",
    crude_results[[3]]$p_value,
    "",
    "",
    crude_results[[4]]$p_value[1:2]
  ),
  `Adjusted OR (95% CI)` = c(
    "",
    "-",
    adjusted_results[[1]]$or_ci,
    "",
    "-",
    adjusted_results[[2]]$or_ci[1:3],
    "",
    "-",
    adjusted_results[[3]]$or_ci,
    "",
    "-",
    adjusted_results[[4]]$or_ci[1:2]
  ),
  `Adjusted P Value` = c(
    "",
    "",
    adjusted_results[[1]]$p_value,
    "",
    "",
    adjusted_results[[2]]$p_value[1:3],
    "",
    "",
    adjusted_results[[3]]$p_value,
    "",
    "",
    adjusted_results[[4]]$p_value[1:2]
  )
)

# Create and format table
table_3 <- table_rows %>%
  gt() %>%
  tab_header(
    title = md("**Table 3.** Crude and Adjusted Odds Ratios for Current Use of Modern Birth Control Methods Among Women in Papua New Guinea, 2018")
  ) %>%
  cols_align(
    align = "left",
    columns = Characteristic
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Crude OR (95% CI)`, `P Value`, `Adjusted OR (95% CI)`, `Adjusted P Value`)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Characteristic,
      rows = Characteristic %in% c("Age, y", "Educational status", "Place of residence", "Number of children")
    )
  ) %>%
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = Characteristic,
      rows = !Characteristic %in% c("Age, y", "Educational status", "Place of residence", "Number of children")
    )
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = ""
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    column_labels.font.weight = "bold",
    table.font.size = 12,
    data_row.padding = px(5),
    table.width = pct(100)
  ) %>%
  tab_footnote(
    footnote = "Adjusted odds ratios control for age, educational status, and number of children.",
    locations = cells_column_labels(columns = `Adjusted OR (95% CI)`)
  )

# Save the table
gt::gtsave(table_3, "table3_odds_ratios.html")

# Convert to PNG
webshot2::webshot(
  url = "table3_odds_ratios.html",
  file = "table3_odds_ratios.png",
  zoom = 2,
  vwidth = 800,
  vheight = 1000)
