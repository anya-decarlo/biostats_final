# Analysis of Prevalence of Modern Birth Control Method Use
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

# Function to calculate CI for proportion and format result
calc_prop_ci <- function(x, n) {
  if(n == 0) return(c(NA, NA, NA))
  test <- prop.test(x, n)
  c(
    prop = x/n * 100,
    lci = test$conf.int[1] * 100,
    uci = test$conf.int[2] * 100
  )
}

# Format functions according to JAMA style
format_n_percent <- function(n, prop) {
  sprintf("%d (%0.1f)", n, prop)
}

format_ci <- function(lci, uci) {
  sprintf("%0.1f-%0.1f", lci, uci)
}

# Calculate chi-square p-value
calc_p_value <- function(var, data) {
  chisq <- chisq.test(table(data[[var]], data$birthc))
  format_p_value(chisq$p.value)
}

# Format p-value according to JAMA style
format_p_value <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<.001")
  sprintf(".%03d", round(p * 1000))
}

# Calculate statistics for each variable
variables <- c("age_cat", "everschool", "place", "children_cat")
results <- list()

for(var in variables) {
  # Get the counts and calculate p-value
  counts <- table(analysis_data[[var]], analysis_data$birthc)
  p_value <- calc_p_value(var, analysis_data)
  
  # Calculate proportions and CIs
  props <- apply(counts, 1, function(row) {
    calc_prop_ci(row[2], sum(row))
  })
  
  # Format the results
  results[[var]] <- data.frame(
    category = rownames(counts),
    n = counts[,2],
    total = rowSums(counts),
    prop = props[1,],
    lci = props[2,],
    uci = props[3,]
  ) %>%
    mutate(
      n_percent = format_n_percent(n, prop),
      ci = format_ci(lci, uci),
      p_value = p_value
    )
}

# Create the table
table_2 <- tibble(
  Characteristic = c(
    "Age, y",
    results[["age_cat"]]$category,
    "Educational status",
    results[["everschool"]]$category,
    "Place of residence",
    results[["place"]]$category,
    "Number of children",
    results[["children_cat"]]$category
  ),
  `No. (%)` = c(
    "",
    results[["age_cat"]]$n_percent,
    "",
    results[["everschool"]]$n_percent,
    "",
    results[["place"]]$n_percent,
    "",
    results[["children_cat"]]$n_percent
  ),
  `95% CI` = c(
    "",
    results[["age_cat"]]$ci,
    "",
    results[["everschool"]]$ci,
    "",
    results[["place"]]$ci,
    "",
    results[["children_cat"]]$ci
  ),
  `P Value` = c(
    results[["age_cat"]]$p_value[1],
    rep("", length(results[["age_cat"]]$category)),
    results[["everschool"]]$p_value[1],
    rep("", length(results[["everschool"]]$category)),
    results[["place"]]$p_value[1],
    rep("", length(results[["place"]]$category)),
    results[["children_cat"]]$p_value[1],
    rep("", length(results[["children_cat"]]$category))
  )
) %>%
  gt() %>%
  tab_header(
    title = md("**Table 2.** Prevalence of Current Use of Modern Birth Control Methods Among Women in Papua New Guinea, 2018")
  ) %>%
  cols_align(
    align = "left",
    columns = Characteristic
  ) %>%
  cols_align(
    align = "center",
    columns = c(`No. (%)`, `95% CI`, `P Value`)
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
  )

# Save the table
gt::gtsave(table_2, "prevalence_table2.html")

# Convert to PNG
webshot2::webshot(
  url = "prevalence_table2.html",
  file = "prevalence_table2.png",
  zoom = 2,
  vwidth = 800,
  vheight = 1000)
