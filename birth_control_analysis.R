# Load required libraries
library(readxl)
library(tidyverse)
library(gtsummary)
library(gt)
library(webshot2)

# First, let's see what sheets are available
sheet_names <- excel_sheets("~/Desktop/BirthControlData.xls")
print("Available sheets:")
print(sheet_names)

# Read both the data and codebook
birth_control_data <- read_excel("~/Desktop/BirthControlData.xls", sheet = "data")
codebook <- read_excel("~/Desktop/BirthControlData.xls", sheet = "codebook")

# Look at the structure of the data
str(birth_control_data)

# Display the first few rows
head(birth_control_data)

# Get column names
names(birth_control_data)

# Basic summary of the data
summary(birth_control_data)

# Create children categories
birth_control_data <- birth_control_data %>%
  mutate(
    children_category = case_when(
      totalchildren == 0 ~ "No child",
      totalchildren <= 2 ~ "1 to 2 children",
      totalchildren >= 3 ~ "3 or more children",
      TRUE ~ NA_character_
    ),
    # Convert binary variables to factors with meaningful labels
    birthc = factor(birthc, levels = c(0, 1), 
                   labels = c("No modern methods", "Uses modern methods")),
    place = factor(place, levels = c(0, 1), 
                  labels = c("Rural", "Urban")),
    everschool = factor(everschool, levels = c(0, 1), 
                       labels = c("Never attended", "Has attended"))
  )

# Convert to factor with specific order
birth_control_data$children_category <- factor(
  birth_control_data$children_category,
  levels = c("No child", "1 to 2 children", "3 or more children")
)

# Get summary statistics of totalchildren
summary(birth_control_data$totalchildren)

# Create a frequency table of totalchildren
table(birth_control_data$totalchildren)

# Create a visualization of the distribution
ggplot(birth_control_data, aes(x = totalchildren)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Children",
       x = "Number of Children",
       y = "Frequency")

# Get quartiles for potential grouping
quantile(birth_control_data$totalchildren, probs = seq(0, 1, 0.25), na.rm = TRUE)

# Look at the distribution of the new categories
table(birth_control_data$children_category)

# Create a bar plot of the categories
ggplot(birth_control_data, aes(x = children_category)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Children Categories",
       x = "Children Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the demographic table
demographic_table <- birth_control_data %>%
  select(age, everschool, place, children_category, birthc) %>%
  tbl_summary(
    by = NULL,
    label = list(
      age ~ "Age, years",
      everschool ~ "Educational Status",
      place ~ "Place of Residence",
      children_category ~ "Number of Children",
      birthc ~ "Current Use of Modern Birth Control Methods"
    ),
    type = list(where(is.factor) ~ "categorical"),
    digits = list(all_categorical() ~ c(0, 1))
  ) %>%
  bold_labels() %>%
  modify_header(label = "**Characteristic**") %>%
  modify_header(stat_0 = "**No. (%)**\nN = 15,198") %>%
  as_gt() %>%
  tab_header(
    title = md("**Table 1.** Demographic Characteristics of Women (N=15,198) Who Participated in the Study, Papua New Guinea, 2018")
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    column_labels.font.weight = "bold",
    table.font.size = 11,
    heading.title.font.size = 11,
    data_row.padding = px(5),
    table.width = pct(100)
  )

# First save as HTML
gt::gtsave(demographic_table, "demographic_table.html")

# Then convert HTML to PNG with higher resolution
webshot2::webshot(
  url = "demographic_table.html",
  file = "demographic_table.png",
  zoom = 2,
  vwidth = 800,
  vheight = 1200  # increased height to accommodate more text
)
