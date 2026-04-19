# =========================================================
# CSC 308 Organization of Programming Languages Final Project
# Individual R Script
# Project: Healthcare Access Trends (2021-2023)
# Student:Chloe Washington
# Role: Implications of Results, Analytical Discussion, and Interpretation
# =========================================================

# This script supports my role in discussing what the project results mean
# and why those findings matter in a real-world healthcare context.

library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# SECTION 1: Load the cleaned dataset
# ---------------------------------------------------------

data21 <- read.csv("processed_data/2021/Master_2021_Cleaned.csv")

# Quick check that the file loaded correctly
head(data21)

# ---------------------------------------------------------
# SECTION 2: Focus on the selected states
# ---------------------------------------------------------

states <- c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida")

filtered_data <- data21 %>%
  filter(State %in% states)

# ---------------------------------------------------------
# SECTION 3: Summary table for discussion
# ---------------------------------------------------------

summary_table <- filtered_data %>%
  group_by(State) %>%
  summarise(
    avg_income = mean(Median.Household.Income, na.rm = TRUE),
    avg_clinical_rank = mean(Clinical_Care_Rank, na.rm = TRUE)
  )

print("Summary table:")
print(summary_table)

# ---------------------------------------------------------
# SECTION 4: One main plot for discussion
# ---------------------------------------------------------

ggplot(filtered_data, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Income vs Clinical Care (2021)",
    x = "Median Household Income",
    y = "Clinical Care Rank"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 5: One correlation to support discussion
# ---------------------------------------------------------

correlation <- cor(
  filtered_data$Median.Household.Income,
  filtered_data$Clinical_Care_Rank,
  use = "complete.obs"
)

print("Correlation between income and clinical care:")
print(correlation)

# ---------------------------------------------------------
# SECTION 6: State comparison for implications
# ---------------------------------------------------------

ggplot(summary_table, aes(x = State, y = avg_income)) +
  geom_col() +
  labs(
    title = "Average Income by State (2021)",
    x = "State",
    y = "Average Income"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 7: Health factors and health outcomes
# ---------------------------------------------------------

correlation2 <- cor(
  filtered_data$Health_Factors_Rank,
  filtered_data$Health_Outcomes_Rank,
  use = "complete.obs"
)

print("Correlation between health factors and health outcomes:")
print(correlation2)