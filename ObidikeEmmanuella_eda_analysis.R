# =========================================================
# CSC 308 Organization of Programming Languages Final Project
# Individual R Script
# Student: Emmanuella Obidike
# Role: DATA SOURCING, EDA ANALYSIS, AND INTERPRETATION 
# =========================================================

# --- ORIGINAL CODE BY EMMANUELLA OBIDIKE ---
# Using the cleaned dataset to do some additional EDA and
# better understand patterns in the data.

library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# SECTION 1: Load the cleaned dataset
# ---------------------------------------------------------

master_2021 <- read.csv("Master_2021_Cleaned.csv")

# Assign to my working dataset

data21 <- master_2021

# Quick check to confirm data loaded correctly
head(data21)

# ---------------------------------------------------------
# SECTION 2: Summary of key variables (nationwide)
# ---------------------------------------------------------

# Get average income and clinical care rank across all counties
summary_table <- data21 %>%
  summarise(
    avg_income = mean(Median.Household.Income, na.rm = TRUE),
    avg_clinical_rank = mean(Clinical_Care_Rank, na.rm = TRUE)
  )

print("Overall summary (U.S. counties):")
print(summary_table)

# ---------------------------------------------------------
# SECTION 3: Plot 1 - Income vs Clinical Care (Nationwide)
# ---------------------------------------------------------

ggplot(data21, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
  geom_point(color = "blue", alpha = 0.3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Income vs Clinical Care Across U.S. Counties (2021)",
    x = "Median Household Income",
    y = "Clinical Care Rank (Lower = Better)"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 4: Correlation (Income vs Clinical Care)
# ---------------------------------------------------------

correlation <- cor(
  data21$Median.Household.Income,
  data21$Clinical_Care_Rank,
  use = "complete.obs"
)

print("Correlation between income and clinical care:")
print(correlation)

# ---------------------------------------------------------
# SECTION 5: Health Factors vs Health Outcomes (Nationwide)
# ---------------------------------------------------------

ggplot(data21, aes(x = Health_Factors_Rank, y = Health_Outcomes_Rank)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  labs(
    title = "Health Factors vs Health Outcomes Across U.S. Counties (2021)",
    x = "Health Factors Rank (Lower = Better)",
    y = "Health Outcomes Rank (Lower = Better)"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 6: Correlation (Factors vs Outcomes)
# ---------------------------------------------------------

correlation2 <- cor(
  data21$Health_Factors_Rank,
  data21$Health_Outcomes_Rank,
  use = "complete.obs"
)

print("Correlation between health factors and health outcomes:")
print(correlation2)

# ---------------------------------------------------------
# SECTION 7: Income Distribution Overview
# ---------------------------------------------------------

ggplot(data21, aes(x = Median.Household.Income)) +
  geom_histogram(fill = "purple", bins = 30, alpha = 0.7) +
  labs(
    title = "Distribution of Median Household Income (U.S. Counties, 2021)",
    x = "Median Household Income",
    y = "Count"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 8: Racial Disparities (YPLL)
# ---------------------------------------------------------

library(tidyr)

racial_data <- data21 %>%
  select(YPLL_Black, YPLL_White) %>%
  pivot_longer(cols = everything(), names_to = "Group", values_to = "YPLL")

ggplot(racial_data, aes(x = Group, y = YPLL, fill = Group)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Racial Disparities in Premature Mortality (YPLL)",
    x = "Population Group",
    y = "YPLL Rate"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 9: Income vs Health Outcomes
# ---------------------------------------------------------

ggplot(data21, aes(x = Median.Household.Income, y = Health_Outcomes_Rank)) +
  geom_point(alpha = 0.3, color = "darkred") +
  labs(
    title = "Income vs Health Outcomes Across U.S. Counties (2021)",
    x = "Median Household Income",
    y = "Health Outcomes Rank"
  ) +
  theme_minimal()


# ---------------------------------------------------------
# SECTION 10: Clinical Care Distribution
# ---------------------------------------------------------

ggplot(data21, aes(x = Clinical_Care_Rank)) +
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
  labs(
    title = "Distribution of Clinical Care Rankings",
    x = "Clinical Care Rank",
    y = "Count"
  ) +
  theme_minimal()
