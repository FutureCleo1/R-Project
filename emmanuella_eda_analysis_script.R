# =========================================================
# CSC 308 Organization of Programming Languages Final Project
# Individual R Script
# Project: Healthcare Access Trends (2021-2023)
# Student: Emmanuella Obidike
# Role: Data Sourcing, EDA Analysis, and Interpretation 
# =========================================================

# --- ORIGINAL CODE BY EMMANUELLA OBIDIKE ---
# Using the cleaned dataset to do some additional EDA and
# better understand patterns in the data.

library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# SECTION 1: Load the cleaned dataset
# ---------------------------------------------------------

# Load the final cleaned dataset from the project
data21 <- read.csv("processed_data/2021/Master_2021_Cleaned.csv")

# Quick check so I know the data loaded correctly
head(data21)

# ---------------------------------------------------------
# SECTION 2: Focus on specific states
# ---------------------------------------------------------

# These are the states we decided to focus on for comparison
states <- c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida")

# Filter the dataset to only include those states
filtered_data <- data21 %>%
  filter(State %in% states)

# ---------------------------------------------------------
# SECTION 3: Simple summary (income + clinical care)
# ---------------------------------------------------------

# Get average income and clinical care rank per state
summary_table <- filtered_data %>%
  group_by(State) %>%
  summarise(
    avg_income = mean(Median.Household.Income, na.rm = TRUE),
    avg_clinical_rank = mean(Clinical_Care_Rank, na.rm = TRUE)
  )

# Print the summary so I can see the results
print("Summary table:")
print(summary_table)

# ---------------------------------------------------------
# SECTION 4: Plot 1 - Income vs Clinical Care
# ---------------------------------------------------------

# Scatter plot to see if income relates to clinical care ranking
ggplot(filtered_data, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
  geom_point(color = "blue", alpha = 0.4) +
  labs(
    title = "Income vs Clinical Care (2021)",
    x = "Median Household Income",
    y = "Clinical Care Rank"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 5: Plot 2 - Income by State
# ---------------------------------------------------------

# Bar chart showing average income across states
ggplot(summary_table, aes(x = State, y = avg_income)) +
  geom_col(fill = "purple") +
  labs(
    title = "Average Income by State (2021)",
    x = "State",
    y = "Average Income"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 6: Correlation check
# ---------------------------------------------------------

# Checking how strongly income and clinical care are related
correlation <- cor(
  filtered_data$Median.Household.Income,
  filtered_data$Clinical_Care_Rank,
  use = "complete.obs"  # ignore missing values
)

print("Correlation between income and clinical care:")
print(correlation)

# ---------------------------------------------------------
# SECTION 7: Compare income across states
# ---------------------------------------------------------

# Another quick way to compare average income by state
income_by_state <- filtered_data %>%
  group_by(State) %>%
  summarise(avg_income = mean(Median.Household.Income, na.rm = TRUE))

print(income_by_state)

# ---------------------------------------------------------
# SECTION 8: Health Factors vs Health Outcomes
# ---------------------------------------------------------

# Scatter plot to see if better environments lead to better health outcomes
ggplot(filtered_data, aes(x = Health_Factors_Rank, y = Health_Outcomes_Rank)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  labs(
    title = "Health Factors vs Health Outcomes",
    x = "Health Factors Rank",
    y = "Health Outcomes Rank"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 9: Correlation (factors vs outcomes)
# ---------------------------------------------------------

# Checking if health factors and outcomes move together
correlation2 <- cor(
  filtered_data$Health_Factors_Rank,
  filtered_data$Health_Outcomes_Rank,
  use = "complete.obs"
)

print("Correlation between factors and outcomes:")
print(correlation2)
