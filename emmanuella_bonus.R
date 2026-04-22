# ==========================================
# EXTRA CREDIT ANALYSIS
# Student: Emmanuella Obidike
# Description:
# This file contains additional analysis beyond required EDA,
# including regression modeling to further examine the
# relationship between socioeconomic variables and health outcomes.
# ==========================================

# ---------------------------------------------------------
# Income Group Impact on Health
# ---------------------------------------------------------

# Create income groups (Low, Medium, High)
master_2021$Income_Group <- cut(
  master_2021$Median.Household.Income,
  breaks = quantile(master_2021$Median.Household.Income, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)

# Compare average health outcomes by income group
income_group_summary <- master_2021 %>%
  group_by(Income_Group) %>%
  summarise(
    Avg_Health_Outcomes = mean(Health_Outcomes_Rank, na.rm = TRUE),
    Avg_Clinical_Care = mean(Clinical_Care_Rank, na.rm = TRUE)
  )

print(income_group_summary)

ggplot(master_2021, aes(x = Income_Group, y = Health_Outcomes_Rank, fill = Income_Group)) +
  geom_boxplot() +
  labs(
    title = "Health Outcomes by Income Group (2021)",
    x = "Income Level",
    y = "Health Outcomes Rank"
  )

library(dplyr)

# Create summary statistics by income group
variability_summary <- master_2021 %>%
  group_by(Income_Group) %>%
  summarise(
    Mean_Health_Outcomes = mean(Health_Outcomes_Rank, na.rm = TRUE),
    SD_Health_Outcomes = sd(Health_Outcomes_Rank, na.rm = TRUE),
    Min_Health_Outcomes = min(Health_Outcomes_Rank, na.rm = TRUE),
    Max_Health_Outcomes = max(Health_Outcomes_Rank, na.rm = TRUE)
  )

print(variability_summary)

# ---------------------------------------------------------
# REGRESSION ANALYSIS (Extra Credit)
# ---------------------------------------------------------

# Build regression model
model <- lm(
  Health_Outcomes_Rank ~ Median.Household.Income + X..Uninsured + X..Rural,
  data = master_2021
)

# Print model summary
print("Regression Model Summary:")
summary(model)


# ---------------------------------------------------------
# CORRELATION MATRIX (Extra Insight)
# ---------------------------------------------------------

# Select key variables
corr_data <- master_2021 %>%
  select(
    Median.Household.Income,
    Clinical_Care_Rank,
    Health_Outcomes_Rank,
    X..Uninsured,
    X..Rural
  )

# Compute correlation matrix
cor_matrix <- cor(corr_data, use = "complete.obs")

print("Correlation Matrix:")
print(cor_matrix)


# ---------------------------------------------------------
# ADDITIONAL VISUALIZATION (Income vs Uninsured)
# ---------------------------------------------------------

ggplot(master_2021, aes(x = Median.Household.Income, y = X..Uninsured)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Income vs Uninsured Rate (2021)",
    x = "Median Household Income",
    y = "Uninsured Percentage"
  ) +
  theme_minimal()
