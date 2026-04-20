# CSC 308 Final Project
# Student: Joshua Hall
# Role: Visualization and Data Presentation

library(dplyr)
library(ggplot2)

# Load Dataset
data21 <- read.csv("processed_data/2021/Master_2021_Cleaned.csv")

# --- Visualization 1: Income vs Uninsured ----
ggplot(data21, aes(x = Median.Household.Income, y = Uninsured)) + 
  geom_point(alpha = 0.5, color = "blue") + 
  lab(
    title = "Income vs Uninsured Rate",
    x = "Median Household Income",
    y = "Uninsured Percentage"
  ) + 
  theme_minimal()

# --- Visualization 2: Rural vs Healthcare Access ---
ggplot(data21, aes(x = Rural_Percentage, y = Clinical_Care_Rank)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(
      title = "Rural Population vs Clinical Care",
      x = "Rural Population %"
      y = "Clincal Care Rank"
  ) +
  theme_minimal()
