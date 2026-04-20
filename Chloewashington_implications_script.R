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
 # SECTION 1: Load the cleaned dataset (2021)
 # ---------------------------------------------------------
 
 # Use the cleaned dataset created earlier in the project
 data21 <- master_2021
 
 # Quick check that the data loaded correctly
 head(data21)
 
 # ---------------------------------------------------------
 # SECTION 2: Use full dataset (all U.S. counties)
 # ---------------------------------------------------------
 
 # No filtering is applied here so analysis reflects all states
 filtered_data <- data21
 
 # ---------------------------------------------------------
 # SECTION 3: Summary table for discussion
 # ---------------------------------------------------------
 
 summary_table <- filtered_data %>%
   group_by(State) %>%
   summarise(
     avg_income = mean(Median.Household.Income, na.rm = TRUE),
     avg_clinical_rank = mean(Clinical_Care_Rank, na.rm = TRUE)
   )
 
 print("Summary table (average values by state):")
 print(summary_table)
 print("Interpretation: Differences in average income across states highlight regional disparities that may contribute to unequal healthcare access and outcomes.")
 
 # ---------------------------------------------------------
 # SECTION 4: Main plot for discussion (Income vs Clinical Care)
 # ---------------------------------------------------------
 
 ggplot(filtered_data, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
   geom_point(alpha = 0.4) +
   labs(
     title = "Income vs Clinical Care Across U.S. Counties (2021)",
     x = "Median Household Income",
     y = "Clinical Care Rank (Lower = Better)"
   ) +
   theme_minimal()
 
 # ---------------------------------------------------------
 # SECTION 5: Correlation to support discussion
 # ---------------------------------------------------------
 
 correlation <- cor(
   filtered_data$Median.Household.Income,
   filtered_data$Clinical_Care_Rank,
   use = "complete.obs"
 )
 
 print("Correlation between income and clinical care:")
 print(correlation)
 print("Interpretation: The negative correlation suggests that counties with higher income tend to have better clinical care rankings. This indicates that economic resources may influence access to healthcare services.")
 
 # ---------------------------------------------------------
 # SECTION 6: State-level comparison for implications
 # ---------------------------------------------------------
 
 ggplot(summary_table, aes(x = reorder(State, avg_income), y = avg_income)) +
   geom_col() +
   coord_flip() +
   labs(
     title = "Average Income by State (2021)",
     x = "State",
     y = "Average Income"
   ) +
   theme_minimal()
 
 # ---------------------------------------------------------
 # SECTION 7: Health factors and health outcomes relationship
 # ---------------------------------------------------------
 
 correlation2 <- cor(
   filtered_data$Health_Factors_Rank,
   filtered_data$Health_Outcomes_Rank,
   use = "complete.obs"
 )
 
 print("Correlation between health factors and health outcomes:")
 print(correlation2)
 print("Interpretation: The strong relationship between health factors and health outcomes suggests that environmental and behavioral conditions are closely linked to overall population health.")
 