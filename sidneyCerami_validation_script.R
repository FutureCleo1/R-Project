# -------------------------------
# CSC 308: Organization of Programming Languages Final Project
# My individual script
# SIDNEY CERAMI 
# Role: VALIDATION, DATA QUALITY, AND ETHICS ANALYSIS
# -------------------------------

# This section evaluates the reliability, quality, and ethical considerations 
# of the dataset used in this project.

library(dplyr)

# ---------------------------------------------------------
# SECTION 0: Load Raw Dataset (Independent)
# ---------------------------------------------------------

# Load raw file directly 
master_2021 <- read.csv("processed_data/2021/Master_2021_Cleaned.csv")


print("Dataset loaded successfully:")
print(dim(master_2021))

# ---------------------------------------------------------
# SECTION 1: Data Quality Check (Missing Values)
# ---------------------------------------------------------

print("Checking for missing values in dataset:")
missing_values <- colSums(is.na(master_2021))
print(missing_values)

print("Total missing values in dataset:")
print(sum(is.na(master_2021)))

# ---------------------------------------------------------
# SECTION 2: Check for Duplicates
# ---------------------------------------------------------

n_duplicates <- sum(duplicated(master_2021))
print("Number of duplicate rows:")
print(n_duplicates)

# ---------------------------------------------------------
# SECTION 3: Check for Inconsistencies
# ---------------------------------------------------------

invalid_income <- master_2021 %>%
  filter(Median.Household.Income < 0)

print("Invalid income values (should be 0 rows):")
print(nrow(invalid_income))

summary(master_2021$Median.Household.Income)

# ---------------------------------------------------------
# SECTION 4: Data Distribution Check
# ---------------------------------------------------------

print("Summary of key variables:")
summary(master_2021 %>%
          select(
            Median.Household.Income,
            Life.Expectancy
          ))

# ---------------------------------------------------------
# SECTION 5: Source Reliability & Credibility
# ---------------------------------------------------------

print("Data Source Evaluation:")
print("The dataset is derived from County Health Rankings data,")
print("which compiles information from credible sources such as")
print("the CDC and U.S. Census Bureau.")

# ---------------------------------------------------------
# SECTION 6: Limitations of the Dataset
# ---------------------------------------------------------

print("Dataset Limitations:")
print("- Data is limited to one year (2021).")
print("- Missing values may reduce representation of some counties.")
print("- Only selected variables are analyzed, not all health factors.")

# ---------------------------------------------------------
# SECTION 7: Potential Sources of Bias
# ---------------------------------------------------------

print("Potential Bias Analysis:")
print("- Rural areas may be underrepresented.")
print("- Data reporting varies across regions.")
print("- Socioeconomic inequalities may influence results.")

# ---------------------------------------------------------
# SECTION 8: Impact on Results and Interpretation
# ---------------------------------------------------------

print("Impact on Analysis:")
print("Results may not fully capture all populations.")
print("Relationships observed do not imply causation.")
print("Findings should be interpreted with caution.")

# ---------------------------------------------------------
# SECTION 9: Ethical Considerations
# ---------------------------------------------------------

print("Ethical Considerations:")
print("- Avoid misrepresentation of demographic groups.")
print("- Do not generalize findings to individuals.")
print("- Maintain transparency about data limitations.")

