# =========================================================
# CSC 308: Organization of Programming Languages Final Project
# Project: Healthcare Access Trends (2021)
# Students: Madison Boyd, Emmanuella Obidike, Chloe Washington
# =========================================================

# ---------------------------------------------------------
# FIX 1: All library() and install.packages() calls moved
# to the top. Install once, then load. Remove install calls
# from mid-script to avoid re-installing on every run.
# ---------------------------------------------------------
# install.packages(c("dplyr", "tidyverse", "ggplot2", "hexbin", "tidyr"))
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
=
# =========================================================
# SECTION: 2021 DATA PREPARATION AND ANALYSIS
# (Madison Boyd - Data Cleaning & Preprocessing)
# =========================================================

# ---------------------------------------------------------
# STEP 1: PROCESSING AND CLEANING 2021 DATA
# ---------------------------------------------------------

# --- FILE 1: Additional Measure Data ---
# Skipped the first placeholder row; treat "N/A" as missing
data21_measures <- read.csv(
  "AdditionalMeasureData.csv",
  skip       = 1,
  na.strings = c("N/A", "", " ")
)

# Narrow down to variables for Research Questions
clean21_measures <- data21_measures %>%
  select(
    FIPS,
    State,
    County,
    Median.Household.Income,
    X..Uninsured,
    X..Rural,
    Life.Expectancy
  ) %>%
  filter(County != "") %>%
  na.omit()  # PREPROCESSING: Remove rows that were originally N/A

# --- VERIFICATION STEP: Data Quality Check ---
print("Checking for missing values in 2021 Measures:")
print(sum(is.na(clean21_measures)))   # Should output 0
print("Row count after cleaning (verify it decreased):")
print(nrow(clean21_measures))

# FIX 2: View() removed from all locations. It is interactive-only and
# will error in non-interactive sessions (e.g. Rscript, knitr).
# Use head() for verification instead.
head(clean21_measures)

# Add Year label for future comparison
clean21_measures$Year <- 2021


# --- FILE 2: Additional Measures Sources and Years ---
# FIX 3: Removed redundant skip = 0 (it is the default).
data21_metadata <- read.csv(
  "raw_data/2021/Addtl Measure Sources and Years.csv",
  na.strings = c("N/A", "", " ")
)

clean21_metadata <- data21_metadata %>%
  dplyr::select(
    Category    = Focus.Area,
    Measure     = Measure,
    Description = Description,
    Source      = Source,
    Years       = Year.s.
  ) %>%
  filter(!is.na(Measure) & Measure != "")

# Verification
print("2021 Data Dictionary (Metadata) Successfully Cleaned:")
head(clean21_metadata)


# --- FILE 3: Outcomes and Factors Rankings ---
data21_outcomesandfactorsrankings <- read.csv(
  "raw_data/2021/Outcomes and Factors Rankings.csv",
  skip       = 1,
  na.strings = c("N/A", "", " ")
)

clean21_ranked <- data21_outcomesandfactorsrankings %>%
  dplyr::select(
    FIPS,
    State,
    County,
    # 'Rank'   = Health Outcomes (Current Health)
    # 'Rank.1' = Health Factors  (Future Health Predictors)
    Health_Outcomes_Rank = Rank,
    Health_Factors_Rank  = Rank.1
  ) %>%
  filter(County != "") %>%
  na.omit()  # Remove rows where the county wasn't ranked

print("Row count for clean21_ranked:")
print(nrow(clean21_ranked))
head(clean21_ranked)


# --- FILE 4: Outcomes and Factors Sub-Rankings ---
data21_subranked <- read.csv(
  "raw_data/2021/Outcomes and Factors SubRankings.csv",
  skip       = 1,
  na.strings = c("N/A", "", " ")
)

clean21_subranked <- data21_subranked %>%
  dplyr::select(
    FIPS,
    State,
    County,
    Length_of_Life_Rank = Rank,    # Rank.1 = Quality of Life (not used here)
                                   # Rank.2 = Health Behaviors (not used here)
    Clinical_Care_Rank   = Rank.3,
    Socioeconomic_Rank   = Rank.4
  ) %>%
  filter(County != "") %>%
  na.omit()  # PREPROCESSING: Ensure complete dataset for EDA

print("Sample of Renamed Sub-Rankings:")
head(clean21_subranked)


# --- FILE 5: Ranked Measure Data ---
data21_rankedmeasures <- read.csv(
  "raw_data/2021/Ranked Measure Data.csv",
  skip       = 1,
  na.strings = c("N/A", "", " ")
)

clean21_rankedmeasures <- data21_rankedmeasures %>%
  dplyr::select(
    FIPS,
    State,
    County,
    YPLL_Rate             = Years.of.Potential.Life.Lost.Rate,  # Primary health outcome
    YPLL_Black            = YPLL.Rate..Black.,                  # Demographic comparison (RQ3)
    YPLL_White            = YPLL.Rate..White.,
    Percent_Poor_Health   = X..Fair.or.Poor.Health,             # Quality of life
    Percent_Low_Birthweight = X..Low.birthweight
  ) %>%
  filter(County != "") %>%
  na.omit()  # PREPROCESSING: Remove incomplete records

print("Verification: 2021 Ranked Measures Sample")
head(clean21_rankedmeasures, 5)


# --- FILE 6: Ranked Measures Sources and Years ---
# FIX 4: This file was being re-read into data21_metadata, overwriting the
# FILE 2 object. Given both files are the same source, the cleaned result is
# simply aliased here under a distinct name to preserve clarity.
metadata21 <- clean21_metadata

print("2021 Metadata Table Successfully Cleaned:")
head(metadata21)


# ---------------------------------------------------------
# STEP 2: INTEGRATING 2021 DATA
# ---------------------------------------------------------

# FIX 5: Join on all shared key columns (FIPS, State, County) to prevent
# duplicate .x / .y suffix columns being created for State and County.
master_2021 <- clean21_measures %>%
  inner_join(clean21_ranked,        by = c("FIPS", "State", "County")) %>%
  inner_join(clean21_subranked,     by = c("FIPS", "State", "County")) %>%
  inner_join(clean21_rankedmeasures, by = c("FIPS", "State", "County"))

# FIX 6: The old rename + select(-ends_with(".y")) block is no longer needed
# because the multi-column join above prevents duplicate suffixes entirely.
# A sanity check confirms no .x / .y columns remain.
stopifnot(!any(grepl("\\.(x|y)$", names(master_2021))))

print("--- MASTER 2021 SUCCESSFULLY ESTABLISHED ---")
print(paste("Final Row Count:", nrow(master_2021)))
head(master_2021[, 1:5])


# ---------------------------------------------------------
# DATA TYPE AUDIT
# Fixes overlapping axes and 'NR' discrete errors by coercing
# character columns to numeric (continuous).
# ---------------------------------------------------------

# 1. Rankings and Outcomes
master_2021$Health_Factors_Rank  <- as.numeric(as.character(master_2021$Health_Factors_Rank))
master_2021$Health_Outcomes_Rank <- as.numeric(as.character(master_2021$Health_Outcomes_Rank))
master_2021$Clinical_Care_Rank   <- as.numeric(as.character(master_2021$Clinical_Care_Rank))
master_2021$Socioeconomic_Rank   <- as.numeric(as.character(master_2021$Socioeconomic_Rank))

# 2. Economic and Quality of Life measures
master_2021$Median.Household.Income <- as.numeric(as.character(master_2021$Median.Household.Income))
master_2021$Percent_Poor_Health      <- as.numeric(as.character(master_2021$Percent_Poor_Health))

# 3. Racial Disparity Metrics (YPLL)
master_2021$YPLL_Rate  <- as.numeric(as.character(master_2021$YPLL_Rate))
master_2021$YPLL_Black <- as.numeric(as.character(master_2021$YPLL_Black))
master_2021$YPLL_White <- as.numeric(as.character(master_2021$YPLL_White))

# 4. PREPROCESSING: Remove rows where coercion failed (e.g. "NR" entries)
master_2021 <- na.omit(master_2021)

# 5. VERIFICATION
print("Audit Successful. Verified Numeric Columns:")
str(master_2021[c("YPLL_Black", "Health_Factors_Rank", "Median.Household.Income")])

# Final NA check
print("Final NA check for 2021 Master:")
print(sum(is.na(master_2021)))

# Export cleaned master file
write.csv(master_2021, "Master_2021_Cleaned.csv", row.names = FALSE)


# =========================================================
# EMMANUELLA OBIDIKE - EDA ANALYSIS AND INTERPRETATION
# =========================================================

data21 <- master_2021

head(data21)

# ---------------------------------------------------------
# SECTION 2: Summary of key variables (nationwide)
# ---------------------------------------------------------
summary_table <- data21 %>%
  summarise(
    avg_income       = mean(Median.Household.Income, na.rm = TRUE),
    avg_clinical_rank = mean(Clinical_Care_Rank,     na.rm = TRUE)
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
    x     = "Median Household Income",
    y     = "Clinical Care Rank (Lower = Better)"
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
    x     = "Health Factors Rank (Lower = Better)",
    y     = "Health Outcomes Rank (Lower = Better)"
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
    x     = "Median Household Income",
    y     = "Count"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 8: Racial Disparities (YPLL)
# ---------------------------------------------------------
racial_data <- data21 %>%
  select(YPLL_Black, YPLL_White) %>%
  pivot_longer(cols = everything(), names_to = "Group", values_to = "YPLL")

ggplot(racial_data, aes(x = Group, y = YPLL, fill = Group)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Racial Disparities in Premature Mortality (YPLL)",
    x     = "Population Group",
    y     = "YPLL Rate"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 9: Income vs Health Outcomes
# ---------------------------------------------------------
ggplot(data21, aes(x = Median.Household.Income, y = Health_Outcomes_Rank)) +
  geom_point(alpha = 0.3, color = "darkred") +
  labs(
    title = "Income vs Health Outcomes Across U.S. Counties (2021)",
    x     = "Median Household Income",
    y     = "Health Outcomes Rank"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 10: Clinical Care Distribution
# ---------------------------------------------------------
ggplot(data21, aes(x = Clinical_Care_Rank)) +
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
  labs(
    title = "Distribution of Clinical Care Rankings",
    x     = "Clinical Care Rank",
    y     = "Count"
  ) +
  theme_minimal()


# =========================================================
# CHLOE WASHINGTON - IMPLICATIONS, DISCUSSION, INTERPRETATION
# =========================================================

data21        <- master_2021
filtered_data <- data21   # No state filtering; reflects all U.S. counties

# ---------------------------------------------------------
# SECTION 3: Summary table for discussion
# ---------------------------------------------------------
summary_table <- filtered_data %>%
  group_by(State) %>%
  summarise(
    avg_income        = mean(Median.Household.Income, na.rm = TRUE),
    avg_clinical_rank = mean(Clinical_Care_Rank,      na.rm = TRUE)
  )
print("Summary table (average values by state):")
print(summary_table)
print("Interpretation: Differences in average income across states highlight regional disparities that may contribute to unequal healthcare access and outcomes.")

# ---------------------------------------------------------
# SECTION 4: Main plot (Income vs Clinical Care)
# ---------------------------------------------------------
ggplot(filtered_data, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Income vs Clinical Care Across U.S. Counties (2021)",
    x     = "Median Household Income",
    y     = "Clinical Care Rank (Lower = Better)"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 5: Correlation (Income vs Clinical Care)
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
# SECTION 6: State-level income comparison
# ---------------------------------------------------------
ggplot(summary_table, aes(x = reorder(State, avg_income), y = avg_income)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Income by State (2021)",
    x     = "State",
    y     = "Average Income"
  ) +
  theme_minimal()

# ---------------------------------------------------------
# SECTION 7: Health Factors vs Outcomes
# ---------------------------------------------------------
correlation2 <- cor(
  filtered_data$Health_Factors_Rank,
  filtered_data$Health_Outcomes_Rank,
  use = "complete.obs"
)
print("Correlation between health factors and health outcomes:")
print(correlation2)
print("Interpretation: The strong relationship between health factors and health outcomes suggests that environmental and behavioral conditions are closely linked to overall population health.")


# =========================================================
# SIDNEY CERAMI - VALIDATION, DATA QUALITY, AND ETHICS
# =========================================================

# ---------------------------------------------------------
# SECTION 1: Data Quality Check (Missing Values)
# ---------------------------------------------------------
master_2021 <- read.csv("Master_2021_Cleaned.csv")

print("Dataset loaded successfully:")
print(dim(master_2021))
print("Checking for missing values in master dataset:")
missing_values <- colSums(is.na(master_2021))
print(missing_values)
print("Total missing values in dataset:")
print(sum(is.na(master_2021)))

# ---------------------------------------------------------
# SECTION 2: Check for Duplicates
# ---------------------------------------------------------
# FIX 7: duplicated() returns a logical vector; sum() counts TRUE values.
# Variable renamed to n_duplicates for clarity.
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
  select(Median.Household.Income, Clinical_Care_Rank,
         Health_Outcomes_Rank, YPLL_Rate))

# ---------------------------------------------------------
# SECTION 5: Source Reliability & Credibility
# ---------------------------------------------------------
print("Data Source Evaluation:")
print("The dataset is derived from County Health Rankings data, which compiles")
print("information from credible sources such as the CDC, U.S. Census Bureau,")
print("and other national health databases.")
print("This increases the reliability and credibility of the dataset.")

# ---------------------------------------------------------
# SECTION 6: Limitations of the Dataset
# ---------------------------------------------------------
print("Dataset Limitations:")
print("- Data is limited to one year (2021), so trends over time cannot be analyzed.")
print("- Some counties may have missing or suppressed data that was removed during cleaning.")
print("- Rankings are relative, meaning they compare counties rather than measure absolute performance.")
print("- Socioeconomic and health variables may be influenced by external factors not in the dataset.")

# ---------------------------------------------------------
# SECTION 7: Potential Sources of Bias
# ---------------------------------------------------------
print("Potential Bias Analysis:")
print("- Rural counties may be underrepresented due to incomplete data.")
print("- Reporting differences between states may introduce inconsistencies.")
print("- Structural inequalities (race, income, geography) may influence outcomes and rankings.")
print("- Data aggregation at the county level may hide disparities within counties.")

# ---------------------------------------------------------
# SECTION 8: Impact on Results and Interpretation
# ---------------------------------------------------------
print("Impact on Analysis:")
print("These limitations and biases may affect the accuracy of conclusions.")
print("For example, correlations between income and health outcomes may not imply causation.")
print("Additionally, missing or removed data could lead to underrepresentation of certain populations.")
print("Therefore, results should be interpreted with caution within the context of these limitations.")

# ---------------------------------------------------------
# SECTION 9: Ethical Considerations
# ---------------------------------------------------------
print("Ethical Considerations:")
print("- The analysis avoids misuse of sensitive demographic data.")
print("- Results should not be used to stereotype or generalize populations.")
print("- Findings should inform policy to improve healthcare access, not reinforce disparities.")
print("- Transparency in data limitations is maintained to ensure responsible analysis.")
