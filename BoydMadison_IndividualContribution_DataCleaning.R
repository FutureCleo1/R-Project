# =========================================================
# CSC 308: Organization of Programming Languages Final Project
# Project: Healthcare Access Trends (2021)
# Students: Madison Boyd Individual Contributions
# =========================================================

# -------------------------------
# MADISON BOYD - DATA CLEANING & PREPROCESSING
# -------------------------------

# Load the required library for data cleaning
library(dplyr)

library(tidyverse)

# =========================================================
# SECTION: 2021 DATA PREPARATION AND ANALYSIS
# =========================================================

# ---------------------------------------------------------
# STEP 1: PROCESSING AND CLEANING 2021 DATA
# ---------------------------------------------------------


# --- FILE 1: Additional Measure Data ---
# skipped the first placeholder row told R to treat "N/A" as a missing value
data21_measures <- read.csv("raw_data/2021/AdditionalMeasureData.csv", 
                            skip = 1, 
                            na.strings = c("N/A", "", " "))

# Narrowing down to variables for Research Questions
clean21_measures <- data21_measures %>%
  select(
    FIPS, State, County,
    Median.Household.Income,
    X..Uninsured,
    X..Rural,
    Life.Expectancy
  ) %>%
  filter(County != "") %>%
  # PREPROCESSING: Removing the rows that were originally N/A
  na.omit()

# --- VERIFICATION STEP: Data Quality Check ---

print("Checking for missing values in 2021 Measures:")
print(sum(is.na(clean21_measures))) # Should output 0

print("Row count after cleaning (Verify it decreased):")
print(nrow(clean21_measures))

# view data
View(clean21_measures)

head(clean21_measures)

# Add Year label for future comparison
clean21_measures$Year <- 2021



# --- FILE 2. Additional Measures Sources and Years File ---

# 1. Read the file
data21_metadata <- read.csv("raw_data/2021/Addtl Measure Sources and Years.csv", 
                            skip = 0, 
                            na.strings = c("N/A", "", " "))

# 2. clean data 
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

# view data

View(clean21_metadata)

# --- FILE 3. Outcomes and Factors Rankings File ---

library(dplyr)

# read file data
data21_outcomesandfactorsrankings <- read.csv("raw_data/2021/Outcomes and Factors Rankings.csv", 
                                              skip = 1, 
                                              na.strings = c("N/A", "", " "))

# view data
View(data21_outcomesandfactorsrankings)

# clean data

clean21_ranked <- data21_outcomesandfactorsrankings %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    # In this file: 
    # 'Rank' is Health Outcomes (Current Health)
    # 'Rank.1' is Health Factors (Future Health Predictors)
    Health_Outcomes_Rank = Rank,
    Health_Factors_Rank = Rank.1
  ) %>%
  filter(County != "") %>%
  # Data Quality: Remove rows where the county wasn't ranked (N/A)
  na.omit()

nrow(clean21_ranked)
# Check to ensure the names are changed
head(clean21_ranked)

View(clean21_ranked)

# --- FILE 4. Outcomes and Factors sub Rankings File ---

library(dplyr)

# Load the sub-rankings file, skipping the top header

data21_subranked <- read.csv("raw_data/2021/Outcomes and Factors SubRankings.csv", 
                             skip = 1, 
                             na.strings = c("N/A", "", " "))

# Narrowing down to the sub-ranks that answer the 6 research questions

# map the R names to the actual categories 
clean21_subranked <- data21_subranked %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    # Match numbered ranks to their categories
    Length_of_Life_Rank   = Rank,
    # (Rank.1 is usually Quality of Life)
    # (Rank.2 is usually Health Behaviors)
    Clinical_Care_Rank    = Rank.3, 
    Socioeconomic_Rank    = Rank.4
  ) %>%
  filter(County != "") %>%
  # PREPROCESSING: Ensure complete dataset for EDA
  na.omit()

# Verification for  Report
print("Sample of Renamed Sub-Rankings:")
head(clean21_subranked)

# view data

View(clean21_subranked)

# --- FILE 5. Ranked Measure Data ---
# read file
data21_rankedmeasures <- read.csv("raw_data/2021/Ranked Measure Data.csv", 
                                  skip = 1, 
                                  na.strings = c("N/A", "", " "))

# view file
View(data21_rankedmeasures)

# 2. Narrow down to the "In-Depth" variables for report
clean21_rankedmeasures <- data21_rankedmeasures %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    # Primary health outcome metric
    YPLL_Rate = Years.of.Potential.Life.Lost.Rate,
    # Demographic comparison metrics (for Research Question 3)
    YPLL_Black = YPLL.Rate..Black.,
    YPLL_White = YPLL.Rate..White.,
    # Quality of life indicators
    Percent_Poor_Health = X..Fair.or.Poor.Health,
    Percent_Low_Birthweight = X..Low.birthweight
  ) %>%
  filter(County != "") %>%
  # PREPROCESSING: Ensuring Data Quality by removing incomplete records
  na.omit()

# 3. Verification: Check the first 5 rows
print("Verification: 2021 Ranked Measures Sample")
head(clean21_rankedmeasures, 5)

# view data

View(clean21_rankedmeasures)


# --- FILE 6. Ranked Measures Sources and Year ---
# 1. Read the file
data21_metadata <- read.csv("raw_data/2021/Addtl Measure Sources and Years.csv", 
                            skip = 0, 
                            na.strings = c("N/A", "", " "))

# 2. Clean the file
metadata21 <- data21_metadata %>%
  dplyr::select(
    Category    = Focus.Area,
    Measure     = Measure,
    Definition = Description,
    Source      = Source,
    Years       = Year.s.
  ) %>%
  # PREPROCESSING: Remove the rows where Measure is empty (Data Quality)
  filter(!is.na(Measure) & Measure != "")

# 3. Verification
print("2021 Metadata Table Successfully Cleaned:")
head(metadata21)

#View data

View(metadata21)


# ---------------------------------------------------------
# STEP 2: INTEGRATING 2021 DATA
# ---------------------------------------------------------

# 1. Join all files strictly by FIPS 
master_2021 <- clean21_measures %>%
  inner_join(clean21_ranked, by = "FIPS") %>%
  inner_join(clean21_subranked, by = "FIPS") %>%
  inner_join(clean21_rankedmeasures, by = "FIPS")

# 2. Standardize names and remove the .x/.y duplicates
master_2021 <- master_2021 %>%
  rename(State = State.x, County = County.x) %>%
  select(-ends_with(".y"), -ends_with(".x.x"), -ends_with(".y.y"))

# 3. Final Check 
print("--- MASTER 2021 SUCCESSFULLY ESTABLISHED ---")
print(paste("Final Row Count:", nrow(master_2021)))

# Check the first few columns to ensure names are clean
head(master_2021[, 1:5])

# install the package

install.packages("ggplot2")

# verify and load

library(ggplot2)

# install hexbin package 

install.packages("hexbin")

# install tidyr

install.packages("tidyr")





# --- B. DATA TYPE AUDIT (FIXES OVERLAPPING AXES & 'NR' DISCRETE ERRORS) ---
# Ensure R treats these as Numeric Doubles (Continuous) rather than text (Discrete).
# This prevents the "Discrete value supplied to continuous scale" error in ggplot.

# --- B. DATA TYPE AUDIT ---
# Fixing the 'replacement has 0 rows' error by using verified column names.

# 1. Force Rankings and Outcomes to Numeric
master_2021$Health_Factors_Rank  <- as.numeric(as.character(master_2021$Health_Factors_Rank))
master_2021$Health_Outcomes_Rank <- as.numeric(as.character(master_2021$Health_Outcomes_Rank))
master_2021$Clinical_Care_Rank   <- as.numeric(as.character(master_2021$Clinical_Care_Rank))
master_2021$Socioeconomic_Rank   <- as.numeric(as.character(master_2021$Socioeconomic_Rank))

# 2. Force Economic and Quality of Life Measures
master_2021$Median.Household.Income <- as.numeric(as.character(master_2021$Median.Household.Income))
master_2021$Percent_Poor_Health     <- as.numeric(as.character(master_2021$Percent_Poor_Health))

# 3. Force Racial Disparity Metrics (YPLL - Years of Potential Life Lost)
# replace Life Expectancy as primary demographic disparity proof
master_2021$YPLL_Rate  <- as.numeric(as.character(master_2021$YPLL_Rate))
master_2021$YPLL_Black <- as.numeric(as.character(master_2021$YPLL_Black))
master_2021$YPLL_White <- as.numeric(as.character(master_2021$YPLL_White))

# 4. PREPROCESSING: Data Quality Check
# This removes rows where conversion failed (like "NR" entries)
master_2021 <- na.omit(master_2021)

# 5. VERIFICATION
print("Audit Successful. Verified Numeric Columns:")
str(master_2021[c("YPLL_Black", "Health_Factors_Rank", "Median.Household.Income")])


# --- C. Final 2021 Audit ---
# Proving Data Quality by checking for any remaining NAs in master file
print("Final NA check for 2021 Master:")
print(sum(is.na(master_2021)))

# Exporting a CSV version of Master File to folder 
write.csv(master_2021, "Master_2021_Cleaned.csv", row.names = FALSE)
