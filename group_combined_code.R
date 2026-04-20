# =========================================================
# CSC 308: Organization of Programming Languages Final Project
# Project: Healthcare Access Trends (2021-2023)
# Students: Madison Boyd, Emmanuella Obidike, Chloe Washington
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


# =========================================================
# SECTION: 2022 DATA PREPARATION AND ANALYSIS
# =========================================================

library(dplyr)

# ---------------------------------------------------------
# STEP 1: PROCESSING AND CLEANING 2022 DATA
# ---------------------------------------------------------

# --- FILE 1: Additional Measure Data ---

data22_measures <- read.csv("raw_data/2022/Additional Measure Data.csv", 
                            skip = 1, 
                            na.strings = c("N/A", "", " "))

head(data22_measures)

names(data22_measures)  # Lists every column name in the console
View(data22_measures)   # shows data in another tab

# Creating the clean 2022 dataset
clean22_measures <- data22_measures %>%
  # Select and Rename based on the headers
  
  select(
    FIPS, 
    State,
    County,
    Median.Household.Income,
    Covid_Deaths_2020 = X..deaths.due.to.COVID.19.during.2020,
    Covid_Death_Rate_22 = COVID.19.death.rate,
    Life_Exp_22 = Life.Expectancy
  ) %>%
  
  #  Basic Cleaning: Remove rows where County is empty 
  
  filter(County != "")

# View the results
View(clean22_measures)

write.csv(clean22_measures, "clean22_measures.csv", row.names = FALSE)

# --- FILE 2: Additional Measure Sources and Years ---

#  Read the 2022 raw metadata
data22_metadata <- read.csv("raw_data/2022/Addtl Measure Sources and Years.csv", 
                            skip = 0, 
                            na.strings = c("N/A", "", " "))

#  Clean and Rename
clean22_metadata <- data22_metadata %>%
  dplyr::select(
    Category    = Focus.Area,
    Measure     = Measure,
    Description = Description,
    Source      = Source,
    Years       = Year.s.
  ) %>%
  # Filter out empty rows or section headers
  filter(!is.na(Measure) & Measure != "")

#  Verification
print("2022 Metadata Successfully Cleaned!")
head(clean22_metadata)

View(clean22_metadata)

write.csv(clean22_metadata, "clean22_metadata.csv", row.names = FALSE)

# --- FILE 3: Outcomes and Factors Rankings ---

#  Read the 2022 file

data22_ranked <- read.csv("raw_data/2022/Outcomes and Factors Rankings.csv", 
                          skip = 1, 
                          na.strings = c("N/A", "", " "))

#  Clean and Rename
clean22_ranked <- data22_ranked %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    # Standardizing names for side-by-side comparison with 2021
    Health_Outcomes_Rank = Rank,
    Health_Factors_Rank = Rank.1
  ) %>%
  # Keep only actual counties
  filter(County != "") %>% 
  # Drop N/As for counties that weren't ranked
  na.omit()

#  Verification
print("--- 2022 RANKINGS CLEANED ---")
print(paste("Number of Ranked Counties:", nrow(clean22_ranked)))
head(clean22_ranked)

View(clean22_ranked)
# 4. Save to Console 
write.csv(clean22_ranked, "clean22_ranked.csv", row.names = FALSE)

# --- FILE 4: Outcomes and Factors SubRankings ---

#  Read the 2022 SubRankings file
data22_subranked <- read.csv("raw_data/2022/Outcomes and Factors SubRankings.csv", 
                             skip = 1, 
                             na.strings = c("N/A", "", " "))

#  Clean and Rename
clean22_subranked <- data22_subranked %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    Length_of_Life_Rank   = Rank,
    Clinical_Care_Rank    = Rank.3, 
    Socioeconomic_Rank    = Rank.4
  ) %>%
  # Remove state-level summary rows
  filter(County != "") %>%
  # Remove unranked counties 
  na.omit()

# Verification

head(clean22_subranked)

#  SAVE FILE
write.csv(clean22_subranked, "clean22_subranked.csv", row.names = FALSE)

# --- FILE 5: Ranked Measure Data ---

# read 2022 file
data22_rankedmeasures <- read.csv("raw_data/2022/Ranked Measure Data.csv", 
                                  skip = 1, 
                                  na.strings = c("N/A", "", " "))

 Clean and Rename
clean22_rankedmeasures <- data22_rankedmeasures %>%
  dplyr::select(
    FIPS, State, County,
    YPLL_Rate = Years.of.Potential.Life.Lost.Rate,
   
    YPLL_Black = YPLL.Rate..Black.,
    YPLL_White = YPLL.Rate..white., 
    Percent_Poor_Health = X..Fair.or.Poor.Health,
    Percent_Low_Birthweight = X..Low.birthweight
  ) %>%
  filter(County != "") %>%
  # Keep counties with a primary YPLL rate
  filter(!is.na(YPLL_Rate))

#  Verification
print("--- 2022 RANKED MEASURES CLEANED ---")
print(paste("Rows available for YPLL analysis:", nrow(clean22_rankedmeasures)))

#  SAVE FILE
write.csv(clean22_rankedmeasures, "clean22_rankedmeasures.csv", row.names = FALSE)

# --- FILE 6: Ranked Measure Sources and Years ---

#  Read the 2022 metadata file
metadata22 <- read.csv("raw_data/2022/Ranked Measure Sources and Years.csv", 
                       skip = 0, 
                       na.strings = c("N/A", "", " "))

#  Clean and Rename

clean22_metadata_ranked <- metadata22 %>%
  dplyr::select(
    Category    = Focus.Area,
    Measure     = Measure,
    Definition  = Description,
    Source      = Source,
    Years       = Year.s.
  ) %>%
  # Filter out empty rows to keep the dictionary clean
  filter(!is.na(Measure) & Measure != "")

#  Verification
print("--- 2022 RANKED METADATA CLEANED ---")
head(clean22_metadata_ranked)

#  SAVE FILE 
write.csv(clean22_metadata_ranked, "metadata22.csv", row.names = FALSE)

# ---------------------------------------------------------
# STEP 2 (2022): INTEGRATING 2022 DATA
# ---------------------------------------------------------

#  Join all 2022 files strictly by FIPS 
master_2022 <- clean22_measures %>%
  inner_join(clean22_ranked, by = "FIPS") %>%
  inner_join(clean22_subranked, by = "FIPS") %>%
  inner_join(clean22_rankedmeasures, by = "FIPS")

# Cleanup: Standardize names and remove the .x/.y duplicates
master_2022 <- master_2022 %>%
  rename(State = State.x, County = County.x) %>%
  select(-ends_with(".y"), -ends_with(".x.x"), -ends_with(".y.y"))

#  Final Check for Join Success
print("--- MASTER 2022 SUCCESSFULLY ESTABLISHED ---")
print(paste("Final 2022 Row Count:", nrow(master_2022)))

# ---------------------------------------------------------
# B. DATA TYPE AUDIT (2022)
# ---------------------------------------------------------


#  Force Rankings and Outcomes to Numeric
master_2022$Health_Factors_Rank  <- as.numeric(as.character(master_2022$Health_Factors_Rank))
master_2022$Health_Outcomes_Rank <- as.numeric(as.character(master_2022$Health_Outcomes_Rank))
master_2022$Clinical_Care_Rank   <- as.numeric(as.character(master_2022$Clinical_Care_Rank))
master_2022$Socioeconomic_Rank   <- as.numeric(as.character(master_2022$Socioeconomic_Rank))

#  Force Economic and Quality of Life Measures
master_2022$Median.Household.Income <- as.numeric(as.character(master_2022$Median.Household.Income))
master_2022$Percent_Poor_Health     <- as.numeric(as.character(master_2022$Percent_Poor_Health))

#  Force Racial Disparity Metrics (YPLL)
master_2022$YPLL_Rate  <- as.numeric(as.character(master_2022$YPLL_Rate))
master_2022$YPLL_Black <- as.numeric(as.character(master_2022$YPLL_Black))
master_2022$YPLL_White <- as.numeric(as.character(master_2022$YPLL_White))

#  PREPROCESSING
master_2022 <- na.omit(master_2022)

#  VERIFICATION
print("2022 Audit Successful. Verified Numeric Columns:")
str(master_2022[c("YPLL_Black", "Health_Factors_Rank", "Median.Household.Income")])

#  SAVE THE MASTER
write.csv(master_2022, "Master_2022_Final.csv", row.names = FALSE)



# =========================================================
# SECTION: 2023 DATA PREPARATION AND ANALYSIS
# =========================================================

library(dplyr)

# ---------------------------------------------------------
# STEP 1: PROCESSING AND CLEANING 2023 DATA
# ---------------------------------------------------------




# --- FILE 1: Additional Measure Data ---
data23_measures_raw <- read.csv("raw_data/2023/Additional Measure Data.csv", 
                                skip = 1, 
                                na.strings = c("N/A", "", " "))

clean23_measures <- data23_measures_raw %>%
  dplyr::select(
    FIPS, State, County,
    Median.Household.Income,
    # Renaming 'Uninsured Adults' to the standard name so graphs don't break
    X..Uninsured = X..Uninsured.Adults,
    
    X..Rural = X..Rural
  ) %>%
  filter(County != "")

clean23_measures$Year <- 2023
write.csv(clean23_measures, "clean23_measures.csv", row.names = FALSE)


# --- FILE 2: Metadata - Additional Measures ---
data23_metadata <- read.csv("raw_data/2023/Addtl Measure Sources and Years.csv", 
                            skip = 0, 
                            na.strings = c("N/A", "", " "))

clean23_metadata <- data23_metadata %>%
  dplyr::select(Category = Focus.Area, Measure, Description, Source, Years = Year.s.) %>%
  filter(!is.na(Measure) & Measure != "")

write.csv(clean23_metadata, "clean23_metadata.csv", row.names = FALSE)


# --- FILE 3: Outcomes and Factors Rankings ---
data23_ranked <- read.csv("raw_data/2023/Outcomes and Factors Rankings.csv", 
                          skip = 1, 
                          na.strings = c("N/A", "", " "))

clean23_ranked <- data23_ranked %>%
  dplyr::select(FIPS, State, County, 
                Health_Outcomes_Rank = Rank, 
                Health_Factors_Rank = Rank.1) %>%
  filter(County != "") %>%
  na.omit()

write.csv(clean23_ranked, "clean23_ranked.csv", row.names = FALSE)

# --- FILE 4: Outcomes and Factors SubRankings ---
data23_subranked <- read.csv("raw_data/2023/Outcomes and Factors SubRankings.csv", 
                             skip = 1, 
                             na.strings = c("N/A", "", " "))

clean23_subranked <- data23_subranked %>%
  dplyr::select(FIPS, State, County,
                Length_of_Life_Rank = Rank,
                Clinical_Care_Rank = Rank.3, 
                Socioeconomic_Rank = Rank.4) %>%
  filter(County != "") %>%
  na.omit()

write.csv(clean23_subranked, "clean23_subranked.csv", row.names = FALSE)

# --- FILE 5: Ranked Measure Data ---
data23_rankedmeasures <- read.csv("raw_data/2023/Ranked Measure Data.csv", 
                                  skip = 1, 
                                  na.strings = c("N/A", "", " "))

clean23_rankedmeasures <- data23_rankedmeasures %>%
  dplyr::select(
    FIPS, State, County,
    YPLL_Rate = contains("Potential.Life.Lost.Rate"),
    YPLL_Black = contains("YPLL.Rate..Black"),
    YPLL_White = contains("YPLL.Rate..white"), # Checks lowercase first
    Percent_Poor_Health = contains("Fair.or.Poor.Health"),
    Percent_Low_Birthweight = contains("Low.birthweight")
  ) %>%
  filter(County != "") %>%
  filter(!is.na(YPLL_Rate))

write.csv(clean23_rankedmeasures, "clean23_rankedmeasures.csv", row.names = FALSE)


# --- FILE 6: Metadata - Ranked Measures ---
metadata23 <- read.csv("raw_data/2023/Ranked Measure Sources and Years.csv", 
                       skip = 0, 
                       na.strings = c("N/A", "", " "))

clean23_metadata_ranked <- metadata23 %>%
  dplyr::select(Category = Focus.Area, Measure, Definition = Description, Source, Years = Year.s.) %>%
  filter(!is.na(Measure) & Measure != "")

write.csv(clean23_metadata_ranked, "metadata23.csv", row.names = FALSE)

# ---------------------------------------------------------
# STEP 2 (2023): INTEGRATING 2023 DATA
# ---------------------------------------------------------

master_2023 <- clean23_measures %>%
  inner_join(clean23_ranked, by = "FIPS") %>%
  inner_join(clean23_subranked, by = "FIPS") %>%
  inner_join(clean23_rankedmeasures, by = "FIPS")

#  Standardize names and remove the .x/.y duplicates
master_2023 <- master_2023 %>%
  # Rename the multiple matches from the Join to standard names
  rename(
    State = State.x, 
    County = County.x,
    YPLL_Black = YPLL_Black1, # Selecting the primary rate
    YPLL_White = YPLL_White1  # Selecting the primary rate
  ) %>%
  # Remove duplicate suffixes and extra YPLL matches (Black2, White2, etc.)
  select(-ends_with(".y"), -ends_with(".x.x"), -ends_with(".y.y"), 
         -contains("Black2"), -contains("Black3"), -contains("Black4"),
         -contains("White2"), -contains("White3"), -contains("White4"))

# Final Check for Join success
print("--- MASTER 2023 SUCCESSFULLY ESTABLISHED ---")
print(paste("Final 2023 Row Count:", nrow(master_2023)))


# ---------------------------------------------------------
# B. DATA TYPE AUDIT (2023)
# ---------------------------------------------------------

# 1. Force Rankings and Outcomes to Numeric
master_2023$Health_Factors_Rank  <- as.numeric(as.character(master_2023$Health_Factors_Rank))
master_2023$Health_Outcomes_Rank <- as.numeric(as.character(master_2023$Health_Outcomes_Rank))
master_2023$Clinical_Care_Rank   <- as.numeric(as.character(master_2023$Clinical_Care_Rank))
master_2023$Socioeconomic_Rank   <- as.numeric(as.character(master_2023$Socioeconomic_Rank))

# 2. Force Economic and Quality of Life Measures
master_2023$Median.Household.Income <- as.numeric(as.character(master_2023$Median.Household.Income))
master_2023$Percent_Poor_Health     <- as.numeric(as.character(master_2023$Percent_Poor_Health))

# 3. Force Racial Disparity Metrics (YPLL)
master_2023$YPLL_Rate  <- as.numeric(as.character(master_2023$YPLL_Rate))
master_2023$YPLL_Black <- as.numeric(as.character(master_2023$YPLL_Black))
master_2023$YPLL_White <- as.numeric(as.character(master_2023$YPLL_White))

# 4. PREPROCESSING: Remove any "NR" or missing rows created by the numeric force
master_2023 <- na.omit(master_2023)

# 5. VERIFICATION
print("2023 Audit Successful. Verified Numeric Columns:")
str(master_2023[c("YPLL_Black", "Health_Factors_Rank", "Median.Household.Income")])

# 6. SAVE THE MASTER 
write.csv(master_2023, "Master_2023_Final.csv", row.names = FALSE)


# -------------------------------
# EMMANUELLA OBIDIKE - DATA SOURCING, EDA ANALYSIS, AND INTERPRETATION
# -------------------------------

# library(dplyr)
# library(ggplot2)

# ---------------------------------------------------------
# SECTION 1: Load the cleaned dataset
# ---------------------------------------------------------

# Load the final cleaned dataset from the project
data21 <- master_2021

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

# -------------------------------
# Chloe Washington - Implications of Results, Analytical Discussion, and Interpretation
# -------------------------------
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
