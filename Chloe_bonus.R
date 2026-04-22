# =========================================================
# CSC 308: Organization of Programming Languages Final Project
# Project: Healthcare Access Trends (2021) - MIDWEST REGION
# Students:Chloe Washington (Bonus)
# =========================================================

# -------------------------------
# GLOBAL SETUP & LIBRARIES
# -------------------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)

# Automatic Working Directory Fix
if (!file.exists("raw_data")) {
  possible_dirs <- list.dirs(".", recursive = FALSE)
  for (d in possible_dirs) {
    if (file.exists(file.path(d, "raw_data"))) {
      setwd(d)
      break
    }
  }
}

# =========================================================
# SECTION: 2021 DATA PREPARATION AND CLEANING
# =========================================================

# --- FILE 1: Additional Measure Data ---
data21_measures <- read.csv("raw_data/2021/AdditionalMeasureData.csv", skip = 1, na.strings = c("N/A", "", " "))
clean21_measures <- data21_measures %>%
  select(FIPS, State, County, Median.Household.Income, X..Uninsured, X..Rural, Life.Expectancy) %>%
  filter(County != "") %>% na.omit()
clean21_measures$Year <- 2021

# --- FILE 2: Outcomes and Factors Rankings ---
data21_outcomesandfactorsrankings <- read.csv("raw_data/2021/Outcomes and Factors Rankings.csv", skip = 1, na.strings = c("N/A", "", " "))
clean21_ranked <- data21_outcomesandfactorsrankings %>%
  select(FIPS, State, County, Health_Outcomes_Rank = Rank, Health_Factors_Rank = Rank.1) %>%
  filter(County != "") %>% na.omit()

# --- FILE 3: Outcomes and Factors SubRankings ---
data21_subranked <- read.csv("raw_data/2021/Outcomes and Factors SubRankings.csv", skip = 1, na.strings = c("N/A", "", " "))
clean21_subranked <- data21_subranked %>%
  select(FIPS, State, County, Length_of_Life_Rank = Rank, Clinical_Care_Rank = Rank.3, Socioeconomic_Rank = Rank.4) %>%
  filter(County != "") %>% na.omit()

# --- FILE 4: Ranked Measure Data ---
data21_rankedmeasures <- read.csv("raw_data/2021/Ranked Measure Data.csv", skip = 1, na.strings = c("N/A", "", " "))
clean21_rankedmeasures <- data21_rankedmeasures %>%
  select(FIPS, State, County, YPLL_Rate = Years.of.Potential.Life.Lost.Rate, YPLL_Black = YPLL.Rate..Black., YPLL_White = YPLL.Rate..White., Percent_Poor_Health = X..Fair.or.Poor.Health, Percent_Low_Birthweight = X..Low.birthweight) %>%
  filter(County != "") %>% na.omit()

# ---------------------------------------------------------
# STEP 2: INTEGRATING & FILTERING (EARLY MIDWEST FILTER)
# ---------------------------------------------------------

# 1. Join all files strictly by FIPS 
master_2021_full <- clean21_measures %>%
  inner_join(clean21_ranked, by = "FIPS") %>%
  inner_join(clean21_subranked, by = "FIPS") %>%
  inner_join(clean21_rankedmeasures, by = "FIPS")

# 2. Standardize names
master_2021_full <- master_2021_full %>%
  rename(State = State.x, County = County.x) %>%
  select(-ends_with(".y"), -ends_with(".x.x"), -ends_with(".y.y"))

# 3. APPLY MIDWEST FILTER (Early Injection)
midwest_list <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", 
                  "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin",
                  "IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI")

master_2021 <- master_2021_full %>% filter(State %in% midwest_list)

# 4. Data Type Audit (Fixing Numeric conversion)
cols_to_fix <- c("Health_Factors_Rank", "Health_Outcomes_Rank", "Clinical_Care_Rank", 
                 "Socioeconomic_Rank", "Median.Household.Income", "Percent_Poor_Health",
                 "YPLL_Rate", "YPLL_Black", "YPLL_White")

master_2021[cols_to_fix] <- lapply(master_2021[cols_to_fix], function(x) as.numeric(as.character(x)))
master_2021 <- na.omit(master_2021)

print(paste("Final Midwest County Count:", nrow(master_2021)))
write.csv(master_2021, "Master_2021_Midwest_Cleaned.csv", row.names = FALSE)

# -------------------------------
#EDA & INTERPRETATION
# -------------------------------
data21 <- master_2021

ggplot(data21, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
  geom_point(color = "blue", alpha = 0.3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Midwest: Income vs Clinical Care (2021)", x = "Median Household Income", y = "Clinical Care Rank") +
  theme_minimal()

print("Midwest Correlation (Income vs Clinical):")
print(cor(data21$Median.Household.Income, data21$Clinical_Care_Rank, use = "complete.obs"))

# -------------------------------
# IMPLICATIONS & DISCUSSION
# -------------------------------
summary_table <- data21 %>%
  group_by(State) %>%
  summarise(avg_income = mean(Median.Household.Income), avg_clinical_rank = mean(Clinical_Care_Rank))

ggplot(summary_table, aes(x = reorder(State, avg_income), y = avg_income)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "Average Income by Midwest State (2021)", x = "State", y = "Average Income") +
  theme_minimal()

# -------------------------------
# VALIDATION & ETHICS
# -------------------------------
print("Midwest Data Quality Check:")
print(colSums(is.na(master_2021)))
print(paste("Duplicate Rows:", sum(duplicated(master_2021))))

# -------------------------------
# VISUALIZATION
# -------------------------------
# Income vs Uninsured (Midwest)
ggplot(data21, aes(x = Median.Household.Income, y = X..Uninsured)) + 
  geom_point(alpha = 0.5, color = "darkgreen") + 
  labs(title = "Midwest: Income vs Uninsured Rate", x = "Median Household Income", y = "Uninsured %") + 
  theme_minimal()

# Rural vs Clinical Care (Midwest)
ggplot(data21, aes(x = X..Rural, y = Clinical_Care_Rank)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Midwest: Rural % vs Clinical Care Rank", x = "Rural %", y = "Clinical Care Rank") +
  theme_minimal()