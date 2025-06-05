# Script that puts data from the questionnaires (.xpt files) and the circadian rhythm features (.csv files) together into one big dataframe
# -------------------------------------
# This script reads all questionnaire and circadian rhythm feature data files and merges them based on the SEQNs (participant IDS).
# The script also re-classifies questionnaire results
# The output is a big .csv file of all data needed as input for the machine learning models.
# -------

library(haven)
library(tidyverse)

# Read all data
sleep_data <- read_xpt("Bachelor Thesis/Survey Data/2011/SLQ_G.xpt")
depression_data <- read_xpt("Bachelor Thesis/Survey Data/2011/DPQ_G.xpt")
cosinor_data <- read_csv("Bachelor Thesis/Processed/cosinor.csv")
nparACT_data <- read_csv("Bachelor Thesis/Processed/nparACT_percentile.csv")     # Normal or percentile (change)
demographic_data <- read_xpt("Bachelor Thesis/Survey Data/2011/DEMO_G.xpt")
alcohol_data <- read_xpt("Bachelor Thesis/Survey Data/2011/ALQ_G.xpt")
bmi_data <- read_xpt("Bachelor Thesis/Survey Data/2011/WHQ_G.xpt")
smoke_data <- read_xpt("Bachelor Thesis/Survey Data/2011/SMQ_G.xpt")
occupation_data <- read_xpt("Bachelor Thesis/Survey Data/2011/OCQ_G.xpt")

# Filter sleep disturbance andd depression data to only include participants who are also included in the analysis
read_ids <- read.csv("Bachelor Thesis/SEQN_list.csv")
filtered_ids <- as.numeric(read_ids[1,])

filtered_sleep_data <- sleep_data %>%
  filter(SEQN %in% filtered_ids)

filtered_depression_data <- depression_data %>%
  filter(SEQN %in% filtered_ids)

# Use depression classification: every question answered with a 2 or higher changes to 1 (else 0), if participants have more than 4 1s 
# or answered 2 or higher to question DPQ010 or DPQ020, they are put into the depression group (1), the others are put in the healthy controls (0)
depression <- filtered_depression_data %>%
  mutate(across(
    starts_with("DPQ"),
    ~ifelse(. >= 2, 1, 0)
  )) %>%
  mutate(depression = (ifelse(rowSums(.[, -1]) > 4 | DPQ010 == 1 | DPQ020 == 1, 1, 0))) %>%
  select(SEQN, depression)

# Use sleep disturbances classification: every 2 is changed to 0 (as 2 = no). If participants have answered yes (1) to one or more questions, 
# they are put into the sleep disturbances group (1), the others are put in the healthy controls (0)
sleep <- filtered_sleep_data %>%
  select(SEQN, SLQ050, SLQ060) %>%
  mutate(
    SLQ050 = ifelse(SLQ050 == 2, 0, SLQ050),
    SLQ060 = ifelse(SLQ060 == 2, 0, SLQ060),
    sleep_problems = ifelse(SLQ050 == 1 | SLQ060 == 1, 1, 0)
  ) %>%
  select(SEQN, sleep_problems)

# Combine the depression and sleep disturbances questionnaires with the circadian rhythm features
cosinor_depression <- left_join(depression, cosinor_data, by = c("SEQN" = "ID"))
nparACT_depression <- right_join(depression, nparACT_data, by = c("SEQN" = "ID"))
cosinor_sleep <- left_join(sleep, cosinor_data, by = c("SEQN" = "ID"))
nparACT_sleep <- right_join(sleep, nparACT_data, by = c("SEQN" = "ID"))
combined_depression <- inner_join(cosinor_depression, nparACT_depression, by = c("SEQN" = "SEQN", "depression" = "depression"))
combined_sleep <- inner_join(cosinor_sleep, nparACT_sleep, by = c("SEQN" = "SEQN", "sleep_problems" = "sleep_problems"))

# Select all features from the demographics questionnaire used for later data analysis
demographic <- demographic_data %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH3, DMDEDUC2, DMDHHSIZ, WTINT2YR, SDMVPSU, SDMVSTRA) %>%
  rename(
    gender = RIAGENDR,     # 1 = male. 2 = female
    age = RIDAGEYR,
    race = RIDRETH3,       # 1 = mexican american, 2 = other hispanic, 3 = white, 4 = black, 6 = asian, 7 = other
    education = DMDEDUC2,
    household_size = DMDHHSIZ, 
    sample_weights = WTINT2YR,
    psu = SDMVPSU,
    strata = SDMVSTRA)  %>%
  mutate(education = ifelse(education >= 1 & education <= 3, 0,
                            ifelse(education >= 4, 1, NA)),                # 0 = low educated, 1 = highly educated
         household_size = ifelse(household_size >= 1 & household_size <= 2, 0,
                                 ifelse(household_size >= 3 & household_size <= 5, 1,
                                        ifelse(household_size > 5, 2, NA))))    # 0 = small, 1 = medium, 2 = large           

# Merge the demographics data with the earlier data
combined_depression <- inner_join(combined_depression, demographic, by = c("SEQN" = "SEQN"))
combined_sleep <- inner_join(combined_sleep, demographic, by = c("SEQN" = "SEQN"))

# Determine alcohol consumption (0, 1, 2)
alcohol <- alcohol_data %>%
  select(SEQN, ALQ130) %>%
  rename(no_light_heavy_drinker = ALQ130) %>%
  mutate(no_light_heavy_drinker = ifelse(no_light_heavy_drinker >= 0 & no_light_heavy_drinker <= 1, 0,
                                       ifelse(no_light_heavy_drinker >= 2 & no_light_heavy_drinker <= 4, 1,
                                              ifelse(no_light_heavy_drinker >= 5, 2, NA))))    # 0 = no drinker, 1 = light drinker, 2 = heavy drinker

# Determine smoking behaviour (0, 1)
smoking <- smoke_data %>%
  select(SEQN, SMQ020) %>%
  rename(heavy_smoker_yes_no = SMQ020) %>%
  mutate(heavy_smoker_yes_no = ifelse(heavy_smoker_yes_no == 2, 0, heavy_smoker_yes_no))      # 1 = yes, 0 = no

# Merge the alcohol and smoking data with the earlier data
combined_depression <- inner_join(combined_depression, alcohol, by = c("SEQN" = "SEQN"))
combined_sleep <- inner_join(combined_sleep, alcohol, by = c("SEQN" = "SEQN"))
combined_depression <- inner_join(combined_depression, smoking, by = c("SEQN" = "SEQN"))
combined_sleep <- inner_join(combined_sleep, smoking, by = c("SEQN" = "SEQN"))

# Determine BMI by first calculating it and then classifying it as underweight (0), optimal range (1), overweight (2), and obese (3)
bmi <- bmi_data %>%
  mutate(BMI = round((WHD020*703)/(WHD010^2))) %>%
  select(SEQN, BMI) %>%
  mutate(BMI = ifelse(BMI >= 0 & BMI < 18.5, 0,
                      ifelse(BMI >= 18.5 & BMI < 25, 1,
                             ifelse(BMI >= 25 & BMI < 30, 2,
                                    ifelse(BMI >= 30, 3, NA)))))    # 0 = underweight, 1 = optimal range, 2 = overweight, 3 = obese

# Merge the BMI data with the earlier data
combined_depression <- inner_join(combined_depression, bmi, by = c("SEQN" = "SEQN"))
combined_sleep <- inner_join(combined_sleep, bmi, by = c("SEQN" = "SEQN"))  

# Determine if participants are employed or unemployed
employment <- occupation_data %>%
  select(SEQN, OCD150) %>%
  rename(employed = OCD150) %>%
  mutate(employed = ifelse(employed == 1 | employed == 2, 1, 0))  # 1 = employed, 0 = unemployed

# Merge the employment data with the earlier data
combined_depression <- inner_join(combined_depression, employment, by = c("SEQN" = "SEQN"))
combined_sleep <- inner_join(combined_sleep, employment, by = c("SEQN" = "SEQN"))

# Merge everything together (both depression and sleep) and write it to a .csv file 
combined_everything <- inner_join(sleep, combined_depression, by = c("SEQN" = "SEQN"))
write.csv(combined_everything, "Bachelor Thesis/Processed/population_percentile.csv", row.names = FALSE)
