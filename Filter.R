# Script to filter NHANES Data
# -------------------------------------
# This script filters the NHANES demographic data set on the following features:
#
# 1. One should have Physical Activity Monitor (PAM) data
# 2. One should have >= 4 valid days (>= 16 hours per day)
# 3. One should not have data quality flags
# 4. One should be between 20 and 45 years old
# 5. One should not be pregnant
# 6. One should not have NAs or "I dont knows" in sleep questionnaire data and depression questionnaire data
# 7. One should have valid sleep hours
# 8. One should have data about sex, education level, employment, race, 
#    total number of people in one's household, smoking status, alcohol consumption and BMI
#
# The output of this script is a list and vector of the filtered IDs. 
# This vector will decide which files will be unzipped later in the main file, 
# ensuring that only participants with valid information will be included in the study.
# -------------------------------------

library(tidyverse)
library(haven)

# Read all questionnaire data files
demographic_data <- read_xpt("Bachelor Thesis/Survey Data/2011/DEMO_G.xpt")
sleep_data <- read_xpt("Bachelor Thesis/Survey Data/2011/SLQ_G.xpt")
alcohol_data <- read_xpt("Bachelor Thesis/Survey Data/2011/ALQ_G.xpt")
depression_data <- read_xpt("Bachelor Thesis/Survey Data/2011/DPQ_G.xpt")
bmi_data <- read_xpt("Bachelor Thesis/Survey Data/2011/WHQ_G.xpt")
smoke_data <- read_xpt("Bachelor Thesis/Survey Data/2011/SMQ_G.xpt")
occupation_data <- read_xpt("Bachelor Thesis/Survey Data/2011/OCQ_G.xpt")
day_summary_file <- read_xpt("Bachelor Thesis/PAXDAY_G.xpt")

message("IDs in demographic data: ", nrow(demographic_data))

# Filter on available PAM data
zip_folder <- "public/datasets/Physical_Activity_Monitor-Raw_Date_80hz"
files <- list.files(zip_folder)
id_s <- gsub("\\.tar\\.bz2", "", files)
id_s <- as.integer(id_s)

filtered_PAM <- demographic_data %>%
filter(SEQN %in% id_s)
message("IDs after filtering on available PAM data: ", nrow(filtered_PAM))

# Filter on valid days >= 4 (valid hours >= 16)
filtered_day_summary_file <- day_summary_file %>%
  mutate(total_valid_minutes = PAXWWMD + PAXSWMD,
         valid_day = total_valid_minutes >= 960) %>%              # >= 16 hours
  group_by(SEQN) %>%
  summarise(valid_days = sum(valid_day)) %>%
  filter(valid_days >= 4) %>%
  pull(SEQN)

filtered_data_valid_days <- filtered_PAM %>%
  filter(SEQN %in% filtered_day_summary_file)
message("IDs after filtering on valid days: ", nrow(filtered_data_valid_days))

# Filter on no quality flags
filtered_day_summary_file2 <- day_summary_file %>%
  group_by(SEQN) %>%
  filter(all(PAXQFD == 0 )) %>%
  ungroup()

filtered_data_quality_flags <- filtered_data_valid_days %>%
  filter(SEQN %in% filtered_day_summary_file2$SEQN)
message("IDs after excluding quality flags: ", nrow(filtered_data_quality_flags))

# Filter on age
filtered_data_age <- filtered_data_quality_flags %>%
  filter(RIDAGEYR > 19 & RIDAGEYR < 46)
message("IDs after filtering on age: ", nrow(filtered_data_age))

# Exclude pregnant participants
filtered_data_pregnancy <-filtered_data_age %>%
  filter(RIDEXPRG %in% c(2,3,4) | is.na(RIDEXPRG))
message("IDs after excluding on pregnancy: ", nrow(filtered_data_pregnancy))
  
# Make sure there are no NAs and "I don't knows" in sleep data 
available_sleep_data <- sleep_data %>%
  filter(!is.na(SLD010H) & !is.na(SLQ050) & !is.na (SLQ060) & SLQ050 %in% c(1,2) & SLQ060 %in% c(1,2) & (SLD010H > 1 & SLD010H < 13))     
print(nrow(available_sleep_data))

filtered_data_sleep_data <- filtered_data_pregnancy %>%
  filter(SEQN %in% available_sleep_data$SEQN)
message("IDs after filtering on available sleep data: ", nrow(filtered_data_sleep_data))

# Make sure there are no NAs and "I don't knows" in depression data
available_depression_data <- depression_data %>%
  filter(!is.na(DPQ010) & !is.na(DPQ020) & !is.na(DPQ030) & !is.na(DPQ040) & !is.na(DPQ050) & !is.na(DPQ060) & !is.na(DPQ070) & !is.na(DPQ080) & !is.na(DPQ090) & !is.na(DPQ100)
         & DPQ010 %in% c(0,1,2,3) & DPQ020 %in% c(0,1,2,3) & DPQ030 %in% c(0,1,2,3) & DPQ040 %in% c(0,1,2,3) & DPQ050 %in% c(0,1,2,3) & DPQ060 %in% c(0,1,2,3)
         & DPQ070 %in% c(0,1,2,3) & DPQ080 %in% c(0,1,2,3) & DPQ090 %in% c(0,1,2,3) & DPQ100 %in% c(0,1,2,3))     
print(nrow(available_sleep_data))

filtered_data_depression_data <- filtered_data_sleep_data %>%
  filter(SEQN %in% available_depression_data$SEQN)
message("IDs after filtering on available depression data: ", nrow(filtered_data_depression_data))

# Make sure covariates sex, education level, and household size are available
filtered_data_covariates <- filtered_data_depression_data %>%
  filter(!is.na(RIAGENDR))                                             # Sex 
  message("Gender not available: ", nrow(filtered_data_covariates))

filtered_data_covariates <- filtered_data_covariates %>%
  filter(DMDEDUC2 > 0 & DMDEDUC2 < 6)                                  # Education level
  message("Education level not available: ", nrow(filtered_data_covariates))

filtered_data_covariates <- filtered_data_covariates %>%
  filter(!is.na(RIDRETH3))                                             # Race
  message("Race not available: ", nrow(filtered_data_covariates))

filtered_data_covariates <- filtered_data_covariates %>%
  filter(!is.na(DMDHHSIZ))                                             # Household size
  message("Number of people in household not available: ", nrow(filtered_data_covariates))

# Make sure covariate smoking status is available
smoking <- smoke_data %>%
  filter(SMQ020 == 1 | SMQ020 == 2)                                    # 1 = at least 100 cigarettes = smoker, 2 = non-smoker
print(nrow(smoking))

filtered_data_smoking <- filtered_data_covariates %>%
  filter(SEQN %in% smoking$SEQN)
message("IDs after filtering on covariate smoking: ", nrow(filtered_data_smoking))

# Make sure covariate alcohol consumption is available
drinking <- alcohol_data %>%
  filter(ALQ130 > 0 & ALQ130 < 96)                                     # Avg # alcoholic drinks/day - past 12 mos
print(nrow(drinking))

filtered_data_alcohol <- filtered_data_smoking %>%
  filter(SEQN %in% drinking$SEQN)
message("IDs after filtering on covariate alcohol: ", nrow(filtered_data_alcohol)) 

# Make sure covariate BMI is available
BMI <- bmi_data %>%
  filter(WHD010 > 1 & WHD010 < 100 & WHD020 > 1 & WHD020 < 500)        # WHD010 = Height in inches, WHD020 = Weight in pounds
print(nrow(BMI))

filtered_data_BMI <- filtered_data_alcohol %>%
  filter(SEQN %in% BMI$SEQN)
message("IDs after filtering on covariate BMI: ", nrow(filtered_data_BMI))

# Make sure covariate occupation is available
occupation <- occupation_data %>%
  filter(OCD150 %in% c(1, 2, 3, 4))                                    # Type of work done last week      
print(nrow(occupation))

filtered_data_occupation <- filtered_data_BMI %>%
  filter(SEQN %in% occupation$SEQN)
message("IDs after filtering on covariate occupation: ", nrow(filtered_data_occupation)) 

# Get SEQNs and save it as a .csv file
SEQNs_after_filtering <- filtered_data_occupation$SEQN
SEQN_list <- as.list(SEQNs_after_filtering)
write.csv(SEQN_list, "Bachelor Thesis/SEQN_list.csv", row.names = FALSE)
