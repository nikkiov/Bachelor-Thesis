# Script to compare circadian rhythm features between outcome groups
# -------------------------------------
# This script calculates the mean and standard deviation of all circadian rhythm features for all outcome groups,
# and then performs a statistical test (t-test) to assess group differences.
# -------

library(tidyverse)
library(effsize)

circadian_data <- read_csv("Bachelor Thesis/Processed/population.csv")          # Read file

# Mean and sd of all circadian rhythm feature from the depression group
circadian_data %>%
  filter(depression == 1) %>%
  summarise(
    mean_mesor = mean(mesor),
    sd_mesor = sd(mesor),
    amplitude_mean = mean(amplitude),
    sd_amplitude = sd(amplitude),
    mean_acro = mean(corrected_rad_acro),
    sd_acro = sd(corrected_rad_acro),
    mean_IS = mean(IS),
    sd_IS = sd(IS),
    mean_IV = mean(IV),
    sd_IV = sd(IV),
    mean_RA = mean(RA),
    sd_RA = sd(RA)
  )

# Mean and sd of all circadian rhythm feature from the not depression (healthy controls) group
circadian_data %>%
  filter(depression == 0) %>%
  summarise(
    mean_mesor = mean(mesor),
    sd_mesor = sd(mesor),
    amplitude_mean = mean(amplitude),
    sd_amplitude = sd(amplitude),
    mean_acro = mean(corrected_rad_acro),
    sd_acro = sd(corrected_rad_acro),
    mean_IS = mean(IS),
    sd_IS = sd(IS),
    mean_IV = mean(IV),
    sd_IV = sd(IV),
    mean_RA = mean(RA),
    sd_RA = sd(RA)
  )

# Mean and sd of all circadian rhythm feature from the sleep disturbances group
circadian_data %>%
  filter(sleep_problems == 1) %>%
  summarise(
    mean_mesor = mean(mesor),
    sd_mesor = sd(mesor),
    amplitude_mean = mean(amplitude),
    sd_amplitude = sd(amplitude),
    mean_acro = mean(corrected_rad_acro),
    sd_acro = sd(corrected_rad_acro),
    mean_IS = mean(IS),
    sd_IS = sd(IS),
    mean_IV = mean(IV),
    sd_IV = sd(IV),
    mean_RA = mean(RA),
    sd_RA = sd(RA)
  )

# Mean and sd of all circadian rhythm feature from the not sleep disturbances (healthy controls) group
circadian_data %>%
  filter(sleep_problems == 0) %>%
  summarise(
    mean_mesor = mean(mesor),
    sd_mesor = sd(mesor),
    amplitude_mean = mean(amplitude),
    sd_amplitude = sd(amplitude),
    mean_acro = mean(corrected_rad_acro),
    sd_acro = sd(corrected_rad_acro),
    mean_IS = mean(IS),
    sd_IS = sd(IS),
    mean_IV = mean(IV),
    sd_IV = sd(IV),
    mean_RA = mean(RA),
    sd_RA = sd(RA)
  )

# Perform t-test to compare circadian rhythm features between groups 
t.test(mesor ~ depression, data = circadian_data)
t.test(mesor ~ sleep_problems, data = circadian_data)
t.test(corrected_rad_acro ~ depression, data = circadian_data)
t.test(corrected_rad_acro ~ sleep_problems, data = circadian_data)
t.test(amplitude ~ depression, data = circadian_data)
t.test(amplitude ~ sleep_problems, data = circadian_data)
t.test(IS ~ depression, data = circadian_data)
t.test(IS ~ sleep_problems, data = circadian_data)
t.test(IV ~ depression, data = circadian_data)
t.test(IV ~ sleep_problems, data = circadian_data)
t.test(RA ~ depression, data = circadian_data)
t.test(RA ~ sleep_problems, data = circadian_data)

# Perform cohen d to compare circadian rhythm features between groups
cohen.d(mesor ~ depression, data = circadian_data)
cohen.d(mesor ~ sleep_problems, data = circadian_data)
cohen.d(corrected_rad_acro ~ depression, data = circadian_data)
cohen.d(corrected_rad_acro ~ sleep_problems, data = circadian_data)
cohen.d(amplitude ~ depression, data = circadian_data)
cohen.d(amplitude ~ sleep_problems, data = circadian_data)
cohen.d(IS ~ depression, data = circadian_data)
cohen.d(IS ~ sleep_problems, data = circadian_data)
cohen.d(IV ~ depression, data = circadian_data)
cohen.d(IV ~ sleep_problems, data = circadian_data)
cohen.d(RA ~ depression, data = circadian_data)
cohen.d(RA ~ sleep_problems, data = circadian_data)

# Perform chi-square (t-test for age) to compare covariate features between groups
chisq.test(table(circadian_data$gender, circadian_data$depression))
chisq.test(table(circadian_data$BMI, circadian_data$depression))
chisq.test(table(circadian_data$education, circadian_data$depression))
chisq.test(table(circadian_data$employed, circadian_data$depression))
chisq.test(table(circadian_data$heavy_smoker_yes_no, circadian_data$depression))
chisq.test(table(circadian_data$no_light_heavy_drinker, circadian_data$depression))
chisq.test(table(circadian_data$household_size, circadian_data$depression))
chisq.test(table(circadian_data$race, circadian_data$depression))
t.test(age ~ depression, data = circadian_data)
chisq.test(table(circadian_data$gender, circadian_data$sleep_problems))
chisq.test(table(circadian_data$BMI, circadian_data$sleep_problems))
chisq.test(table(circadian_data$education, circadian_data$sleep_problems))
chisq.test(table(circadian_data$employed, circadian_data$sleep_problems))
chisq.test(table(circadian_data$heavy_smoker_yes_no, circadian_data$sleep_problems))
chisq.test(table(circadian_data$no_light_heavy_drinker, circadian_data$sleep_problems))
chisq.test(table(circadian_data$household_size, circadian_data$sleep_problems))
chisq.test(table(circadian_data$race, circadian_data$sleep_problems))
t.test(age ~ sleep_problems, data = circadian_data)
