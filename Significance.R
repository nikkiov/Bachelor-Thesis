# Script to compare circadian rhythm features between outcome groups
# -------------------------------------
# This script calculates the mean and standard deviation of all circadian rhythm features for all outcome groups,
# and then performs a statistical test (t-test) to assess group differences.
# -------

library(tidyverse)

circadian_data <- read_csv("Bachelor Thesis/Processed/population_percentile.csv")          # Read file

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
