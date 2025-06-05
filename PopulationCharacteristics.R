# Script used to make the population characteristics table.
# -------------------------------------
# This script sums (or gives the mean of) several features. 
# It also gives the inter-quartile range (or percentages) of those features.
# -------

library(haven)
library(survey)
library(dplyr)

# Code source: https://static-bcrf.biochem.wisc.edu/courses/Tabular-data-analysis-with-R-and-Tidyverse/book/12-usingNHANESweights.html
nhanesAnalysis <- read_csv("Bachelor Thesis/Processed/population_percentile.csv")   

# Specifies a complex survey design
nhanesDesign <- svydesign(id = ~psu,
                          strata = ~strata,
                          weights = ~sample_weights,
                          nest = TRUE,
                          data = nhanesAnalysis)

svyquantile(~age, nhanesDesign, c(0.25, 0.5, 0.75))

sum(nhanesAnalysis$gender == 1)     # man
sum(nhanesAnalysis$gender == 2)     # woman
svytable(~gender, nhanesDesign) %>% # percentage of men and women
  prop.table()

sum(nhanesAnalysis$race == 1)       # mexican american
sum(nhanesAnalysis$race == 2)       # other-hispanic
sum(nhanesAnalysis$race == 3)       # white
sum(nhanesAnalysis$race == 4)       # black
sum(nhanesAnalysis$race == 6)       # asian
sum(nhanesAnalysis$race == 7)       # other
svytable(~race, nhanesDesign) %>% 
  prop.table()

sum(nhanesAnalysis$education == 0)  # low educated
sum(nhanesAnalysis$education == 1)  # highly educated
svytable(~education, nhanesDesign) %>%
  prop.table()

sum(nhanesAnalysis$employed == 0)   # not employed
sum(nhanesAnalysis$employed == 1)   # employed
svytable(~employed, nhanesDesign) %>%
  prop.table()

sum(nhanesAnalysis$household_size == 0)  # small
sum(nhanesAnalysis$household_size == 1)  # medium
sum(nhanesAnalysis$household_size == 2)  # large
svytable(~household_size, nhanesDesign) %>%
  prop.table()

sum(nhanesAnalysis$BMI == 0)  # underweight
sum(nhanesAnalysis$BMI == 1)  # normal
sum(nhanesAnalysis$BMI == 2)  # overweight
sum(nhanesAnalysis$BMI == 3)  # obese
svytable(~BMI, nhanesDesign) %>%
  prop.table()

sum(nhanesAnalysis$heavy_smoker_yes_no == 0)     # not a heavy smoker
sum(nhanesAnalysis$heavy_smoker_yes_no == 1)     # heavy smoker
svytable(~heavy_smoker_yes_no, nhanesDesign) %>%
  prop.table()

sum(nhanesAnalysis$no_light_heavy_drinker == 0)  # no drinker
sum(nhanesAnalysis$no_light_heavy_drinker == 1)  # light drinker
sum(nhanesAnalysis$no_light_heavy_drinker == 2)  # heavy drinker
svytable(~no_light_heavy_drinker, nhanesDesign) %>%
  prop.table()

# Calculate mean and inter-quartile range for the circadian rhythm features
svyquantile(~mesor, nhanesDesign, c(0.25, 0.5, 0.75))
svyquantile(~amplitude, nhanesDesign, c(0.25, 0.5, 0.75))
svyquantile(~corrected_rad_acro, nhanesDesign, c(0.25, 0.5, 0.75))
svyquantile(~IV, nhanesDesign, c(0.25, 0.5, 0.75))
svyquantile(~IS, nhanesDesign, c(0.25, 0.5, 0.75))
svyquantile(~RA, nhanesDesign, c(0.25, 0.5, 0.75))

# Sum and give percentages of participants with certain outcomes
sum(nhanesAnalysis$depression == 1 & nhanesAnalysis$sleep_problems == 0)   # only depression
sum(nhanesAnalysis$sleep_problems == 1 & nhanesAnalysis$depression == 0)   # only sleep problems
sum(nhanesAnalysis$sleep_problems == 1 & nhanesAnalysis$depression == 1)   # both depression and sleep problems
sum(nhanesAnalysis$depression == 0 & nhanesAnalysis$sleep_problems == 0)   # none
svytable(~depression + sleep_problems, nhanesDesign) %>%
  prop.table()
svytable(~depression, nhanesDesign) %>%
  prop.table()
svytable(~sleep_problems, nhanesDesign) %>%
  prop.table()
