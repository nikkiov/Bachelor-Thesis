# Script that processes (calculates the circadian rhythm features of) the summarized actigraphy files
# -------------------------------------
# This script calculates the cosinor features (MESOR, amplitude and acrophase) and the circadian rhythm features (IV, IS, RA)
# based on a summarized actigraphy file.
# The outputs of this script are three files: one .csv file with all cosinor features of all participants,
# one .csv file with all circadian regularity features of all participants,
# and one .csv file with all circadian regularity features of all participants with a consisent and unfragmented circadian rhythm for the sensitivity analysis.
# -------

library(tidyverse)
library(nparACT)
library(cosinor)
library(cosinor2)
library(fs)
library(astroFns)

input_directory <- "Bachelor Thesis/Summarized Data"
summarized_files <- dir_ls(input_directory)

cosinor_df <- data.frame()
nparACT_df <- data.frame()

# Loops over all summarized actigraphy files and calculates all circadian features
for (file in summarized_files){
  read_file <- read.csv(file)
  
  # Get the SEQN (id) from the participant for later identification
  id <- paste0(gsub("\\.csv$", "", basename(file)))
  
  # Cosinor analysis
  # Filter on days with 24 hours of data and change the time to hours since the start (i.e. 0 hours)
  cosinor_analysis <- read_file %>%
    mutate(
      timestamp = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      date = as.Date(timestamp)) %>%
    group_by(date) %>%
    filter(n_distinct(hour(timestamp)) == 24) %>%
    ungroup() %>%
    mutate(
      time = as.numeric(difftime(timestamp, min(timestamp), units = "hours"))) %>%
    select(time, ENMO_t) %>%
    na.omit()
  
  # Fit a cosinor model to the data and get the results
  cosinor_lm <- cosinor.lm(ENMO_t ~ time(time), data = cosinor_analysis, period = 24)
  acrophase <- correct.acrophase(cosinor_lm)                        # Get the correct acrophase
  acrophase <- abs(acrophase)                                       # Absolute value of the acrophase
  result_cosinor <- data.frame(
    mesor = cosinor_lm$coefficients["(Intercept)"],
    amplitude = cosinor_lm$coefficients["amp"],
    rad_acro = cosinor_lm$coefficients["acr"],
    acrophase = rad2hms(cosinor_lm$coefficients["acr"]),            # Convert radians to hours, minutes and seconds
    corrected_acrophase = rad2hms(acrophase),
    corrected_rad_acro = acrophase
  )
  
  # Bind participant's SEQN to the result and bind the result to the data frame
  result_cosinor <- cbind(ID = id, result_cosinor)
  cosinor_df <-rbind(cosinor_df, result_cosinor, make.row.names = FALSE)
  
  # nparACT analysis
  # Format columns
  npar_analysis <- read_file %>%  
    mutate(
      time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ"),
      ENMO = as.numeric(ENMO_t)
    ) %>%
    na.omit()
  
  # Filter on days with 24 hours of data 
  edited_npar_analysis <- npar_analysis %>%
    group_by(date = as.Date(time)) %>%      
    filter(n_distinct(hour(time)) == 24) %>%
    ungroup() %>%
    select(time = time, activity = ENMO)
  
  activity_matrix <- as.matrix(edited_npar_analysis)
  
  # Calculate the nparACT features, bind participant's SEQN to the result and bind the result to the data frame
  nparACT <- nparACT_base("activity_matrix", SR = 1/60, plot = F, fulldays = TRUE)
  result_nparACT <- cbind(ID = id, nparACT)
  nparACT_df <- rbind(nparACT_df, result_nparACT)
}

# Save cosinor_df to a .csv file
colnames(cosinor_df) <- gsub("X[0-9]+\\.", "", colnames(cosinor_df))
write.csv(cosinor_df, "Bachelor Thesis/Processed/cosinor.csv", row.names = FALSE)

# Save nparACT_df to a .csv file
colnames(nparACT_df) <- gsub("X[0-9]+\\.", "", colnames(nparACT_df))
write.csv(nparACT_df, "Bachelor Thesis/Processed/nparACT.csv", row.names = FALSE)

# Exclude IV >95th percentile and IS <5th percentile and write it to a different .csv file (for later sensitivity analysis)
nparACT_percentile <- nparACT_df %>%
  filter(
    IS >= quantile(IS, 0.05),
    IV <= quantile(IV, 0.95)
  )
write.csv(nparACT_percentile, "Bachelor Thesis/Processed/nparACT_percentile.csv", row.names = FALSE)
