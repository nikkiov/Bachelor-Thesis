# Script that summarizes the raw actigraphy files
# -------------------------------------
# This script unzips a zipped NHANES actigraphy file and summarizes the files within it per minute.
# The output of this script is almost 400 summarized actigraphy .csv files in one directory, one per participant.
# -------------------------------------

library(SummarizedActigraphy)
library(tidyverse)  
library(fs)
library(haven)

# Get the SEQNs from the SEQN list and make sure only the zipped files with the same SEQNs get unzipped later
read_ids <- read.csv("Bachelor Thesis/SEQN_list.csv")
filtered_ids <- as.numeric(read_ids[1,])
batch_ids <- filtered_ids[329:392]                                           # Do the summarization in batches
zip_directory <- "public/datasets/Physical_Activity_Monitor-Raw_Date_80hz"
zip_files <- dir_ls(zip_directory)
filtered_files <- zip_files[gsub("\\.tar\\.bz2$", "", basename(zip_files)) %in% batch_ids]

temp_directory <- tempdir()

complete_files <- c()

complete_data_directory <- "Bachelor Thesis/Summarized Data"

# Loops over all included zip files, unzips them, processes them and finally deletes the unzipped files again.
for (file in filtered_files){
  unlink(dir_ls(temp_directory, full.names = TRUE), recursive = TRUE)   # Important for when the code crashes before it can remove the files
  output_file <- paste0(gsub("\\.tar\\.bz2$", "", basename(file)))
  message("Unzipping file: ", basename(file))
  untar(file, exdir = temp_directory)

  # Only include sensor data
  csv_files <- dir_ls(temp_directory, regexp = "\\.sensor\\.csv$")
  combined_data <- data.frame()
  
  message("Combining csv files from file ", output_file)
  
  # Loops over all csv files and combines them into one data frame
  for (csv_file in csv_files){
    data <- read_csv(csv_file, show_col_types = FALSE)
  
    # Rename and format column
    colnames(data)[1] <- "HEADER_TIME_STAMP"
    data$HEADER_TIME_STAMP <- as.POSIXct(data$HEADER_TIME_STAMP, format = "$Y-%m-%d %H:%M:%S", tz = "UTC")
    combined_data <- bind_rows(combined_data, data)
    }
  
  # Checks if the data frame is empty
  if (nrow(combined_data) == 0){
    message("No data found in ", output_file, ", skipping to next file" )
    unlink(dir_ls(temp_directory, full.names = TRUE), recursive = TRUE)
    next
  }
  
  # Summarize data from the data frame
  combined_data$HEADER_TIME_STAMP <- as.POSIXct(combined_data$HEADER_TIME_STAMP, tz = "UTC")
  message("Summarizing ", output_file)
  result <- summarize_daily_actigraphy(combined_data, flag_data = FALSE)
  result$time <- as.POSIXct(result$time, tz = "UTC")
  
  # Save the summarization to a .csv file (SEQN.csv)
  complete_files <- c(complete_files, output_file)
  write_csv(result, file.path(complete_data_directory, paste0(output_file, ".csv")))
  
  # Remove all unzipped files from the directory
  unlink(dir_ls(temp_directory, full.names = TRUE), recursive = TRUE)
}
