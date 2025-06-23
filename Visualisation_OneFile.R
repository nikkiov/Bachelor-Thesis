# Script that summarizes, processes and plots data from one (or three for comparison) for visualization purposes. 
# -------------------------------------
# This script summarizes and processes data from one participant. It also plots them for visualization purposes in the thesis.
# This script also plots data from three different participants to facilitate visual comparisons in the thesis. 
# For more information, see the SummaryRawData.R and CircadianFeaturesExtraction.R files.
# -------------------------------------

library(tidyverse)  
library(fs)       
library(haven)
library(ggplot2)
library(patchwork)
library(cosinor)
library(cosinor2)
library(astroFns)
library(nparACT)

# Untar one file and put it in a temporary directory
temp_directory = tempdir()
unlink(dir_ls(temp_directory, full.names = TRUE), recursive = TRUE)
untar("public/datasets/Physical_Activity_Monitor-Raw_Date_80hz/62189.tar.bz2", exdir = temp_directory)
csv_files <- dir_ls(temp_directory, regexp = "\\.sensor\\.csv$")

combined_data <- data.frame()

# Put all .csv files together into one big data frame
for (csv_file in csv_files){
  data <- read_csv(csv_file, show_col_types = FALSE)
  
  colnames(data)[1] <- "HEADER_TIME_STAMP"
  data$HEADER_TIME_STAMP <- as.POSIXct(data$HEADER_TIME_STAMP, format = "$Y-%m-%d %H:%M:%S", tz = "UTC")
  combined_data <- bind_rows(combined_data, data)
}

# Change the format of HEADER_TIME_STAMP
combined_data$HEADER_TIME_STAMP <- as.POSIXct(combined_data$HEADER_TIME_STAMP, tz = "UTC")

# Get all data from one day
plot_data <- combined_data %>%
  filter(as.Date(HEADER_TIME_STAMP) == as.Date("2000-01-13"))

# Plot the x-axis
plot_x <- ggplot(plot_data, aes(x=HEADER_TIME_STAMP, y=X)) +
  geom_line() +
  labs(title = "X-axis", x = NULL, y = NULL) +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

# Plot the y-axis
plot_y <- ggplot(plot_data, aes(x=HEADER_TIME_STAMP, y=Y)) +
  geom_line() +
  labs(title = "Y-axis", x = NULL, y = NULL)  +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 16))

# Plot the z-axis
plot_z <- ggplot(plot_data, aes(x=HEADER_TIME_STAMP, y=Z)) +
  geom_line() +
  labs(title = "Z-axis", x = NULL, y = NULL) +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

# Combine all axes together
combined_plot <- (plot_x/plot_y/plot_z) +
  plot_annotation(title = "Raw Triaxial Accelerometer Data from 1 Day (Participant ID: 62189)") &
  theme(text = element_text(size=20),
        plot.title = element_text(size = 30),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16)) &
  labs(x = "Time in the day", y = "Acceleration (m/s2)")

# Skip the preprocsesing step and just use the summarized files to save time for the next part
summary_result <- read_csv("Bachelor Thesis/Summarized Data/62189.csv")

# Cosinor analysis
# Filter on days with 24 hours of data and calculate the time as hours since start of the measurement
cosinor_analysis <- summary_result %>%
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

# Get a specific date
selected_date <- summary_result %>%
  mutate(
    timestamp = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    date = as.Date(timestamp)) %>%
  group_by(date) %>%
  filter(n_distinct(hour(timestamp)) == 24) %>%
  ungroup() %>%
  distinct(date) %>%
  slice(5) %>%
  pull(date) 

# Get the data from the specific date
one_day <- summary_result %>%
  mutate(
    timestamp = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    date = as.Date(timestamp)) %>%
  filter(date == selected_date) %>%
  mutate(
    time = as.numeric(difftime(timestamp, min(timestamp), units = "hours"))) %>%
  select(time, timestamp, ENMO_t) %>%
  na.omit()

# Plot data from the specific date
summarized_plot_one_day <- ggplot(one_day, aes(x=timestamp, y=ENMO_t)) +
  geom_line() +
  labs(x = "Time in the day", y = "ENMO_t (g)") +
  plot_annotation(title = "24-Hour ENMO_t Activity Pattern (Participant ID: 62189)") &
                  theme(text = element_text(size=20),
                        plot.title = element_text(size = 30),
                        axis.text.y = element_text(size = 12),
                        axis.text.x = element_text(size = 16))
  
# Fit a cosine on all data
cosinor_lm <- cosinor.lm(ENMO_t ~ time(time), data = cosinor_analysis, period = 24)
corrected_rad_acrophase <- correct.acrophase(cosinor_lm)
abs_acrophase <- abs(corrected_rad_acrophase)
result_cosinor <- data.frame(
  mesor = cosinor_lm$coefficients["(Intercept)"],
  amplitude = cosinor_lm$coefficients["amp"],
  corrected_acrophase = rad2hms(abs_acrophase)
)

# Fit a cosine on data from the specific date
cosinor_lm_one <- cosinor.lm(ENMO_t ~ time(time), data = one_day, period = 24)
corrected_rad_acrophase <- correct.acrophase(cosinor_lm_one)

# Plot the cosine and data from the specific date
cosinor_plot <- ggplot_cosinor.lm(cosinor_lm_one)
cosinor_plot + 
  geom_line(data=one_day, aes(x=time, y=ENMO_t), alpha = 0.3, color = "blue") +
  labs(x = "Time since start of the day (hours)", y = "ENMO_t (g)") +
  plot_annotation(title = "Fitted Cosine for Day 5 (Participant ID: 61289)") &
  theme(text = element_text(size=30),
        plot.title = element_text(size = 40),
        axis.text.y = element_text(size = 22),
        axis.text.x = element_text(size = 26))

# Plot the cosine derived from all data
cosinor_plot <- ggplot_cosinor.lm(cosinor_lm) +
  labs(x = "Time since start of the day (hours)", y = "ENMO_t (g)") &
  plot_annotation(title = "Fitted Cosine, Summarized Over 7 Days, Showing MESOR, Acrophase and Amplitude (Participant ID: 61289)") &
    theme(text = element_text(size=20),
          plot.title = element_text(size = 23),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 16))

# Plot the cosine derived from all data and the data (ENMO_t) from the specific date
cosinor_plot + 
  geom_line(data=one_day, aes(x=time, y=ENMO_t), alpha = 0.3, color = "blue") +
  labs(x = "Time since start of the day (hours)", y = "ENMO_t (g)") +
  plot_annotation(title = "Fitted Cosine, Summarized Over 7 Days (Participant ID: 61289)") &
  theme(text = element_text(size=30),
        plot.title = element_text(size = 40),
        axis.text.y = element_text(size = 22),
        axis.text.x = element_text(size = 26))

# nparACT
# Change format of the data
npar_analysis <- summary_result %>%  
  mutate(
    time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ"),
    ENMO = as.numeric(ENMO_t),
    hour = hour(time),
    minute = minute(time),
    time_only = hms(format(time, "%H:%M:%S")),
    plot_time = as.POSIXct("1970-01-01") + time_only
  ) %>%
  na.omit()

# Filter on days with 24 hours of data 
edited_npar_analysis <- npar_analysis %>%
  group_by(date = as.Date(time)) %>%
  filter(n_distinct(hour(time)) == 24) %>%
  ungroup()

# Summarize the data per day and hour
npar_plot_data <- edited_npar_analysis %>%
  group_by(date = as.Date(time), hour) %>%
  summarise(ENMO = mean(ENMO)) 

npar_plot_data <- npar_plot_data %>%
  group_by(hour) %>%
  summarise(ENMO = mean(ENMO)) 

# Change the matrix format for later plotting
activity_matrix <- as.matrix(edited_npar_analysis)
time_data <- activity_matrix[,1]
activity_data <- activity_matrix[,-1]
activity_data <- as.numeric(activity_data)
combined_matrix <- cbind(time_data, activity_data)

# Plot the summarized actigraphy data for one participant
npar_1 <- ggplot(npar_plot_data, aes(x = hour, y = ENMO)) +
  geom_col() +
  labs(title = "Actigraphy Plot, 24 Hours (Participant ID: 62189)", subtitle = "Average across days", x = "Time (start: 00:00)", y = "Movement Intensity")  +
  theme(text = element_text(size=20),
        plot.title = element_text(size = 30),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

# Summarize the actigraphy data for the second participant
summary_result2 <- read_csv("Bachelor Thesis/Summarized Data/67442.csv")

npar_analysis2 <- summary_result2 %>%  
  mutate(
    time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ"),
    ENMO = as.numeric(ENMO_t),
    hour = hour(time),
    minute = minute(time),
    time_only = hms(format(time, "%H:%M:%S")),
    plot_time = as.POSIXct("1970-01-01") + time_only
  ) %>%
  na.omit()

edited_npar_analysis2 <- npar_analysis2 %>%
  group_by(date = as.Date(time)) %>%
  filter(n_distinct(hour(time)) == 24) %>%
  ungroup()

npar_plot_data2 <- edited_npar_analysis2 %>%
  group_by(date = as.Date(time), hour) %>%
  summarise(ENMO = mean(ENMO)) 

npar_plot_data2 <- npar_plot_data2 %>%
  group_by(hour) %>%
  summarise(ENMO = mean(ENMO)) 

# Summarize the actigraphy data for the third participant
summary_result3 <- read_csv("Bachelor Thesis/Summarized Data/63033.csv")

npar_analysis3 <- summary_result3 %>%  
  mutate(
    time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ"),
    ENMO = as.numeric(ENMO_t),
    hour = hour(time),
    minute = minute(time),
    time_only = hms(format(time, "%H:%M:%S")),
    plot_time = as.POSIXct("1970-01-01") + time_only
  ) %>%
  na.omit()

edited_npar_analysis3 <- npar_analysis3 %>%
  group_by(date = as.Date(time)) %>%
  filter(n_distinct(hour(time)) == 24) %>%
  ungroup()

npar_plot_data3 <- edited_npar_analysis3 %>%
  group_by(date = as.Date(time), hour) %>%
  summarise(ENMO = mean(ENMO)) 

npar_plot_data3 <- npar_plot_data3 %>%
  group_by(hour) %>%
  summarise(ENMO = mean(ENMO)) 


# Plot the data for participant 62189 (depression)
plot_1 <- ggplot(edited_npar_analysis, aes(x = plot_time, y = ENMO)) +
  geom_line() +
  facet_grid(date ~ ., scales = "free_y") +
  labs(title = "Depression (62189)", x = "Time (clock hour)", y = NULL) +
  theme(text = element_text(size=20), 
        strip.text.y.right = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1)) +
  scale_x_datetime(
    breaks = scales::date_breaks("2 hours"),
    labels = scales::time_format("%H:%M"),
    expand = c(0,0)
  )

# Plot the data for participant 67442 (healthy)
plot_2 <- ggplot(edited_npar_analysis2, aes(x = plot_time, y = ENMO)) +
  geom_line() +
  facet_grid(date ~ ., scales = "free_y") +
  labs(title = "Healthy controls (67442)", x = "Time (clock hour)", y = "ENMO_t (g)") +
  theme(text = element_text(size=20),
        strip.text.y.right = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1)) +
  scale_x_datetime(
    breaks = scales::date_breaks("2 hours"),
    labels = scales::time_format("%H:%M"),
    expand = c(0,0)
  )

# Plot the data for participant 63033 (sleep disturbances)
plot_3 <- ggplot(edited_npar_analysis3, aes(x = plot_time, y = ENMO)) +
  geom_line() +
  facet_grid(date ~ ., scales = "free_y") +
  labs(title = "Sleep disturbances (63033)", x = "Time (clock hour)", y = NULL) +
  theme(text = element_text(size=20),
        strip.text.y.right = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1)) +
  scale_x_datetime(
    breaks = scales::date_breaks("2 hours"),
    labels = scales::time_format("%H:%M"),
    expand = c(0,0)
  )

# Combine the plots from all three participants
combined_plot <- plot_2 + plot_1 + plot_3 + plot_layout(nrow =1) +
  plot_annotation(title = "Daily actigraphy for three different participants",
                  theme = theme(plot.title = element_text(size = 30)))

# Plot the summarized data for participant 62189 (depression)
npar_1 <- ggplot(npar_plot_data, aes(x = hour, y = ENMO)) +
  geom_col() +
  labs(title = "Depression (62189)", x = "Time (start: 00:00)", y = NULL)  +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

# Plot the summarized data for participant 67442 (healthy)
npar_2 <- ggplot(npar_plot_data2, aes(x = hour, y = ENMO)) +
geom_col() +
  labs(title = "Healthy controls (67442)", x = "Time (start: 00:00)", y = "Movement Intensity")  +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

# Plot the summarized data for participant 63033 (sleep disturbances)
npar_3 <- ggplot(npar_plot_data3, aes(x = hour, y = ENMO)) +
  geom_col() +
  labs(title = "Sleep disturbances (63033)", x = "Time (start: 00:00)", y = NULL)  +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

# Combine the summarized plots from all three participants
combined_plot2 <- npar_2 + npar_1 + npar_3 + plot_layout(nrow =1) +
  plot_annotation(
    title = "Actigraphy plot, average across days for three different participants",
    theme = theme(plot.title = element_text(size = 30)))

# Read participant information
output <- read_csv("Bachelor Thesis/Processed/output.csv")
output <- output %>%
  mutate(
    group = case_when(
      sleep_problems == 1 & depression == 1 ~ "Both",
      sleep_problems == 1 & depression == 0 ~ "Sleep disturbances",
      sleep_problems == 0 & depression == 1 ~ "Depression",
      TRUE ~ "Healthy controls"
    )
  )

# Make three different lists of data based on groups
depression_data <- list()
sleep_disturbances_data <- list()
both_data <- list()
healthy_controls_data <- list()

# Read .csv files and summarize data
for (file in dir_ls("Bachelor Thesis/Summarized Data")){
  
  # Get SEQN from .csv file
  seqn <- as.numeric(gsub("[^0-9]", "", basename(file)))
  
  # Pull group from SEQN
  group <- output %>%
    filter(SEQN == seqn) %>%
    pull(group)
    
  # Read and process data
  summary_result <- read_csv(file)
  
  npar_analysis <- summary_result %>%  
    mutate(
      time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ"),
      ENMO = as.numeric(ENMO_t),
      hour = hour(time),
      minute = minute(time),
      time_only = hms(format(time, "%H:%M:%S")),
      plot_time = as.POSIXct("1970-01-01") + time_only
    ) %>%
    na.omit()
  
  edited_npar_analysis <- npar_analysis %>%
    group_by(date = as.Date(time)) %>%
    filter(n_distinct(hour(time)) == 24) %>%
    ungroup()
  
  npar_plot_data <- edited_npar_analysis %>%
    group_by(date = as.Date(time), hour) %>%
    summarise(ENMO = mean(ENMO)) 
  
  npar_plot_data <- npar_plot_data %>%
    group_by(hour) %>%
    summarise(ENMO = mean(ENMO)) 
  
  # Put summarized data in the right group
  if (group == "Both"){
    both_data[[file]] <- npar_plot_data}
  else if (group == "Depression"){
    depression_data[[file]] <- npar_plot_data}
  else if (group == "Sleep disturbances"){
    sleep_disturbances_data[[file]] <- npar_plot_data}
  else{
    healthy_controls_data[[file]] <- npar_plot_data}
}
 
# Summarize the data from each group
healthy_df <- bind_rows(healthy_controls_data)
summary_healthy <- healthy_df %>%
  group_by(hour) %>%
  summarise(mean(ENMO))

depression_df <- bind_rows(depression_data)
summary_depression <- depression_df %>%
  group_by(hour) %>%
  summarise(mean(ENMO))

sleep_df <- bind_rows(sleep_disturbances_data)
summary_sleep <- sleep_df %>%
  group_by(hour) %>%
  summarise(mean(ENMO))

both_df <- bind_rows(both_data)
summary_both <- both_df %>%
  group_by(hour) %>%
  summarise(mean(ENMO))

# Plot the groups
healthy_plot <- ggplot(summary_healthy, aes(x = hour, y = `mean(ENMO)`)) +
  geom_col() +
  labs(title = "Healthy controls", x = NULL, y = "Movement intensity")  +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

depression_plot <- ggplot(summary_depression, aes(x = hour, y = `mean(ENMO)`)) +
  geom_col() +
  labs(title = "Depression", x = NULL, y = NULL)  +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

sleep_plot <- ggplot(summary_sleep, aes(x = hour, y = `mean(ENMO)`)) +
  geom_col() +
  labs(title = "Sleep disturbances", x = "Time (start: 00:00)", y = "Movement intensity")  +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

both_plot <- ggplot(summary_both, aes(x = hour, y = `mean(ENMO)`)) +
  geom_col() +
  labs(title = "Both", x = "Time (start: 00:00)", y = NULL)  +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

# Combine the summarized plots from all four groups
combined_plot3 <- (healthy_plot + depression_plot)/(sleep_plot + both_plot) +
  plot_annotation(
    title = "Actigraphy plot, average across days for all participants grouped by outcome",
    theme = theme(plot.title = element_text(size = 30)))
