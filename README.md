# Bachelor-Thesis

# Filter.R: A script that filters NHANES data. The output is a list and vector of the filtered SEQNs (Participant IDs).
# PopulationCharacteristics.R: A script used to make the population characteristics table. This calculates the mean + inter-quartile range or sum + percentages of the included features.
# Significance.R: A script that compares circadian rhythm features between outcome groups and performs a statistical t-test to assess those differences.

# For the processing of accelerometer data, the following scripts were used:
# SummaryRawData.R: A script that unzips and summarizes raw actigraphy files (from the NHANES).
# CircadianFeatureExtraction.R: A script that processes the summarized actigraphy files that came from the SummaryRawData.R script. The script calculates cosinor features and circadian rhythm features.
# DataMerge.R: A script that merges all questionnaire data and circadian rhythm features together into one single file.

# For visualization purposes, the following script was used:
# Visualisation_OneFile.R: Does what SummaryRawData.R and CircadianFeatureExtraction.R do, but also plots actigraphy data throughout the script and compares several participants to eachother.

# For the machine learning approach, the following four scripts were used (which are almost the same):
# MachineLearning_Depression_Primary.R: For predicting depression in the primary cohort.
# MachineLearning_Depression_Sensitivity.R: For predicting depression in the sensitivity cohort.
# MachineLearning_Sleep_Primary.R: For predicting sleep disturbances in the primary cohort.
# MachineLearning_Sleep_Sensitivity.R: For predicting sleep disturbances in the sensitvity cohort.

