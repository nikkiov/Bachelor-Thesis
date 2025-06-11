# Script that uses several machine learning algorithms to predict sleep disturbances in the sensitivity cohort based on several circadian and covariate features.
# -------------------------------------
# This script uses several machine learning algorithms and preprocessing steps to predict sleep disturbances in the training dataset.
# By using workflowsets, multiple models and preprocessing steps can be compared, after which the best can be chosen based on key performance metrics,
# also calculated in this script.
# By using vip, the most important predictors for predicting sleep disturbances using a certain machine learning model can be determined.
# This script also calibrates the best performing machine models to make the results more reliable.
# Finally, this script also removes equivocal zones from the best performing model in order to improve key performance metrics.
# -------------------------------------

library(workflowsets)
library(workflows)
library(modeldata)
library(recipes)
library(parsnip)
library(dplyr)
library(rsample)
library(tune)
library(yardstick)
library(themis)
library(tidyverse)
library(ggplot2)
library(vip)
library(patchwork)
library(probably)
library(klaR)
library(discrim)

# Read data and change the format of the outcomes
circadian_data_percentile <- read_csv("Bachelor Thesis/Processed/population_percentile.csv") %>%
  mutate(depression = factor(depression, levels = c(1,0), labels = c("Depression", "No_depression")),
         sleep_problems = factor(sleep_problems, levels = c(1,0), labels = c("Sleep_disturbances", "No_sleep_disturbances")))

# Change labels of the used features
labels <- c("age" = "Age", "gender" = "Sex", "employed" = "Employment", "race" = "Race", "education" = "Education", "no_light_heavy_drinker" = "Alcohol consumption", "heavy_smoker_yes_no" = "Smoking behaviour", "household_size" = "Household size")

# Change the numbers to words to make it more clear
circadian_data_plot <- circadian_data_percentile %>%
  pivot_longer(
    cols = c(BMI, gender, employed, race, education, household_size, no_light_heavy_drinker, heavy_smoker_yes_no),
    names_to = "variable",
    values_to = "value"
  )
circadian_data_plot <- circadian_data_plot %>%
  mutate(
    value_label = case_when(
      variable == "BMI" & value == 0 ~ "Underweight",
      variable == "BMI" & value == 1 ~ "Normal",
      variable == "BMI" & value == 2 ~ "Overweight",
      variable == "BMI" & value == 3 ~ "Obese",
      variable == "gender" & value == 1 ~ "Male",
      variable == "gender" & value == 2 ~ "Female",
      variable == "education" & value == 0 ~ "Low educated",
      variable == "education" & value == 1 ~ "Highly educated",
      variable == "employed" & value == 0 ~ "Unemployed",
      variable == "employed" & value == 1 ~ "Employed",
      variable == "heavy_smoker_yes_no" & value == 0 ~ "Non-smoker",
      variable == "heavy_smoker_yes_no" & value == 1 ~ "Smoker",
      variable == "no_light_heavy_drinker" & value == 0 ~ "No drinker",
      variable == "no_light_heavy_drinker" & value == 1 ~ "Light drinker",
      variable == "no_light_heavy_drinker" & value == 2 ~ "Heavy drinker",
      variable == "household_size" & value == 0 ~ "Small",
      variable == "household_size" & value == 1 ~ "Medium",
      variable == "household_size" & value == 2 ~ "Large",
      variable == "race" & value == 1 ~ "Mexican",
      variable == "race" & value == 2 ~ "Hispanic",
      variable == "race" & value == 3 ~ "White",
      variable == "race" & value == 4 ~ "Black",
      variable == "race" & value == 5 ~ "Mexican",
      variable == "race" & value == 6 ~ "Asian",
      variable == "race" & value == 7 ~ "Other"
    )
  )

# Plot the covariates (except age) as bar plots, together in one big plot
circadian_data_plot %>%
  ggplot(aes(x = value_label, fill = factor(sleep_problems))) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_manual(
    values = c("No_sleep_disturbances" = "skyblue", "Sleep_disturbances" = "tomato"),
    labels = c("No_sleep_disturbances" = "Healthy controls", "Sleep_disturbances" = "Sleep disturbances"),
    name = "Group"
  ) +
  facet_wrap(~ variable, scales = "free", ncol = 4, nrow = 2, labeller = labeller(variable = labels)) +
  labs(fill = "Sleep_disturbances", x = NULL, y = "Frequency", title = "Bar plots of covariates by sleep disturbance group (sensitivity dataset)") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(size = 30),
        legend.position = "bottom")

# Plot the age as a density plot
circadian_data_percentile %>%
  pivot_longer(age) %>%
  ggplot(aes(x = value, fill = factor(sleep_problems))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(
    values = c("No_sleep_disturbances" = "skyblue", "Sleep_disturbances"= "tomato"),
    labels = c("No_sleep_disturbances" = "Healthy controls", "Sleep_disturbances" = "Sleep disturbances"),
    name = "Group"
  ) +
  facet_wrap(~ name, labeller = labeller(name = labels)) +
  labs(fill = "Sleep_disturbances", x = NULL, y = "Density", title = "Density plot of covariate age by sleep disturbance group (sensitivity dataset)") +
  theme(text = element_text(size = 30),
        axis.text.y = element_text(size = 27),
        axis.text.x = element_text(size = 27),
        plot.title = element_text(size = 30),
        legend.position = "bottom")

# Plot the circadian rhythm features as density plots, with new labels
labels <- c("corrected_rad_acro" = "Acrophase (radians)", "amplitude" = "Amplitude", "mesor" = "MESOR")
circadian_data_percentile %>%
  pivot_longer(c(IV, RA, IS, mesor, amplitude, corrected_rad_acro)) %>%
  ggplot(aes(x = value, fill = factor(sleep_problems))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(
    values = c("No_sleep_disturbances" = "skyblue", "Sleep_disturbances" = "tomato"),
    labels = c("No_sleep_disturbances" = "Healthy controls", "Sleep_disturbances" = "Sleep disturbances"),
    name = "Group"
  ) +
  facet_wrap(~ name, scales = "free", labeller = labeller(name = labels)) +
  labs(fill = "Sleep_disturbances", x = NULL, y = "Density", title = "Density plots of circadian features by sleep disturbance group (sensitivity dataset)") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(size = 30),
        legend.position = "bottom")

# Set seed in order to create reproducible results
# With thanks to: https://juliasilge.com/blog/ikea-prices/
set.seed(1231)

# Split the data based on sleep disturbances strata, on a 80/20 ratio and create a training and testing dataset
initial_split_sleep_sensitivity <- initial_split(circadian_data_percentile, 
                               prop = 0.80, 
                               strata = sleep_problems)
train_data_sleep_sensitivity <- training(initial_split_sleep_sensitivity)
test_data_sleep_sensitivity <- testing(initial_split_sleep_sensitivity)

# Save all data to files for later access if needed
saveRDS(initial_split_sleep_sensitivity, file="Bachelor Thesis/Processed/initial_split_sleep_sensitivity.rds")
saveRDS(train_data_sleep_sensitivity, file="Bachelor Thesis/Processed/train_data_sleep_sensitivity.rds")
saveRDS(test_data_sleep_sensitivity, file="Bachelor Thesis/Processed/test_data_sleep_sensitivity.rds")

train_data_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/train_data_sleep_sensitivity.rds")
test_data_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/test_data_sleep_sensitivity.rds")

# Make the recipes: different features, different over_ratios and different interaction terms
basic_recipe_sleep_sensitivity <-
  recipe(sleep_problems ~ IS + IV + RA + amplitude + corrected_rad_acro + mesor + age + gender + race + education + household_size + no_light_heavy_drinker + heavy_smoker_yes_no + BMI + employed, data = train_data_sleep_sensitivity) %>%
  step_smote(sleep_problems, over_ratio = 1)

interact1_recipe_sleep_sensitivity <-
  basic_recipe_sleep_sensitivity %>%
  step_interact(~ mesor:amplitude + no_light_heavy_drinker:heavy_smoker_yes_no:BMI + IS:IV:RA)

interact2_recipe_sleep_sensitivity <-
  recipe(sleep_problems ~ IS + IV + RA + amplitude + corrected_rad_acro + mesor, data = train_data_sleep_sensitivity) %>%
  step_smote(sleep_problems, over_ratio = 1) %>%
  step_interact(~ mesor:amplitude)

interact3_recipe_sleep_sensitivity <-
  basic_recipe_sleep_sensitivity %>%
  step_interact(~ no_light_heavy_drinker:heavy_smoker_yes_no)

interact4_recipe_sleep_sensitivity <-
  basic_recipe_sleep_sensitivity %>%
  step_interact(~ no_light_heavy_drinker:BMI)

interact5_recipe_sleep_sensitivity <-
  basic_recipe_sleep_sensitivity %>%
  step_interact(~ IS:IV)

interact6_recipe_sleep_sensitivity <-
  recipe(sleep_problems ~ IS + IV + RA + amplitude + corrected_rad_acro + mesor, data = train_data_sleep_sensitivity) %>%
  step_smote(sleep_problems, over_ratio = 1) %>%
  step_interact(~ IS:IV + mesor:amplitude)

interact7_recipe_sleep_sensitivity <-
  recipe(sleep_problems ~ IS + IV + RA + amplitude + corrected_rad_acro + mesor, data = train_data_sleep_sensitivity) %>%
  step_smote(sleep_problems, over_ratio = 0.6) %>%
  step_interact(~ mesor:amplitude)

interact8_recipe_sleep_sensitivity <-
  basic_recipe_sleep_sensitivity %>%
  step_smote(sleep_problems, over_ratio = 0.6) %>%
  step_interact(~ IS:IV)

# Define a k-nearest neighbors model, set to classification
knn_mod <-
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")

# Define a logistic regression model
lr_mod <-
  logistic_reg() %>%
  set_engine("glm")

# Define a random forest model with 1000 trees, set to classification
rf_mod <- rand_forest(
  mtry = tune(),    
  min_n = tune(),   
  trees = 1000    
) %>%
  set_mode("classification") %>%
  set_engine("randomForest")

# Make lists of the different preprocessing steps and the models
# With thanks to: https://workflowsets.tidymodels.org/reference/workflow_map.html
prepoc_sleep_sensitivity <- list(none = basic_recipe_sleep_sensitivity, interact1 = interact1_recipe_sleep_sensitivity, interact2 = interact2_recipe_sleep_sensitivity, interact3 = interact3_recipe_sleep_sensitivity, interact4 = interact4_recipe_sleep_sensitivity, interact5 = interact5_recipe_sleep_sensitivity, interact6 = interact6_recipe_sleep_sensitivity, interact7 = interact7_recipe_sleep_sensitivity, interact8 = interact8_recipe_sleep_sensitivity)
models <- list(knn = knn_mod, logistic = lr_mod, rf = rf_mod)

# Make an interface for investigating multiple models and preprocessing steps
cell_set_sleep_sensitivity <- workflow_set(prepoc_sleep_sensitivity, models, cross = TRUE)

# Execute the same function across all workflows and evaluates them on the accuracy, precision, recall, F1-score and specificity. 
# It also extracts the underlying model object from each model for later use.
results_sleep_sensitivity <- cell_set_sleep_sensitivity %>%
  workflow_map(
    resamples = vfold_cv(train_data_sleep_sensitivity, strata = sleep_problems),
    grid = 15,
    verbose = TRUE,
    metrics = metric_set(accuracy, precision, recall, f_meas, specificity),
    control = control_grid(extract = extract_fit_engine)
  )

# Save results and read results for later access, if needed
saveRDS(results_sleep_sensitivity, file="Bachelor Thesis/Processed/workflow_sleep_sensitivity.rds")
results_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/workflow_sleep_sensitivity.rds")

# Rank and select the best models based on the F1-score
ranked_results_sleep_sensitivity <- results_sleep_sensitivity %>%
  rank_results(rank_metric = "f_meas", select_best = TRUE) %>%
  filter(.metric == "f_meas") 

# Get the workflow ids of the best performing models
best_wf_id_sleep_sensitivity <- ranked_results_sleep_sensitivity$wflow_id
my_metrics <- metric_set(f_meas, accuracy, specificity, precision, recall)

conf_sleep_sensitivity <- vector()
metrics_sleep_sensitivity <- list()
roc_sleep_sensitivity <- list()
vipp_sleep_sensitivity <- list()
final_fit_list_sleep_sensitivity <- list()
resample_list_sleep_sensitivity <- list()
wf_list_sleep_sensitivity <- list()

# Loop over all best performing models
for (id in best_wf_id_sleep_sensitivity){
  
  # Finalize the workflow with the best F1-score and add it to a list
  final_wf <- results_sleep_sensitivity %>%
    extract_workflow(id) %>%
    finalize_workflow(
      results_sleep_sensitivity %>%
        extract_workflow_set_result(id) %>%
        select_best(metric = "f_meas"))
  wf_list_sleep_sensitivity[[id]] <- final_wf
  
  # Fit the finalized model on the entire training data set, evaluate it on the test data set and add it to a list
  final_fit_sleep_sensitivity <- final_wf %>%
    last_fit(split = initial_split_sleep_sensitivity)
  final_fit_list_sleep_sensitivity[[id]] <- final_fit_sleep_sensitivity
  
  # Collect predictions (F1-score, accuracy, specificity, recall, precision) on the final fit, make a confusion matrix and add it to a vector and list
  conf_sleep_sensitivity[[id]] <- collect_predictions(final_fit_sleep_sensitivity) %>%
    conf_mat(sleep_problems, .pred_class)
  metrics_sleep_sensitivity[[id]] <- collect_predictions(final_fit_sleep_sensitivity) %>%
    my_metrics(truth = sleep_problems, estimate = .pred_class) 
  
  # Make a ROC-curve on the final fit and add it to a list
  roc_sleep_sensitivity[[id]] <- collect_predictions(final_fit_sleep_sensitivity) %>% 
    roc_curve(sleep_problems, .pred_Sleep_disturbances)
  
  # Returns the parsnip model fit object and adds it to a list
  vipp_sleep_sensitivity[[id]] <- extract_fit_parsnip(final_fit_sleep_sensitivity$.workflow[[1]])
  
  # Computes performance metrics across multiple resamples (v-fold cross-validation) and add it to a list
  resample_list_sleep_sensitivity[[id]] <- fit_resamples(final_wf, 
                                                         resamples = vfold_cv(train_data_sleep_sensitivity, strata = sleep_problems), 
                                                         metrics = metric_set(accuracy, roc_auc), 
                                                         control = control_resamples(save_pred = TRUE))
}

# Save all lists/vector to files and read them for later access (if needed)
saveRDS(metrics_sleep_sensitivity, file="Bachelor Thesis/Processed/metrics_sleep_sensitivity.rds")
metrics_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/metrics_sleep_sensitivity.rds")

saveRDS(final_fit_list_sleep_sensitivity, file="Bachelor Thesis/Processed/final_fit_sleep_sensitivity.rds")
final_fit_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/final_fit_sleep_sensitivity.rds")

saveRDS(conf_sleep_sensitivity, file="Bachelor Thesis/Processed/conf_sleep_sensitivity.rds")
conf_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/conf_sleep_sensitivity.rds")

saveRDS(roc_sleep_sensitivity, file="Bachelor Thesis/Processed/roc_sleep_sensitivity.rds")
roc_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/roc_sleep_sensitivity.rds")

saveRDS(vipp_sleep_sensitivity, file="Bachelor Thesis/Processed/vip_sleep_sensitivity.rds")
vip_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/vip_sleep_sensitivity.rds")

saveRDS(resample_list_sleep_sensitivity, file="Bachelor Thesis/Processed/resample_list_sleep_sensitivity.rds")
resample_list_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/resample_list_sleep_sensitivity.rds")

saveRDS(wf_list_sleep_sensitivity, file="Bachelor Thesis/Processed/wf_list_sleep_sensitivity.rds")
wf_list_sleep_sensitivity_read <- readRDS("Bachelor Thesis/Processed/wf_list_sleep_sensitivity.rds")

# Plot the performance of the models across all key performance metrics
plot_data_sleep_sensitivity <- rank_results(results_sleep_sensitivity, rank_metric = "f_meas") %>%
  filter(.metric %in% c("recall", "specificity", "accuracy", "f_meas", "precision"))

model_performance_sleep_sensitivity <- ggplot(plot_data_sleep_sensitivity, aes(x = rank, y = mean, color = model)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
                width = 0.2, alpha = 0.4) +
  facet_wrap(~.metric, nrow = 2,
             labeller = labeller(.metric = c(
               "recall" = "Recall",
               "specificity" = "Specificity",
               "accuracy" = "Accuracy",
               "f_meas" = "F1-score",
               "precision" = "Precision"
             ))) +
  labs(
    title = "Model performance across evaluation metrics - Sleep disturbance, sensitivity dataset",
    x = "Workflow Rank",
    y = "Metric Mean",
    color = "Model Type"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
    panel.background = element_rect(fill = "grey90"),
    strip.background = element_rect(fill = "grey85", color = NA),
    text = element_text(size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 16),
    plot.title = element_text(size = 27)) +
  scale_color_discrete(
    labels = c("logistic_reg" = "Logistic Regression",
               "nearest_neighbor" = "K-Nearest Neighbors",
               "rand_forest" = "Random Forest"))

# Select the best performing models based on metrics_sleep_sensitivity
selected_models <- c("interact1_knn", "interact5_rf", "interact3_logistic", "interact6_logistic", "interact2_knn", "interact6_rf")
model_labels <- c(
  interact1_knn  = "K-nn, covariates", 
  interact5_rf = "Random forest, covariates", 
  interact3_logistic = "Logistic, covariates",
  interact2_knn = "K-nn, no covariates",
  interact6_rf = "Random forest, no covariates",
  interact6_logistic = "Logistic, no covariates"
)

# Use logistic regression calibration on the best performing model to measure performance with and without calibration
# With thanks to: www.tidymodels.org/learn/models/calibration/
log_val <- cal_validate_logistic(resample_list_sleep_sensitivity_read$interact3_logistic, 
                                      save_pred = TRUE, 
                                      times = 25)
cell_cal <- cal_estimate_logistic(resample_list_sleep_sensitivity_read$interact3_logistic)       # Calculate new probabilities
cal_fit <- wf_list_sleep_sensitivity_read$interact3_logistic %>% 
  fit(data = train_data_sleep_sensitivity_read)
cell_test_pred <- augment(cal_fit, 
                          new_data = test_data_sleep_sensitivity_read)
my_metrics <- metric_set(f_meas, accuracy, specificity, precision, recall)
cell_test_pred %>% my_metrics(truth = sleep_problems, 
                              estimate = .pred_class, 
                              .pred_Sleep_disturbances = .pred_Sleep_disturbances)
prob_metrics <- metric_set(roc_auc)
cell_test_pred %>% prob_metrics(truth = sleep_problems, 
                                .pred_Sleep_disturbances)
cell_test_cal_pred <- cell_test_pred %>%
  cal_apply(cell_cal)                                                            # Applies a calibration to a set of existing predictions
cell_test_cal_pred %>% dplyr::select(sleep_problems, starts_with(".pred_"))

# Calculate the new metrics after calibration
cell_test_cal_pred %>% my_metrics(truth = sleep_problems, estimate = .pred_class)
cell_test_cal_pred %>% prob_metrics(truth = sleep_problems, .pred_Sleep_disturbances)

# Plot the uncalibrated model
cell_plot_1 <- cal_plot_windowed(resample_list_sleep_sensitivity_read$interact3_logistic, step_size = 0.025) +
  labs(
    title = "Uncalibrated") +
  theme(
    text = element_text(size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 16))

# Plot the calibrated model
cell_plot_2 <- cell_test_cal_pred %>%
  cal_plot_windowed(truth = sleep_problems, estimate = .pred_Sleep_disturbances, step_size = 0.025) +
  labs(
    title = "Calibrated"
  ) +
  ylab(NULL) +
  theme(
    text = element_text(size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 16))

# Plot both models together
cell_plot_1 + cell_plot_2 +
  plot_annotation(
    title = "Isotonic regression calibration of the sleep disturbances model (sensitivity cohort)",
    theme = theme(plot.title = element_text(size = 30)))

# Collect predictions of the best performing model
# With thanks to: https://probably.tidymodels.org/articles/equivocal-zones.html
collect <- collect_predictions(final_fit_sleep_sensitivity_read$interact3_logistic)

# Convert class probability estimates to class_pred objects and use the normally used threshold
collect_thresh <- collect %>%
  mutate(.pred = make_two_class_pred(
    estimate = .pred_Sleep_disturbances,
    levels = levels(sleep_problems),
    threshold = 0.5
  ))

# Convert class probability estimates to class_pred objects and remove equivocal zones with a buffer of 0.15 around a threshold of 0.40 (best performance)
collect_pred <- collect %>%
  mutate(.pred = make_two_class_pred(
    estimate = .pred_Sleep_disturbances,
    levels = levels(sleep_problems),
    threshold = 0.40,
    buffer = 0.15
  ))

# Collect predictions and reportable rate of the model with removed equivocal zones
collect_pred %>%
  mutate(.pred_fct = as.factor(.pred)) %>%
  my_metrics(truth = sleep_problems, estimate = .pred_fct)

collect_pred %>%
  mutate(.pred_fct = as.factor(.pred)) %>%
  prob_metrics(truth = sleep_problems, .pred_Sleep_disturbances)

collect_pred %>% 
  summarise(reportable = reportable_rate(.pred))

collect_pred %>%
  mutate(.pred_fct = as.factor(.pred)) %>%
  count(.pred, .pred_fct)

# Collect predictions of the initially used model
collect_thresh %>%
  mutate(.pred_fct = as.factor(.pred)) %>%
  my_metrics(truth = sleep_problems, estimate = .pred_fct)

# Plot confusion matrices 
conf_df_sleep_sensitivity <- imap_dfr(conf_sleep_sensitivity_read[selected_models], ~ as_tibble(.x$table) %>% mutate(model = model_labels[.y]))
conf_df_sleep_sensitivity <- conf_df_sleep_sensitivity %>%
  mutate(Prediction = ifelse(Prediction == "No_sleep_disturbances", "No sleep disturbances", Prediction),
         Prediction = ifelse(Prediction == "Sleep_disturbances", "Sleep disturbances", Prediction),
         Truth = ifelse(Truth == "No_sleep_disturbances", "No sleep disturbances", Truth),
         Truth = ifelse(Truth == "Sleep_disturbances", "Sleep disturbances", Truth))
ggplot(conf_df_sleep_sensitivity, aes(x= fct_rev(Truth), y = Prediction, fill = n)) +
  geom_text(aes(label = n), size = 5) +
  facet_wrap(~ model) +
  labs(title = "Model performance - Sleep disturbance, sensitivity dataset") +
  xlab("Truth") +
  theme(text = element_text(size = 16))

# Calculate Area Under the Curve (AUC) values for the best performing models
auc_data_sleep_sensitivity <- c()
for (model_name in selected_models){
  model <- final_fit_sleep_sensitivity_read[[model_name]]
  preds <- collect_predictions(model)
  auc = roc_auc(preds, truth = sleep_problems, .pred_Sleep_disturbances) %>%
    dplyr::pull(.estimate)
  auc_data_sleep_sensitivity[[model_name]] <- auc
}

# Plot a variable importance plot (vip) of the best performing model
vip2 <- vip(vip_sleep_sensitivity_read$interact3_logistic, num_features = 20) +
  ggtitle("Sleep disturbances") +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

