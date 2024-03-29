# 1. Install and load the ISLR package (if not already installed)
# install.packages(c("crayon", "ISLR", "ggplot2", "boot", "dplyr"))

# Load the required libraries
library(ISLR)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(crayon)

# Load the "Default" dataset
data("Default", package = "ISLR")

# Store the Default dataset in a dataframe
default_df <- Default

# Display the first few rows of the dataframe
head(default_df)

# Data Processing:
# Check the structure of the Default dataset
str(Default)

# Check for missing and infinite values
missing_values_income <- sum(is.na(Default$income))
missing_values_balance <- sum(is.na(Default$balance))
infinite_values_income <- sum(!is.finite(Default$income))
infinite_values_balance <- sum(!is.finite(Default$balance))


# Print the number of missing and infinite values
cat("Number of missing values in the data: ", missing_values_income, "\n")
cat("Number of infinite values in the data: ", infinite_values_income, "\n")
cat("Number of missing values in the data: ", missing_values_balance, "\n")
cat("Number of infinite values in the data: ", infinite_values_balance, "\n")

# Check summary statistics for the balance column
summary(Default)

# Box plot: Income
plot_income <- ggplot(Default, aes(y = income)) +
  geom_boxplot(width = 0.2, fill = "skyblue", color = "darkblue") +
  labs(title = "Box Plot of Income", y = "Income") +
  theme_minimal() 
  
# Box plot: Balance
plot_balance <- ggplot(Default, aes(y = balance)) +
  geom_boxplot(width = 0.5, fill = "skyblue", color = "darkblue") +
  labs(title = "Box Plot of Balance", y = "Balance") +
  theme_minimal()

# arrange plot side-by-side
grid.arrange(plot_income, plot_balance, ncol = 2)

# Scatter plot: Default ~ Income
ggplot(Default, aes(x = income, y = default, color = default)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of Default ~ Income") +
  theme_minimal()

# Scatter plot: Default ~ Balance
ggplot(Default, aes(x = balance, y = default, color = default)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of Default ~ Balance") +
  theme_minimal()

# Scatter plot: Default ~ Income + Balance
ggplot(Default, aes(x = income, y = balance, color = default)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of Default ~ Income + Balance", x = "income", y = "balance", color = "default") +
  theme_minimal()

# Task 1: Logistic regression using glm:
# Fit logistic regression models

model_income <- glm(default ~ income, data = Default, family = "binomial")
model_balance <- glm(default ~ balance, data = Default, family = "binomial")
logistic_model <- glm(default ~ income + balance, data = Default, family = "binomial")

# Display summary of the models
summary_income <- summary(model_income)
summary_balance <- summary(model_balance)
summary_logistic <- summary(logistic_model)

# Create a table for summary results
summary_table <- data.frame(
  Model = c("Income", "Balance", "Income + Balance"),
  Deviance = c(summary_income$deviance, summary_balance$deviance, summary_logistic$deviance),
  AIC = c(AIC(model_income), AIC(model_balance), AIC(logistic_model)),
  BIC = c(BIC(model_income), BIC(model_balance), BIC(logistic_model))
)

# Print the summary table
print("Summary Results:")
print(summary_table)


# Task 1.1: Logistic Regression Curve:

# Install and load the pROC library if not already installed
# install.packages("pROC")
library(pROC)

# Function to plot ROC curve
plot_roc_curve <- function(predicted_probs, observed_values, title) {
  roc_curve <- roc(observed_values, predicted_probs)
  auc_value <- auc(roc_curve)
  
  ggplot(roc_curve, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line(color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    labs(title = paste("ROC Curve -", title, "\nAUC =", round(auc_value, 3)),
         x = "1 - Specificity (False Positive Rate)",
         y = "Sensitivity (True Positive Rate)") +
    theme_minimal()
}

# Check levels of the response variable
table(Default$default)

# Convert response variable to a binary outcome (assuming "No" as the reference level)
Default$binary_default <- as.factor(ifelse(Default$default == "Yes", 1, 0))

# Logistic regression curve with ROC curve:

predicted_probs_income <- predict(model_income, type = "response") #for Default ~ Income 
predicted_probs_balance <- predict(model_balance, type = "response") #for Default ~ Balance
predicted_probs_income_balance <- predict(logistic_model, type = "response") #for Default ~ Income + Balance

# Plot ROC curve
roc_curve_income_data <- roc(Default$binary_default, predicted_probs_income)
roc_curve_balance_data <- roc(Default$binary_default, predicted_probs_balance)
roc_curve_income_balance_data <- roc(Default$binary_default, predicted_probs_income_balance)

# Plot ROC curve
plot(roc_curve_income_data, col = "blue", main = "ROC Curve - Default ~ Income", col.main = "darkblue")
plot(roc_curve_balance_data, col = "blue", main = "ROC Curve - Default ~ Balance", col.main = "darkblue")
plot(roc_curve_income_balance_data, col = "blue", main = "ROC Curve - Default ~ Income + Balance", col.main = "darkblue")


# Task 2: Interpretation of the Logistic models:

# Model Validation

# 1. Install and load the required packages
# install.packages(c("ISLR", "ggplot2", "pROC", "dplyr", "boot"))

# Load the required libraries
library(ISLR)
library(ggplot2)
library(pROC)
library(dplyr)
library(boot)

# Load the "Default" dataset
data("Default", package = "ISLR")

# Task 1: Logistic regression using glm
model_income <- glm(default ~ income, data = Default, family = "binomial")
model_balance <- glm(default ~ balance, data = Default, family = "binomial")
logistic_model <- glm(default ~ income + balance, data = Default, family = "binomial")

# Display summary of the models
summary_income <- summary(model_income)
summary_balance <- summary(model_balance)
summary_logistic <- summary(logistic_model)

# Create a table for summary results
summary_table <- data.frame(
  Model = c("Income", "Balance", "Income + Balance"),
  Deviance = c(summary_income$deviance, summary_balance$deviance, summary_logistic$deviance),
  AIC = c(AIC(model_income), AIC(model_balance), AIC(logistic_model)),
  BIC = c(BIC(model_income), BIC(model_balance), BIC(logistic_model))
)

# Print the summary table
print("Summary Results:")
print(summary_table)


# Task 2: Interpretation of the Logistic models
coefficients_income <- coef(model_income)
odds_ratios_income <- exp(coefficients_income)

coefficients_balance <- coef(model_balance)
odds_ratios_balance <- exp(coefficients_balance)

coefficients_income_balance <- coef(logistic_model)
odds_ratios_income_balance <- exp(coefficients_income_balance)

# Create a table for interpretation results
interpretation_table <- data.frame(
  Variable = c("Income", "Balance", "Income + Balance"),
  Coefficients = c(coefficients_income[2], coefficients_balance[2], coefficients_income_balance[2]),
  Odds_Ratios = c(odds_ratios_income[2], odds_ratios_balance[2], odds_ratios_income_balance[2])
)

# Print the interpretation table
print("Interpretation Results:")
print(interpretation_table)

# Task 2.1: Visualization of Model Validation:
# Create dataframes for plotting

df_income <- data.frame(predictor = "income",
                        odds_ratio = odds_ratios_income,
                        lower_ci = exp(confint(model_income)[, 1]),
                        upper_ci = exp(confint(model_income)[, 2]))

df_balance <- data.frame(predictor = "balance",
                         odds_ratio = odds_ratios_balance,
                         lower_ci = exp(confint(model_balance)[, 1]),
                         upper_ci = exp(confint(model_balance)[, 2]))

df_income_balance <- data.frame(predictor = "income + balance",
                                odds_ratio = odds_ratios_income_balance,
                                lower_ci = exp(confint(logistic_model)[, 1]),
                                upper_ci = exp(confint(logistic_model)[, 2]))

# Combine dataframes
df_plot <- rbind(df_income, df_balance, df_income_balance)

# Plotting
ggplot(df_plot, aes(x = predictor, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, fill = predictor)) +
  geom_col(position = "dodge", width = 0.5, color = "black") +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.25, color = "black") +
  labs(title = "Odds Ratios and Confidence Intervals", y = "Odds Ratio", fill = "Predictor") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Task 3: Bootstrapping method for logistic regression analysis:

# Load the required packages
library(boot)

# Create a function to obtain bootstrap coefficients
bootstrap_func <- function(data, indices, model_formula) {
  sampled_data <- data[indices, ]
  fit <- glm(formula(model_formula), data = sampled_data, family = "binomial")
  return(coef(fit))
}

# Set the seed for reproducibility
set.seed(123)

# Number of bootstrap samples
num_boot_samples <- 1000

# Income model
bootstrap_results_income <- boot(
  data = Default,
  statistic = bootstrap_func,
  R = num_boot_samples,  # Increase the number of bootstrap samples
  model = 'default ~ income'
)

# Calculate bootstrap confidence intervals
boot_ci_income <- boot.ci(bootstrap_results_income, type = "perc")


# Balance model
bootstrap_results_balance <- boot(
  data = Default,
  statistic = bootstrap_func,
  R = num_boot_samples,  # Increase the number of bootstrap samples
  model = 'default ~ balance'
)

# Calculate bootstrap confidence intervals
boot_ci_balance <- boot.ci(bootstrap_results_balance, type = "perc")

# Income + Balance model
bootstrap_results_income_balance <- boot(
  data = Default,
  statistic = bootstrap_func,
  R = num_boot_samples,  # Increase the number of bootstrap samples
  model = 'default ~ income + balance'
)

# Calculate bootstrap confidence intervals
boot_ci_income_balance <- boot.ci(bootstrap_results_income_balance, type = "perc")


# Create a table for bootstrap results
bootstrap_table <- data.frame(
  Model = c("Income", "Balance", "Income + Balance"),
  Lower_CI = c(boot_ci_income$perc[4], boot_ci_balance$perc[4], boot_ci_income_balance$perc[4]),
  Upper_CI = c(boot_ci_income$perc[5], boot_ci_balance$perc[5], boot_ci_income_balance$perc[5])
)

# Print the bootstrap results table
print("\nBootstrap Confidence Intervals:")
print(bootstrap_table)


# Task 3.2: Logistic Regression Curve after bootstrapping:

# Function to plot ROC curve after bootstrapping
plot_roc_curve_bootstrap <- function(predicted_probs_vector, labels, model_name) {
  # Check if predicted_probs_vector is a numeric vector
  if (!is.numeric(predicted_probs_vector)) {
    stop("'predicted_probs_vector' must be a numeric vector.")
  }
  
  # Calculate mean ROC curve
  mean_roc_curve <- pROC::roc(labels, predicted_probs_vector)
  auc_value <- pROC::auc(mean_roc_curve)
  
  # Plot mean ROC curve
  plot(mean_roc_curve, col = "blue", main = paste("Mean ROC Curve -", model_name, "\nAUC =", round(auc_value, 3)))
}

# Example usage:

# Plot ROC curve after bootstrapping for Default ~ Income
roc_curve_bootstrap_income <- plot_roc_curve_bootstrap(
  predicted_probs_income,
  labels = Default$default,
  model_name = "Income"
)

# Plot ROC curve after bootstrapping for Default ~ Balance
roc_curve_bootstrap_balance <- plot_roc_curve_bootstrap(
  predicted_probs_balance,
  labels = Default$default,
  model_name = "Balance"
)

# Plot ROC curve after bootstrapping for Default ~ Income + Balance
roc_curve_bootstrap_income_balance <- plot_roc_curve_bootstrap(
  predicted_probs_income_balance,
  labels = Default$default,
  model_name = "Income + Balance"
)

# Display the plots
print(roc_curve_bootstrap_income)
print(roc_curve_bootstrap_balance)
print(roc_curve_bootstrap_income_balance)


# Task 3.3: Compare before and after bootstrap results:

# Visualize the differences of before and after bootstrapping:
# Extract original coefficients

original_coefficients_income <- coef(model_income)
original_coefficients_balance <- coef(model_balance)
original_coefficients_income_balance <- coef(logistic_model)

# Combine coefficients and bootstrap results
df_comparison_income <- data.frame(
  variable = c(names(original_coefficients_income), rep("income", length(boot_ci_income$percent))),
  value = c(original_coefficients_income, rep(NA, length(boot_ci_income$percent)))
)

df_comparison_balance <- data.frame(
  variable = c(names(original_coefficients_balance), rep("balance", length(boot_ci_balance$percent))),
  value = c(original_coefficients_balance, rep(NA, length(boot_ci_balance$percent)))
)

df_comparison_income_balance <- data.frame(
  variable = c(names(original_coefficients_income_balance), rep(c("income", "balance"), each = length(boot_ci_income_balance$percent))),
  value = c(original_coefficients_income_balance, rep(NA, 2 * length(boot_ci_income_balance$percent)))
)

# Fill in the rows corresponding to bootstrap results
df_comparison_income$value[length(names(original_coefficients_income)) + 1:length(boot_ci_income$percent)] <- boot_ci_income$percent
df_comparison_balance$value[length(names(original_coefficients_balance)) + 1:length(boot_ci_balance$percent)] <- boot_ci_balance$percent

# Fill in the rows corresponding to bootstrap results for income
df_comparison_income_balance$value[1:length(original_coefficients_income_balance)] <- original_coefficients_income_balance
df_comparison_income_balance$value[length(original_coefficients_income_balance) + 1:length(boot_ci_income_balance$percent)] <- boot_ci_income_balance$percent

# Fill in the rows corresponding to bootstrap results for balance
df_comparison_income_balance$value[length(original_coefficients_income_balance) + length(boot_ci_income_balance$percent) + 1: length(boot_ci_income_balance$percent)] <- NA

# Plotting
ggplot(df_comparison_income, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Coefficients and Bootstrap Results - Default ~ Income") +
  theme_minimal()

ggplot(df_comparison_balance, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge")
