library(sf)
library(haven)
library(dplyr)
library(units)
library(grf)
library(stargazer)
library(ggplot2)
library(caret)  # For nearZeroVar and findLinearCombos functions
library(grf)  # Generalized Random Forest library

data <- read_dta("ZimVACCombined.dta")
 
# Convert specific variables to numeric
numeric_vars <- c("hhh_female_d2", "hh_size", "hdds", "hhh_age", "lnproxy_income", "cattleyn", "donkeyyn", "sheepyn", "goatsyn", "pigsyn", "poultry_yn", "rabbitsyn")
data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)
 
# Prepare covariates by converting factors to dummy variables
covariates <- data[, !(names(data) %in% c("hdds", "marketing_sum_any", "year"))]
covariates <- model.matrix(~ . - 1, data = covariates)  # The "-1" removes the intercept term

# Remove rows with incomplete cases
data <- na.omit(data)

# Exclude specific variables
excluded_vars <- c("hhid", "marketinformation", "organisedmarketing", "farmerorganisation", 
                                           "marketdistribution", "marketing_sum_a2", "marketing_sum_a3", 
                                           "marketing_sum_a4", "proxy_income")
data <- data[, !names(data) %in% excluded_vars]

# Check the structure of the covariates matrix
str(covariates)

# Filter data for year 2022 for training
data_22 <- subset(data, year == 2024)
data_24 <- subset(data, year == 2024)

# Prepare for causal forest
outcome <- data_22$hdds  # Outcome variable
treatment <- data_22$marketing_sum_any  # Treatment variable
covariates <- data_22[, !(names(data_22) %in% c("hdds", "marketing_sum_any", "marketing_sum","year", "hh_id"))]

# Ensure treatment is a vector
treatment <- as.vector(data_22$marketing_sum_any)

# Ensure outcome is a vector
outcome <- as.vector(data_22$hdds)

# Convert treatment and outcome to numeric
treatment <- as.numeric(data_22$marketing_sum_any)
outcome <- as.numeric(data_22$hdds)

# Identify non-numeric or NA entries
table(is.na(treatment))  # Should show no missing values
table(is.na(outcome))    # Should show no missing values


# Train the causal forest model
causal_forest_model <- causal_forest(
        X = covariates,  # Numeric covariates matrix
        Y = outcome,    # Outcome variable (vector)
        W = treatment, # Treatment variable (vector) 
        honesty = TRUE
    )
average_treatment_effect(causal_forest_model)

treatment_effects <- predict(causal_forest_model, estimate.variance = TRUE)

# Filter data for year 2024
data_24 <- subset(data, year == 2024)

# Prepare covariates for test data, excluding outcome and treatment variables
covariates_24 <- data_24[, !(names(data_24) %in% c("hdds", "marketing_sum_any","marketing_sum", "year"))]

# Predict treatment effects
treatment_effects <- predict(causal_forest_model, newdata = covariates_24, estimate.variance = TRUE)

# Extract treatment effect estimates and variances
estimated_effects <- treatment_effects$predictions
estimated_variances <- treatment_effects$variance.estimates

# Add the treatment effects and variances to the 2024 data
data_24$estimated_effect <- estimated_effects
data_24$estimated_variance <- estimated_variances

# Calculate the average treatment effect
average_treatment_effect <- mean(estimated_effects)

# Calculate standard errors and p-values
data_24$standard_error <- sqrt(estimated_variances)/sqrt(n) ### IS THIS A BUG?
data_24$p_value <- 2 * pnorm(-abs(estimated_effects /data_24$standard_error ))  # Two-tailed test

# Calculate averages
average_cate <- mean(data_24$estimated_effect, na.rm = TRUE)
average_std_error <- mean(data_24$standard_error, na.rm = TRUE)
average_p_value <- mean(data_24$p_value, na.rm = TRUE)

# Determine significance stars based on the average p-value
average_stars <- ifelse(average_p_value < 0.01, "***",
                        ifelse(average_p_value < 0.05, "**",
                               ifelse(average_p_value < 0.1, "*", "")))

# Create the results data frame with average values
results <- data.frame(
  Model = "At Least One Marketing Practice",
  CATE = paste0(round(average_cate, 3), average_stars),
  Std_Error = paste0("(", round(average_std_error, 3), ")")
)

# Create a formatted table
stargazer(results, type = "text", summary = FALSE,
          title = "Causal Forest Results for Household Dietary Diversity Index",
          rownames = FALSE, digits = 3,
          notes = "Note: *p<0.1; **p<0.05; ***p<0.01")







