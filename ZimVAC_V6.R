###---A. Load and Clean-Up----
library(sf)
library(haven)
library(dplyr)
library(units)
library(grf)
library(stargazer)
library(ggplot2)
library(caret)  # For nearZeroVar and findLinearCombos functions
library(here)

# Load the data (change path accordingly)
data_24 <- read_dta(here("data/2024_ZimLAC_rural_SF.dta"))
# If needed, load data_22 similarly

# ------------------------------#
# 2024 Data Processing
# ------------------------------#

# Clean up age variable
data_24$hh_age[data_24$hh_age == 998] <- NA
data_24$hhh_age <- data_24$hh_age

# Create gender variables
data_24$hhh_sex <- data_24$hh_sex
data_24$hhh_female_d <- ifelse(data_24$hhh_sex == 1, 1, 0)

# Clean up marital status
data_24$hhh_marital <- data_24$hh_marital
data_24$hhh_marital[data_24$hhh_marital == 99] <- NA

# Exclude province_code from the analysis
# Do not create province_d or include province_code in the data
# data_24$province_d <- as.factor(data_24$province_code)

# Create religion variable
data_24$hhh_religion_d <- as.factor(data_24$hh_religion)

# Handle education level
data_24$hheduclevel[is.na(data_24$hheduclevel)] <- data_24$reseduclevel
data_24$hhh_education_d <- as.factor(data_24$hheduclevel)
data_24$hhh_educated <- ifelse(data_24$hhh_education_d == 1, 0, 1)

# Employment variable
data_24$hhh_employ <- data_24$hh_employ
data_24$hhh_employ_d <- as.factor(data_24$hhh_employ)

# Other variables
data_24$draughtpowercat_d <- as.factor(data_24$draughtpowercat)
data_24$lnproxy_income <- log(1 + data_24$proxy_income)

# Remove rows with missing lon or lat values
data_24_complete <- data_24[!is.na(data_24$lon) & !is.na(data_24$lat), ]

# Market information variables
if (!all(c('marketinformation', 'organisedmarketing', 'farmerorganisation', 'marketdistribution') %in% names(data_24_complete))) {
  data_24_complete$marketinformation <- ifelse(data_24_complete$MARKETING == 2, 1, 0)
  data_24_complete$organisedmarketing <- ifelse(data_24_complete$MARKETING == 3, 1, 0)
  data_24_complete$farmerorganisation <- ifelse(data_24_complete$MARKETING == 1, 1, 0)
  data_24_complete$marketdistribution <- ifelse(data_24_complete$MARKETING == 4, 1, 0)
}

# Convert to spatial object
df_sf <- st_as_sf(data_24_complete, coords = c("lon", "lat"), crs = 4326)

# Function to compute number of neighbors practicing each marketing practice within 10 km
compute_neighbors <- function(df_all, df_practice, dist) {
  d <- st_distance(df_all, df_practice)
  dist_units <- units::set_units(dist, "m")
  comparison <- d <= dist_units
  num_neighbors <- rowSums(comparison, na.rm = TRUE)
  return(num_neighbors)
}

# Compute the number of neighbors practicing each marketing practice within 10 km
df_sf$marketinformation_neighbors <- compute_neighbors(df_sf, df_sf[df_sf$marketinformation == 1, ], 10000)
df_sf$organisedmarketing_neighbors <- compute_neighbors(df_sf, df_sf[df_sf$organisedmarketing == 1, ], 10000)
df_sf$farmerorganisation_neighbors <- compute_neighbors(df_sf, df_sf[df_sf$farmerorganisation == 1, ], 10000)
df_sf$marketdistribution_neighbors <- compute_neighbors(df_sf, df_sf[df_sf$marketdistribution == 1, ], 10000)

# Combine results
df_sf$marketing_sum <- df_sf$marketinformation_neighbors +
  df_sf$organisedmarketing_neighbors +
  df_sf$farmerorganisation_neighbors +
  df_sf$marketdistribution_neighbors

# Create binary variables
df_sf$marketing_d <- ifelse(df_sf$marketing_sum > 0, 1, 0)
df_sf$marketing_sum_any <- ifelse(df_sf$marketing_sum > 0, 1, 0)
df_sf$marketing_sum_a2 <- ifelse(df_sf$marketing_sum > 1, 1, 0)
df_sf$marketing_sum_a3 <- ifelse(df_sf$marketing_sum > 2, 1, 0)
df_sf$marketing_sum_a4 <- ifelse(df_sf$marketing_sum == 4, 1, 0)

# Convert back to data frame and extract lon and lat
df <- df_sf
df$lon <- st_coordinates(df_sf)[,1]
df$lat <- st_coordinates(df_sf)[,2]
df <- as.data.frame(df)
df$geometry <- NULL  # Remove geometry column

# Variable Selection for final analysis
# Exclude hh_id, lon, lat, province_code, and province_d14
data_24_clean <- df %>%
  select(
    hdds,
    marketing_sum_any,
    hhh_educated,
    hhh_sex,
    hhh_age,
    hhh_female_d,
    hhh_marital,
    hhh_religion_d,
    # Exclude hh_id
    # hh_id,
    hhh_employ_d,
    lnproxy_income,
    draughtpowercat_d,
    hh_size,
    cattleyn,
    donkeyyn,
    sheepyn,
    goatsyn,
    pigsyn,
    poultry_yn,
    rabbitsyn
    # Exclude lon and lat
    # lon,
    # lat
    # Exclude province_code and province_d14
    # province_code
    # province_d14
  ) %>%
  mutate(
    # Convert specific variables to numeric
    hdds = as.numeric(hdds),
    marketing_sum_any = as.numeric(marketing_sum_any),
    hhh_age = as.numeric(hhh_age),
    hhh_female_d = as.numeric(hhh_female_d),
    lnproxy_income = as.numeric(lnproxy_income),
    hh_size = as.numeric(hh_size),
    cattleyn = as.numeric(cattleyn),
    donkeyyn = as.numeric(donkeyyn),
    sheepyn = as.numeric(sheepyn),
    goatsyn = as.numeric(goatsyn),
    pigsyn = as.numeric(pigsyn),
    poultry_yn = as.numeric(poultry_yn),
    rabbitsyn = as.numeric(rabbitsyn)
    # Leave factor variables as factors
  ) %>%
  na.omit()

# Remove rows with NA in factor variables
factor_vars_all <- sapply(data_24_clean, is.factor)
factor_var_names <- names(data_24_clean)[factor_vars_all]

data_24_clean <- data_24_clean %>%
  filter(complete.cases(select(., all_of(factor_var_names))))

# Check the number of observations after cleaning
cat("Number of observations after cleaning:", nrow(data_24_clean), "\n")

###---B. Causal Forest----
# Combine outcome, treatment, and covariates into a single data frame
data_for_analysis <- data_24_clean %>%
  select(hdds, marketing_sum_any, everything())

# Identify factor and numeric variables
factor_vars <- sapply(data_for_analysis, is.factor)
numeric_vars <- !factor_vars

# Combine numeric variables (excluding outcome and treatment)
numeric_data <- data_for_analysis[, numeric_vars]
numeric_data <- numeric_data[, !names(numeric_data) %in% c("hdds", "marketing_sum_any")]

# Ensure that numeric data is numeric
numeric_data[] <- lapply(numeric_data, function(x) as.numeric(as.character(x)))

# Convert factor variables to dummy variables using model.matrix
if (any(factor_vars)) {
  factor_data <- data_for_analysis[, factor_vars]
  # Exclude variables to exclude from factor_data if any
  factor_data <- factor_data[, !names(factor_data) %in% c("province_code")]
  
  # Convert factors to character to avoid issues with as.numeric later
  factor_data[] <- lapply(factor_data, as.character)
  covariates_factors <- model.matrix(~ . -1, data = factor_data)
  
  # Remove columns related to province_code from covariates_factors if any
  covariates_factors <- covariates_factors[, !grepl("province_code", colnames(covariates_factors))]
} else {
  covariates_factors <- NULL
}

# Combine numeric variables and factor variables
if (!is.null(covariates_factors)) {
  covariates_combined <- cbind(numeric_data, covariates_factors)
} else {
  covariates_combined <- as.matrix(numeric_data)
}

# Remove any columns that have become entirely NA after coercion
covariates_combined <- covariates_combined[, colSums(is.na(covariates_combined)) < nrow(covariates_combined)]

# Ensure all columns are numeric and handle any remaining NAs
covariates_combined[] <- lapply(as.data.frame(covariates_combined), function(x) {
  x <- as.numeric(x)
  x[is.na(x) | is.nan(x) | is.infinite(x)] <- 0
  return(x)
})

covariates_combined <- as.matrix(covariates_combined)

# Remove columns with zero variance
covariate_sd <- apply(covariates_combined, 2, sd)
zero_variance_cols <- names(covariate_sd[covariate_sd == 0])
if(length(zero_variance_cols) > 0) {
  cat("Removing columns with zero variance:\n")
  print(zero_variance_cols)
  covariates_combined <- covariates_combined[, !(colnames(covariates_combined) %in% zero_variance_cols), drop = FALSE]
}

# Proceed with detecting linear dependencies
comboInfo <- findLinearCombos(covariates_combined)
if (!is.null(comboInfo$remove) && length(comboInfo$remove) > 0) {
  cat("The following columns are linear combinations of others and will be removed:\n")
  print(colnames(covariates_combined)[comboInfo$remove])
  covariates_combined <- covariates_combined[, -comboInfo$remove, drop = FALSE]
}

# Standardize the covariates matrix
covariates_matrix <- scale(covariates_combined)

# Ensure no NA, NaN, or Inf values after scaling
any_na <- any(is.na(covariates_matrix))
any_nan <- any(is.nan(covariates_matrix))
any_inf <- any(is.infinite(covariates_matrix))
cat("Any NA values in covariates_matrix:", any_na, "\n")
cat("Any NaN values in covariates_matrix:", any_nan, "\n")
cat("Any Inf values in covariates_matrix:", any_inf, "\n")

# Ensure outcome and treatment variables are aligned with covariates_matrix
outcome <- data_for_analysis$hdds
treatment <- data_for_analysis$marketing_sum_any

# Convert outcome and treatment to numeric and handle NAs
outcome <- as.numeric(as.character(outcome))
treatment <- as.numeric(as.character(treatment))

outcome[is.na(outcome) | is.nan(outcome) | is.infinite(outcome)] <- 0
treatment[is.na(treatment) | is.nan(treatment) | is.infinite(treatment)] <- 0

# Ensure lengths match
if(length(outcome) != nrow(covariates_matrix) || length(treatment) != nrow(covariates_matrix)) {
  stop("Lengths of outcome or treatment do not match number of rows in covariates_matrix.")
}

# Train the causal forest
forest <- causal_forest(X = covariates_matrix, Y = outcome, W = treatment, honesty = TRUE)

# Predict the treatment effects
predictions <- predict(forest, estimate.variance = TRUE)

# Extract treatment effect estimates
treatment_effects <- predictions$predictions

# Add treatment effects to the data
data_24_clean$treatment_effects <- treatment_effects

# Average Treatment Effect
ate <- average_treatment_effect(forest)
cat("ATE Estimate:", round(ate[1], 3), "\n")
cat("ATE Standard Error:", round(ate[2], 3), "\n")

###---C. Results Table----
# Extract treatment effects and variances
treatment_effects <- predictions$predictions
std_errors <- sqrt(predictions$variance.estimates)

# Calculate p-values
p_value <- 2 * pnorm(-abs(mean(treatment_effects) / mean(std_errors)))

# Add significance stars
stars <- ifelse(p_value < 0.01, "***",
                ifelse(p_value < 0.05, "**",
                       ifelse(p_value < 0.1, "*", "")))

# Prepare results
results <- data.frame(
  Model = "At Least One Marketing Practice",
  CATE = paste0(round(mean(treatment_effects), 3), stars),
  Std_Error = paste0("(", round(mean(std_errors), 3), ")")
)

# Create a formatted table
stargazer(results, type = "text", summary = FALSE,
          title = "Causal Forest Results for Household Dietary Diversity Index",
          rownames = FALSE, digits = 3,
          notes = "Note: *p<0.1; **p<0.05; ***p<0.01")

###---D. RATE (Rank-Weighted Average Treatment Effect)----
# This step helps to evaluate how effectively the model predicts which individuals will benefit the most from a treatment
# Proceed with RATE analysis
cate_hat <- predict(forest, covariates_matrix)$predictions

# Define a subset excluding extreme propensity scores
subset_indices <- which(forest$W.hat > 0.1 & forest$W.hat < 0.9)

# Compute the RATE using the subset
rate <- rank_average_treatment_effect(
  forest,
  cate_hat,
  subset = subset_indices
)

# Print the RATE estimates and standard errors
print(paste("RATE Estimate:", round(rate$estimate, 3)))
print(paste("RATE Standard Error:", round(rate$std.err, 3)))

# Save the TOC curve to a PNG file
png("TOC_curve.png", width = 800, height = 600)
plot(rate, main = "TOC Curve for Household Dietary Diversity Index")
dev.off()
###---E. Variable Importance Analysis----

# Compute variable importance
var_imp <- variable_importance(forest)

# Create a data frame with variable names and their importance scores
var_imp_df <- data.frame(
  Variable = colnames(covariates_matrix),
  Importance = var_imp
)

# Sort the variables by importance in descending order
var_imp_df <- var_imp_df %>%
  arrange(desc(Importance))

# Print the top variables contributing to the treatment effect heterogeneity
print("Top variables contributing to the treatment effect heterogeneity:")
print(head(var_imp_df, 10))

# Visualize variable importance
library(ggplot2)

ggplot(var_imp_df[1:20, ], aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = 'steelblue') +
  coord_flip() +
  labs(title = "Top 20 Variable Importances in Causal Forest",
       x = "Variables",
       y = "Importance Score") +
  theme_minimal()

###---F. Analysis of Top and Bottom Individuals Based on Important Variables----

# Define the fraction of top and bottom individuals to analyze
top_fraction <- 0.2
num_top_individuals <- round(top_fraction * nrow(data_24_clean))

# Extract top and bottom individuals based on predicted treatment effects
top_individuals <- data_24_clean %>%
  arrange(desc(treatment_effects)) %>%
  head(num_top_individuals)

bottom_individuals <- data_24_clean %>%
  arrange(treatment_effects) %>%
  head(num_top_individuals)

# Get the variable importance scores from the causal forest
var_imp <- variable_importance(forest)

# Create a data frame with variable names and their importance scores
var_imp_df <- data.frame(
  Variable = colnames(covariates_matrix),
  Importance = var_imp
)

# Map dummy variable names back to original variable names
# For dummy variables, extract the base variable name before the dot or underscore
var_imp_df$Original_Variable <- sapply(var_imp_df$Variable, function(x) {
  if (grepl("\\.|_", x)) {
    unlist(strsplit(x, "\\.|_"))[1]
  } else {
    x
  }
})

# Aggregate importance scores by original variable
var_imp_agg <- var_imp_df %>%
  group_by(Original_Variable) %>%
  summarise(Total_Importance = sum(Importance)) %>%
  arrange(desc(Total_Importance))

# Select the top N variables
top_N <- 5  # Adjust as needed
significant_vars <- var_imp_agg$Original_Variable[1:top_N]

# Verify that significant_vars are present in data_24_clean
missing_vars <- setdiff(significant_vars, names(data_24_clean))
if (length(missing_vars) > 0) {
  cat("The following variables are not found in data_24_clean and will be excluded:", missing_vars, "\n")
  significant_vars <- setdiff(significant_vars, missing_vars)
}

# Prepare data for plotting
library(ggplot2)

# Function to plot variable distributions
plot_variable_distributions <- function(var_name) {
  # Check if variable exists in the data
  if (!var_name %in% names(data_24_clean)) {
    cat("Variable", var_name, "not found in the data.\n")
    return(NULL)
  }
  
  # Extract variable from the data
  variable_top <- top_individuals[[var_name]]
  variable_bottom <- bottom_individuals[[var_name]]
  
  # Combine data into a data frame
  df_plot <- data.frame(
    Value = c(variable_top, variable_bottom),
    Group = rep(c("Top Individuals", "Bottom Individuals"), each = num_top_individuals)
  )
  
  # Determine if variable is numeric or factor
  if (is.numeric(df_plot$Value)) {
    # Plot histogram or density plot
    p <- ggplot(df_plot, aes(x = Value, fill = Group)) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Distribution of", var_name),
           x = var_name, y = "Density") +
      theme_minimal()
  } else {
    # Convert Value to factor if it's not already
    df_plot$Value <- as.factor(df_plot$Value)
    # Plot bar chart for factor variables
    p <- ggplot(df_plot, aes(x = Value, fill = Group)) +
      geom_bar(position = "dodge") +
      labs(title = paste("Distribution of", var_name),
           x = var_name, y = "Count") +
      theme_minimal()
  }
  
  print(p)
}

# Loop through significant variables and plot
for (var in significant_vars) {
  plot_variable_distributions(var)
}
