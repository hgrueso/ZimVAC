##########################################
## 0) Define a variable-label dictionary
##########################################
variable_labels <- c(
  "hdds"              = "Household Dietary Diversity Score",
  "marketing_sum_any" = "Adopted ≥1 Marketing Practice",
  
  # Covariates
  "hhh_age"           = "Age of Household Head (Years)",
  "hhh_female_d2"     = "Household Head is Female (Yes=1)",
  "hhh_educated"      = "Household Head is Educated (Yes=1)",
  "lnproxy_income"    = "Log(1 + Monthly Income)",
  "hh_size"           = "Household Size",
  "cattleyn"          = "Owns Cattle (Yes=1)",
  "donkeyyn"          = "Owns Donkeys (Yes=1)",
  "sheepyn"           = "Owns Sheep (Yes=1)",
  "goatsyn"           = "Owns Goats (Yes=1)",
  "pigsyn"            = "Owns Pigs (Yes=1)",
  "poultry_yn"        = "Owns Poultry (Yes=1)",
  "rabbitsyn"         = "Owns Rabbits (Yes=1)",
  
  "province_d1"       = "Province 1 (Manicaland)",
  "province_d2"       = "Province 2 (Mash Central)",
  "province_d3"       = "Province 3 (Mash East)",
  "province_d4"       = "Province 4 (Mash West)",
  "province_d5"       = "Province 5 (Mat North)",
  "province_d6"       = "Province 6 (Mat South)",
  "province_d7"       = "Province 7 (Midlands)",
  "province_d8"       = "Province 8 (Masvingo)",
  
  # Examples if you also store marital or religion dummies
  "hhh_marital_d1"    = "Married, Living Together (Yes=1)",
  "hhh_marital_d2"    = "Married, Living Apart (Yes=1)",
  "hhh_marital_d3"    = "Divorced/Separated (Yes=1)",
  "hhh_marital_d4"    = "Widow/Widower (Yes=1)",
  "hhh_marital_d5"    = "Cohabiting (Yes=1)",
  "hhh_marital_d6"    = "Never Married (Yes=1)",
  
  "hhh_religion_d1"   = "Roman Catholic (Yes=1)",
  "hhh_religion_d2"   = "Protestant (Yes=1)",
  "hhh_religion_d3"   = "Pentecostal (Yes=1)",
  "hhh_religion_d4"   = "Apostolic Sect (Yes=1)",
  "hhh_religion_d5"   = "Zion (Yes=1)",
  "hhh_religion_d6"   = "Other Christian (Yes=1)",
  "hhh_religion_d7"   = "Islam (Yes=1)",
  "hhh_religion_d8"   = "Traditional (Yes=1)",
  "hhh_religion_d9"   = "Other Religion (Yes=1)",
  "hhh_religion_d10"  = "No Religion (Yes=1)",
  
  "unemployed"        = "Household Head is Unemployed (Yes=1)"
)

# Helper function to map raw variable names to labels
label_variables <- function(var_names, dict) {
  sapply(var_names, function(v) if (v %in% names(dict)) dict[v] else v)
}


##########################################
## 1) Load packages
##########################################
library(sf)
library(haven)
library(dplyr)
library(units)
library(grf)
library(ggplot2)
library(caret)
library(knitr)
library(kableExtra)
library(openxlsx)  # For exporting to Excel

##########################################
## 2) Read data & convert certain variables to numeric
##########################################
# data <- read_dta("C:/Users/hgrue/Dropbox/Documents/Laboral/Oxford/Climate, Conflicts, & Pandemics/Zimbabwe Paper/ZimVACCombined.dta")
data <- read_dta("./ZIMVACCombined.dta")
nrow(data)

numerdatanumeric_vars <- c("hhh_female_d2", "hh_size", "hdds", "hhh_age", 
                  "lnproxy_income", "cattleyn", "donkeyyn", "sheepyn", 
                  "goatsyn", "pigsyn", "poultry_yn", "rabbitsyn")
data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)

##########################################
## 3) Remove rows with incomplete cases
##########################################
data <- na.omit(data)

##########################################
## 4) Exclude specific unused variables
##########################################
excluded_vars <- c(
  "hh_id", "marketinformation", "organisedmarketing", "farmerorganisation", 
  "marketdistribution", "marketing_sum_a2", "marketing_sum_a3", "marketing_sum_a4", 
  "proxy_income", "splag1_marketinformation_b", "splag1_organisedmarketing_b", 
  "splag1_farmerorganisation_b", "splag1_marketdistribution_b", 
  "splag1_marketing_sum_any_b"
)
data <- data[, !names(data) %in% excluded_vars]


#############################################################
## 5) Create Descriptive Statistics Table (Table 1)
#############################################################

# 5a) Define variables for descriptive stats
desc_vars <- c(
  "hhh_educated", "hhh_age", "hhh_female_d2", "hh_size", "lnproxy_income",
  "cattleyn", "donkeyyn", "sheepyn", "goatsyn", "pigsyn",
  "poultry_yn", "rabbitsyn",
  "province_d1", "province_d2", "province_d3", "province_d4",
  "province_d5", "province_d6", "province_d7", "province_d8"
)

# 5b) Helper function to compute group means, sds, sample size, difference in means, p-value
make_descriptive_table <- function(data, vars, treat_var, label_dict) {
  
  data_adopt    <- data %>% filter(!!sym(treat_var) == 1)
  data_nonadopt <- data %>% filter(!!sym(treat_var) == 0)
  
  results <- lapply(vars, function(v) {
    x_adopt <- data_adopt[[v]]
    meanA   <- mean(x_adopt, na.rm = TRUE)
    sdA     <- sd(x_adopt, na.rm = TRUE)
    nA      <- sum(!is.na(x_adopt))
    
    x_non   <- data_nonadopt[[v]]
    meanN   <- mean(x_non, na.rm = TRUE)
    sdN     <- sd(x_non, na.rm = TRUE)
    nN      <- sum(!is.na(x_non))
    
    # t-test for difference
    ttest_res <- t.test(x_adopt, x_non)
    diff_mean <- meanA - meanN
    p_val     <- ttest_res$p.value
    
    lbl <- ifelse(v %in% names(label_dict), label_dict[[v]], v)
    
    data.frame(
      Variable         = lbl,
      Mean_Adopt       = round(meanA, 3),
      SD_Adopt         = paste0("(", round(sdA, 3), ")"),
      N_Adopt          = nA,
      Mean_NonAdopt    = round(meanN, 3),
      SD_NonAdopt      = paste0("(", round(sdN, 3), ")"),
      N_NonAdopt       = nN,
      Diff_YminusN     = round(diff_mean, 3),
      p_value          = round(p_val, 4),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}

# 5c) Generate the descriptive table for year = 2024
data_24 <- subset(data, year == 2024)
desc_table <- make_descriptive_table(
  data      = data_24,
  vars      = desc_vars,
  treat_var = "marketing_sum_any",
  label_dict = variable_labels
)

# 5d) Print Table 1 in the console
cat("\n\n-------------------------------\nTable 1: Descriptive Statistics by Adoption (2024)\n-------------------------------\n")
print(
  kable(desc_table, format = "markdown", align = "l", caption = "Descriptive Stats")  
)

##########################################
## 5.1) Create Province Factor with Province 1 as Reference (for analysis)
##########################################
# Combine the individual province dummies into a single factor variable.
data_24 <- data_24 %>% mutate(province = case_when(
  province_d1 == 1 ~ "1",
  province_d2 == 1 ~ "2",
  province_d3 == 1 ~ "3",
  province_d4 == 1 ~ "4",
  province_d5 == 1 ~ "5",
  province_d6 == 1 ~ "6",
  province_d7 == 1 ~ "7",
  province_d8 == 1 ~ "8"
))
data_24$province <- as.factor(data_24$province)
data_24$province <- relevel(data_24$province, ref = "1")

##########################################
## 6) Prepare outcome, treatment, covariates (2024)
##########################################
outcome    <- as.numeric(data_24$hdds)
treatment  <- as.numeric(data_24$marketing_sum_any)
# Exclude the original province dummy variables so that the new 'province' factor is used
covariates <- data_24[, !(names(data_24) %in% c("hdds", "marketing_sum", "marketing_sum_any", "year",
                                                "province_d1", "province_d2", "province_d3", "province_d4",
                                                "province_d5", "province_d6", "province_d7", "province_d8"))]

##########################################
## 7) Train the causal forest model (2024)
##########################################
# Convert covariates to a numeric matrix (exclude the intercept)
covariates_numeric <- model.matrix(~ . - 1, data = covariates)

# Train the causal forest using the numeric matrix
causal_forest_model <- causal_forest(
  X       = covariates_numeric, 
  Y       = outcome,    
  W       = treatment,  
  honesty = TRUE
)

##########################################
## 8) Compute Average Treatment Effects (2024) - Table 2
##########################################
ate_result <- average_treatment_effect(causal_forest_model, target.sample = "all")
att_result <- average_treatment_effect(causal_forest_model, target.sample = "treated")
atu_result <- average_treatment_effect(causal_forest_model, target.sample = "control")

ate_est <- as.numeric(ate_result["estimate"])
ate_se  <- as.numeric(ate_result["std.err"])
ate_tstat <- ate_est / ate_se
ate_p  <- 2 * pt(-abs(ate_tstat), df = Inf)

att_est <- as.numeric(att_result["estimate"])
att_se  <- as.numeric(att_result["std.err"])
att_tstat <- att_est / att_se
att_p  <- 2 * pt(-abs(att_tstat), df = Inf)

atu_est <- as.numeric(atu_result["estimate"])
atu_se  <- as.numeric(atu_result["std.err"])
atu_tstat <- atu_est / atu_se
atu_p  <- 2 * pt(-abs(atu_tstat), df = Inf)

sig_stars <- function(estimate, pval) {
  if (pval < 0.001) stars <- "****"
  else if (pval < 0.01) stars <- "***"
  else if (pval < 0.05) stars <- "**"
  else if (pval < 0.1) stars <- "*"
  else stars <- ""
  paste0(round(estimate, 4), stars)
}

estimates <- c(sig_stars(att_est, att_p),
               sig_stars(ate_est, ate_p),
               sig_stars(atu_est, atu_p))
ses <- c(paste0("(", round(att_se, 4), ")"),
         paste0("(", round(ate_se, 4), ")"),
         paste0("(", round(atu_se, 4), ")"))
nvals <- rep(nrow(data_24), 3)

results_table <- data.frame(
  " "        = c("Adopted ≥1 Marketing Practice", "", "N"),
  "(1) ATT"  = c(estimates[1], ses[1], nvals[1]),
  "(2) ATE"  = c(estimates[2], ses[2], nvals[2]),
  "(3) ATU"  = c(estimates[3], ses[3], nvals[3]),
  check.names = FALSE
)

cat("\n\n-------------------------------\nTable 2: Causal Forest Estimates (ATT, ATE, ATU) - 2024\n-------------------------------\n")
print(
  kable(results_table, format = "markdown", align = "l", caption = "Causal Forest Estimates 2024")
)

##########################################
## 9) Investigate Heterogeneous Treatment Effects (2024)
##########################################

### 9a) Variable Importance - Table 3
vi <- variable_importance(causal_forest_model)
if (is.null(names(vi))) {
  names(vi) <- colnames(covariates_numeric)
}
vi_df <- data.frame(
  Variable = names(vi),
  Importance = vi
) %>% arrange(desc(Importance))

# Replace with labels
vi_df$Variable <- label_variables(vi_df$Variable, variable_labels)

cat("\n\n-------------------------------\nTable 3: Variable Importance (2024)\n-------------------------------\n")
print(
  kable(vi_df, format = "markdown", align = "l", caption = "Variable Importance 2024")
)

### 9b) Best Linear Projection (BLP) - Table 4
blp_24 <- best_linear_projection(causal_forest_model, covariates_numeric)
blp_24_mat <- as.matrix(blp_24)

blp_24_df <- data.frame(
  Variable   = rownames(blp_24_mat),
  Estimate   = blp_24_mat[, "Estimate"],
  Std.Error  = blp_24_mat[, "Std. Error"],
  t.value    = blp_24_mat[, "t value"],
  p.value    = blp_24_mat[, "Pr(>|t|)"]
)

blp_24_df$Variable <- label_variables(blp_24_df$Variable, variable_labels)

cat("\n\n-------------------------------\nTable 4: Best Linear Projection (BLP) Results (2024)\n-------------------------------\n")
print(
  kable(blp_24_df, format = "markdown", align = "l", caption = "BLP Results 2024")
)

#lm_outcome <- lm(Y ~ covariates_numeric + W, data
                 
##########################################
## 10) Sensitivity Analysis (2024) - Table 5
##########################################
model_interaction <- lm(
  hdds ~ marketing_sum_any * province_d7 +
    marketing_sum_any * province_d2 +
    marketing_sum_any * province_d5 +
    cattleyn + donkeyyn + sheepyn + goatsyn + pigsyn +
    poultry_yn + rabbitsyn + hh_size + hhh_age + hhh_female_d2 +
    lnproxy_income,
  data = data_24
)

interaction_summary <- summary(model_interaction)
cat("\n\n-------------------------------\nTable 5: Sensitivity Analysis - Interaction Model (2024)\n-------------------------------\n")
print(
  kable(interaction_summary$coefficients, format = "markdown", caption = "Interaction Model 2024")
)

## 10b) New Regression: Interaction with Female (Heterogeneous Treatment Effects) - 2024
model_interaction_female <- lm(
  hdds ~ marketing_sum_any * hhh_female_d2 +
    cattleyn + donkeyyn + sheepyn + goatsyn + pigsyn +
    poultry_yn + rabbitsyn + hh_size + hhh_age + lnproxy_income +
    province,  # including the province factor as control
  data = data_24
)

# Obtain the summary and extract the coefficients
interaction_female_summary <- summary(model_interaction_female)
coef_table <- as.data.frame(interaction_female_summary$coefficients)
coef_table$Variable <- rownames(coef_table)

# Replace raw variable names with actual labels using your dictionary
coef_table$Variable <- label_variables(coef_table$Variable, variable_labels)

# Reorder columns to show the Variable label first
coef_table <- coef_table[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

cat("\n\n-------------------------------\nNew Regression: Interaction with Female (Heterogeneous Treatment Effects) - 2024\n-------------------------------\n")
print(
  kable(coef_table, format = "markdown", caption = "Interaction Model with Female 2024")
)

##########################################
## 11) Selection Model (2024) - Table 6
##########################################
model_selection <- lm(
  marketing_sum_any ~ cattleyn + donkeyyn + sheepyn + goatsyn + pigsyn +
    poultry_yn + rabbitsyn + hh_size + hhh_age + hhh_female_d2 +
    lnproxy_income + province_d2 + province_d3 + province_d4 + province_d5 +
    province_d6 + province_d7 + province_d8,
  data = data_24
)
selection_summary <- summary(model_selection)

cat("\n\n-------------------------------\nTable 6: Selection Model (Any Practice) - 2024\n-------------------------------\n")
print(
  kable(selection_summary$coefficients, format = "markdown", caption = "Selection Model 2024")
)

##########################################
## 12) Additional Analysis: BLP on 2022 Data - Appendix Table 1
##########################################
# Subset data for 2022
data_22 <- subset(data, year == 2022)

# Create province factor for data_22 using the province dummies and explicitly set levels
data_22 <- data_22 %>% mutate(province = case_when(
  province_d1 == 1 ~ "1",
  province_d2 == 1 ~ "2",
  province_d3 == 1 ~ "3",
  province_d4 == 1 ~ "4",
  province_d5 == 1 ~ "5",
  province_d6 == 1 ~ "6",
  province_d7 == 1 ~ "7",
  province_d8 == 1 ~ "8"
))
data_22$province <- factor(data_22$province, levels = c("1","2","3","4","5","6","7","8"))
# Exclude Province 1 observations
data_22 <- subset(data_22, province != "1")

# Prepare outcome, treatment, and covariates.
outcome_22    <- as.numeric(data_22$hdds)
treatment_22  <- as.numeric(data_22$marketing_sum_any)
covariates_22 <- data_22[, !(names(data_22) %in% c("hdds", "marketing_sum", "marketing_sum_any", "year",
                                                   "province_d1", "province_d2", "province_d3", "province_d4",
                                                   "province_d5", "province_d6", "province_d7", "province_d8"))]

# Convert covariates to a numeric matrix (one-hot encoding factors)
covariates_22_numeric <- model.matrix(~ . - 1, data = covariates_22)

# Ensure that the expected province dummy columns are present.
# We expect dummy columns for provinces "2", "3", "4", "5", "6", "7", and "8".
expected_province_cols <- paste0("province", c("2", "3", "4", "5", "6", "7", "8"))
missing_cols <- setdiff(expected_province_cols, colnames(covariates_22_numeric))
if (length(missing_cols) > 0) {
  for (col in missing_cols) {
    covariates_22_numeric <- cbind(covariates_22_numeric,
                                   setNames(data.frame(rep(0, nrow(covariates_22_numeric))),
                                            col))
  }
}
# Reorder the columns so that the province dummies appear in the expected order
covariates_22_numeric <- covariates_22_numeric[, c(setdiff(colnames(covariates_22_numeric),
                                                           expected_province_cols),
                                                   expected_province_cols)]

# Train the causal forest model on the 2022 data (with Province 1 excluded)
causal_forest_22 <- causal_forest(
  X       = covariates_22_numeric, 
  Y       = outcome_22,    
  W       = treatment_22,  
  honesty = TRUE
)

# Compute the best linear projection
blp_22_obj <- best_linear_projection(causal_forest_22, covariates_22_numeric)
blp_22_mat <- as.matrix(blp_22_obj)

# Create a results table and replace raw variable names with labels
blp_22_df <- data.frame(
  Variable  = rownames(blp_22_mat),
  Estimate  = blp_22_mat[, "Estimate"],
  Std.Error = blp_22_mat[, "Std. Error"],
  t.value   = blp_22_mat[, "t value"],
  p.value   = blp_22_mat[, "Pr(>|t|)"]
)

blp_22_df$Variable <- label_variables(blp_22_df$Variable, variable_labels)

cat("\n\n-------------------------------\nAppendix Table 1: Best Linear Projection (BLP) Results (2022, Province 1 Excluded)\n-------------------------------\n")
print(
  kable(blp_22_df[, c("Variable", "Estimate", "Std.Error", "t.value", "p.value")], 
        format = "markdown", align = "l", caption = "BLP Results 2022 (Province 1 Excluded)")
)
