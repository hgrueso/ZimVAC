library(haven)
library(spatstat)
library(dplyr)
library(here)

# Load the data
data_24 <- read_dta(here("data/2024_ZimLAC_rural_SF.dta"))
data_22 <- read_dta(here("data/2022_ZimVAC_RLA_SF.dta"))

# Background characteristics
# 2024 Data Processing
data_24$hh_age[data_24$hh_age == 998] <- NA
data_24$hhh_age <- data_24$hh_age

data_24$hhh_sex <- data_24$hh_sex
data_24$hhh_female_d <- ifelse(data_24$hhh_sex == 1, 1, 0)

data_24$hhh_marital <- data_24$hh_marital
data_24$hhh_marital[data_24$hhh_marital == 99] <- NA

data_24$province_d <- as.factor(data_24$province_code)
data_24$hhh_religion_d <- as.factor(data_24$hh_religion)

data_24$hheduclevel[is.na(data_24$hheduclevel)] <- data_24$reseduclevel
data_24$hhh_education_d <- as.factor(data_24$hheduclevel)
data_24$hhh_educated <- ifelse(data_24$hhh_education_d == 1, 0, 1)

data_24$hhh_employ <- data_24$hh_employ
data_24$hhh_employ_d <- as.factor(data_24$hhh_employ)

data_24$draughtpowercat_d <- as.factor(data_24$draughtpowercat)
data_24$lnproxy_income <- log(1 + data_24$proxy_income)

# Remove rows with missing lon or lat values
data_24_complete <- data_24[!is.na(data_24$lon) & !is.na(data_24$lat), ]

# Market information paper
# Convert the relevant variables into spatial point patterns
marketinformation_ppp <- ppp(data_24_complete$lon, data_24_complete$lat, marks = data_24_complete$marketinformation, window = owin(range(data_24_complete$lon), range(data_24_complete$lat)))
organisedmarketing_ppp <- ppp(data_24_complete$lon, data_24_complete$lat, marks = data_24_complete$organisedmarketing, window = owin(range(data_24_complete$lon), range(data_24_complete$lat)))
farmerorganisation_ppp <- ppp(data_24_complete$lon, data_24_complete$lat, marks = data_24_complete$farmerorganisation, window = owin(range(data_24_complete$lon), range(data_24_complete$lat)))
marketdistribution_ppp <- ppp(data_24_complete$lon, data_24_complete$lat, marks = data_24_complete$marketdistribution, window = owin(range(data_24_complete$lon), range(data_24_complete$lat)))

# Calculate distances (k=10) and convert to binary/count variables
data_24_complete$marketinformation_dist <- ifelse(nndist(marketinformation_ppp, k=10) <= 10, 1, 0)
data_24_complete$organisedmarketing_dist <- ifelse(nndist(organisedmarketing_ppp, k=10) <= 10, 1, 0)
data_24_complete$farmerorganisation_dist <- ifelse(nndist(farmerorganisation_ppp, k=10) <= 10, 1, 0)
data_24_complete$marketdistribution_dist <- ifelse(nndist(marketdistribution_ppp, k=10) <= 10, 1, 0)

# Combine results
data_24_complete$marketing_sum <- rowSums(data_24_complete[, c("marketinformation_dist", "organisedmarketing_dist", "farmerorganisation_dist", "marketdistribution_dist")])
data_24_complete$marketing_d <- ifelse(data_24_complete$marketing_sum == 0, 0, 1)

# Generate categorical variables based on marketing_sum
data_24_complete$marketing_sum_d <- cut(data_24_complete$marketing_sum, breaks=c(-Inf, 0, 1, 2, 3, Inf), labels=0:4)
data_24_complete$marketing_sum_any <- ifelse(data_24_complete$marketing_sum_d == 1, 0, 1)

data_24_complete$marketing_sum_a2 <- ifelse(data_24_complete$marketing_sum <= 1, 0, 1)
data_24_complete$marketing_sum_a3 <- ifelse(data_24_complete$marketing_sum <= 2, 0, 1)
data_24_complete$marketing_sum_a4 <- ifelse(data_24_complete$marketing_sum == 4, 1, 0)

# Merge results back to the original data_24 frame
data_24 <- merge(data_24, data_24_complete, all.x = TRUE) ### WHY IS THIS SO SLOW?!

# Variable Selection for final analysis
data_24_clean <- data_24 %>%
  select(hdds, marketing_sum_any, hhh_educated, hhh_sex, hhh_age, hhh_female_d, 
         hhh_marital, hhh_religion_d, hh_id, hhh_employ_d, lnproxy_income, draughtpowercat_d, 
         hh_size, cattleyn, donkeyyn, sheepyn, goatsyn, pigsyn, poultry_yn, rabbitsyn, lon, lat, starts_with("province")) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# 2022 Data Processing
data_22$hhage[data_22$hhage == 998] <- NA
data_22$hhh_age <- data_22$hhage

data_22$hhh_sex <- data_22$hhsex
data_22$hhh_female_d <- ifelse(data_22$hhh_sex == 1, 1, 0)

data_22$hhh_marital <- data_22$hhmarital
data_22$hhh_marital[data_22$hhh_marital == 99] <- NA

data_22$province_d <- as.factor(data_22$PROVINCE_CODE)

data_22$hheduclevel[is.na(data_22$hheducation)] <- data_22$hheducation
data_22$hhh_education_d <- as.factor(data_22$hheduclevel)

data_22$draughtpowercat_d <- as.factor(data_22$DROUGHT)

# Remove rows with missing lon or lat values
data_22_complete <- data_22[!is.na(data_22$LONGITUDE) & !is.na(data_22$LATITUDE), ]
names(data_22_complete)[names(data_22_complete) == "LONGITUDE"] <- "lon"
names(data_22_complete)[names(data_22_complete) == "LATITUDE"] <- "lat"

data_22_complete$hhh_educated <- ifelse(data_22_complete$hhh_education_d == 1, 0, 1)

# Market information paper
marketinformation <- ifelse(data_22_complete$MARKETING==2,1,0)
organisedmarketing <- ifelse(data_22_complete$MARKETING==3,1,0)
farmerorganisation <- ifelse(data_22_complete$MARKETING==1,1,0)
marketdistribution<- ifelse(data_22_complete$MARKETING==4,1,0)

# Market information paper
# Convert the relevant variables into spatial point patterns
marketinformation_ppp <- ppp(data_22_complete$lon, data_22_complete$lat, marks = data_22_complete$marketinformation, window = owin(range(data_22_complete$lon), range(data_22_complete$lat)))
organisedmarketing_ppp <- ppp(data_22_complete$lon, data_22_complete$lat, marks = data_22_complete$organisedmarketing, window = owin(range(data_22_complete$lon), range(data_22_complete$lat)))
farmerorganisation_ppp <- ppp(data_22_complete$lon, data_22_complete$lat, marks = data_22_complete$farmerorganisation, window = owin(range(data_22_complete$lon), range(data_22_complete$lat)))
marketdistribution_ppp <- ppp(data_22_complete$lon, data_22_complete$lat, marks = data_22_complete$marketdistribution, window = owin(range(data_22_complete$lon), range(data_22_complete$lat)))

# Calculate distances (k=10) and convert to binary/count variables
data_22_complete$marketinformation_dist <- ifelse(nndist(marketinformation_ppp, k=10) <= 10, 1, 0)
data_22_complete$organisedmarketing_dist <- ifelse(nndist(organisedmarketing_ppp, k=10) <= 10, 1, 0)
data_22_complete$farmerorganisation_dist <- ifelse(nndist(farmerorganisation_ppp, k=10) <= 10, 1, 0)
data_22_complete$marketdistribution_dist <- ifelse(nndist(marketdistribution_ppp, k=10) <= 10, 1, 0)

# Combine results
data_22_complete$marketing_sum <- rowSums(data_22_complete[, c("marketinformation_dist", "organisedmarketing_dist", "farmerorganisation_dist", "marketdistribution_dist")])
data_22_complete$marketing_d <- ifelse(data_22_complete$marketing_sum == 0, 0, 1)

# Generate categorical variables based on marketing_sum
data_22_complete$marketing_sum_d <- cut(data_22_complete$marketing_sum, breaks=c(-Inf, 0, 1, 2, 3, Inf), labels=0:4)
data_22_complete$marketing_sum_any <- ifelse(data_22_complete$marketing_sum_d == 1, 0, 1)

data_22_complete$marketing_sum_a2 <- ifelse(data_22_complete$marketing_sum <= 1, 0, 1)
data_22_complete$marketing_sum_a3 <- ifelse(data_22_complete$marketing_sum <= 2, 0, 1)
data_22_complete$marketing_sum_a4 <- ifelse(data_22_complete$marketing_sum == 4, 1, 0)

# Household Dietary Diversity Index
data_22_complete$hdds <- data_22_complete$HDDS_HH

# Rename the variables to lowercase in data_22
data_22 <- data_22 %>%
  rename_with(tolower, .cols = c("SHEEPYN", "DONKEYYN", "CATTLEYN", "GOATSYN", "PIGSYN", "POULTRY_YN", "RABBITSYN"))

# Income
data_22$lnproxy_income <- log(1 + data_22$proxy_income)

# Merge results back to the original data_22 frame
data_22 <- merge(data_22, data_22_complete, all.x = TRUE)

# Variable Selection for final analysis
data_22_clean <- data_22 %>%
  select(hdds, marketing_sum_any, hhh_educated, hhh_sex, hhh_age, hhh_female_d, 
         hhh_marital, draughtpowercat_d, 
         hh_size, lnproxy_income, cattleyn, donkeyyn, sheepyn, goatsyn, pigsyn, poultry_yn, rabbitsyn, lon, lat, starts_with("province")) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()
