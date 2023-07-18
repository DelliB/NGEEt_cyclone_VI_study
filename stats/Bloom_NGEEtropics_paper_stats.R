# Dellena Bloom
# September 16th, 2022
# NGEE-tropics paper stats


### Organizing Environment
## Load packages and data
# load packages
library(tidyverse)
library(ggpubr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(webshot)
library(lme4)

# Dellena file path
#data_directory <- '~/Documents/Kueppers lab'
data_directory <- 'private/data'

# load data
VIs_sites <- read.csv(file.path(data_directory, "case_study_data.csv"), 
                      stringsAsFactors= FALSE, na.strings = '-9999') %>%
  drop_na(change_annual_LAI_500m)
# recovery <- read.csv(file.path(data_directory, "MODIS_recovery.csv"), 
#                      stringsAsFactors= FALSE, na.strings = '-9999')
LFreco <- read.csv(file.path(data_directory, "MODIS_recovery_bb.csv"), 
                   stringsAsFactors= FALSE, na.strings = '-9999')


### Stats
## Regressions
# annual LAI vs log transformed soil P
aLAIsoilP <- lm(change_annual_LAI_500m ~ log10(soil_P), data = VIs_sites)
# Print summary of regression in console
summary(aLAIsoilP)
# Save regression output
tab_model(aLAIsoilP, file = "aLAI_soilP_Reg.html")
# Convert saved html to png
webshot("aLAI_soilP_Reg.html", "aLAI_soilP_Reg.png")

# sub-annual LAI vs log transformed soil P
saLAIsoilP <- lm(change_sub.annual_LAI_500m ~ log10(soil_P), data = VIs_sites)
summary(saLAIsoilP)
tab_model(saLAIsoilP, file = "saLAI_soilP_Reg.html")
webshot("saLAI_soilP_Reg.html", "saLAI_soilP_Reg.png")

# annual EVI vs log transformed soil P
aEVIsoilP <- lm(change_annual_EVI_250m ~ log10(soil_P), data = VIs_sites)
summary(aEVIsoilP)
tab_model(aEVIsoilP, file = "aEVI_soilP_Reg.html")
webshot("aEVI_soilP_Reg.html", "aEVI_soilP_Reg.png")

# sub-annual EVI vs log transformed soil P
saEVIsoilP <- lm(change_sub.annual_EVI_250m ~ log10(soil_P), data = VIs_sites)
summary(saEVIsoilP)
tab_model(saEVIsoilP, file = "saEVI_soilP_Reg.html")
webshot("saEVI_soilP_Reg.html", "saEVI_soilP_Reg.png")


## Mixed effects
# Fixed effects: annual LAI vs log transformed soil phosphorous
# Mixed effects: Wind speed
aLAIsoilPC <- lmer(change_annual_LAI_500m ~ log10(soil_P) + (1|peak_wind_speed_ms), 
                   data = VIs_sites)
summary(aLAIsoilPC)
tab_model(aLAIsoilPC, file = "aLAI_soilP_cyclone_ME.html")
webshot("aLAI_soilP_cyclone_ME.html", "aLAI_soilP_cyclone_ME.png")

# Fixed effects: sub-annual LAI vs log transformed soil phosphorous
# Mixed effects: Wind speed
saLAIsoilPC <- lmer(change_sub.annual_LAI_500m ~ log10(soil_P) + (1|peak_wind_speed_ms), 
                  data = VIs_sites)
summary(saLAIsoilPC)
tab_model(saLAIsoilPC, file = "saLAI_soilP_cyclone_ME.html")
webshot("saLAI_soilP_cyclone_ME.html", "saLAI_soilP_cyclone_ME.png")

# Fixed effects: annual EVI vs log transformed soil phosphorous
# Mixed effects: Wind speed
aEVIsoilPC <- lmer(change_annual_EVI_250m ~ log10(soil_P) + (1|peak_wind_speed_ms), 
                  data = VIs_sites)
summary(aEVIsoilPC)
tab_model(aEVIsoilPC, file = "aEVI_soilP_cyclone_ME.html")
webshot("aEVI_soilP_cyclone_ME.html", "aEVI_soilP_cyclone_ME.png")

# Fixed effects: sub-annual EVI vs log transformed soil phosphorous
# Mixed effects: Wind speed
saEVIsoilPC <- lmer(change_sub.annual_EVI_250m ~ log10(soil_P) + (1|peak_wind_speed_ms), 
                  data = VIs_sites)
summary(saEVIsoilPC)
tab_model(saEVIsoilPC, file = "saEVI_soilP_cyclone_ME.html")
webshot("saEVI_soilP_cyclone_ME.html", "saEVI_soilP_cyclone_ME.png")

# Site effects on LAI vs log transformed soil phosphorous regression
#LAIsoilPS <- lmer(change_annual_LAI_500m ~ log10(soil_P) + (1|Site), 
#                   data = VIs_sites)
#summary(LAIsoilPS)
#tab_model(LAIsoilPS, file = "LAI_soilP_site_ME.html")
#webshot("LAI_soilP_site_ME.html", "LAI_soilP_site_ME.png")

# Determine best LAI mixed effects model using AIC
# Lower AIC values indicate a better-fit model
AIC(LAIsoilPC) # 19
#AIC(LAIsoilPS) # 25

# Determine best EVI mixed effects model using AIC
#AIC(EVIsoilPC) # 17
AIC(EVIsoilPS) # 14


## Variation in recovery times
# Calculating recovery stats for EVI
EVI_time_stats <- LFreco %>%
  select(EVI_change_250m, month, case_study) %>%
  filter(EVI_change_250m > 0) %>%
  reframe(time_at_baseline = min(month), .by = case_study) %>%
  reframe(EVI_month_lower = quantile(time_at_baseline, 0.05, na.rm = T),
          EVI_month_upper = quantile(time_at_baseline, 0.95, na.rm = T),
          EVI_month_m = mean(time_at_baseline, na.rm = T))

# Calculating recovery stats for LAI
LAI_time_stats <- LFreco %>%
  select(LAI_change_500m, month, case_study) %>%
  filter(LAI_change_500m > 0) %>%
  reframe(time_at_baseline = min(month), .by = case_study) %>%
  reframe(LAI_month_lower = quantile(time_at_baseline, 0.05, na.rm = T),
          LAI_month_upper = quantile(time_at_baseline, 0.95, na.rm = T),
          LAI_month_m = mean(time_at_baseline, na.rm = T))


