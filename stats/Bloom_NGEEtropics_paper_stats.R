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


### Stats
## Regressions
# LAI vs log transformed soil P
LAIsoilP <- lm(change_annual_LAI_500m ~ log10(soil_P), data = VIs_sites)
# Print summary of regression in console
summary(LAIsoilP)
# Save regression output
tab_model(LAIsoilP, file = "LAI_soilP_Reg.html")
# Convert saved html to png
webshot("LAI_soilP_Reg.html", "LAI_soilP_Reg.png")

# EVI vs log transformed soil P
EVIsoilP <- lm(change_annual_EVI_250m ~ log10(soil_P), data = VIs_sites)
summary(EVIsoilP)
tab_model(EVIsoilP, file = "EVI_soilP_Reg.html")
webshot("EVI_soilP_Reg.html", "EVI_soilP_Reg.png")


## Mixed effects
# Fixed effects: LAI vs log transformed soil phosphorous
# Mixed effects: Wind speed effects on 
LAIsoilPWS <- lmer(change_annual_LAI_500m ~ log10(soil_P) + (1|peak_wind_speed_ms), 
                   data = VIs_sites)
summary(LAIsoilPWS)
tab_model(LAIsoilPWS, file = "LAI_soilP_wind_ME.html")
webshot("LAI_soilP_wind_ME.html", "LAI_soilP_wind_ME.png")

# Wind speed effects on EVI vs log transformed soil phosphorous regression
EVIsoilPWS <- lmer(change_annual_EVI_250m ~ log10(soil_P) + (1|peak_wind_speed_ms), 
                   data = VIs_sites)
summary(EVIsoilPWS)
tab_model(EVIsoilPWS, file = "EVI_soilP_wind_ME.html")
webshot("EVI_soilP_wind_ME.html", "EVI_soilP_wind_ME.png")


