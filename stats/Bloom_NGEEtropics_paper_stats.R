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
# Mixed effects: Cyclone name
LAIsoilPC <- lmer(change_annual_LAI_500m ~ log10(soil_P) + (1|Cyclone_name), 
                   data = VIs_sites)
summary(LAIsoilPC)
tab_model(LAIsoilPC, file = "LAI_soilP_cyclone_ME.html")
webshot("LAI_soilP_cyclone_ME.html", "LAI_soilP_cyclone_ME.png")

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


# Cyclone name effects on EVI vs log transformed soil phosphorous regression
#EVIsoilPC <- lmer(change_annual_EVI_250m ~ log10(soil_P) + (1|Cyclone_name), 
#                   data = VIs_sites)
#summary(EVIsoilPC)
#tab_model(EVIsoilPC, file = "EVI_soilP_cyclone_ME.html")
#webshot("EVI_soilP_cyclone_ME.html", "EVI_soilP_cyclone_ME.png")

# Site effects on EVI vs log transformed soil phosphorous regression
EVIsoilPS <- lmer(change_annual_EVI_250m ~ log10(soil_P) + (1|Site), 
                   data = VIs_sites)
summary(EVIsoilPS)
tab_model(EVIsoilPS, file = "EVI_soilP_site_ME.html")
webshot("EVI_soilP_site_ME.html", "EVI_soilP_site_ME.png")

# Determine best EVI mixed effects model using AIC
#AIC(EVIsoilPC) # 17
AIC(EVIsoilPS) # 14

