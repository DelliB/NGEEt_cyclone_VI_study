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
#library(lme4)

# Dellena file path
#data_directory <- '~/Documents/Kueppers lab'
data_directory <- 'private/data'

# load data
VIs_sites <- read.csv(file.path(data_directory, "case_study_data.csv"), 
                      stringsAsFactors= FALSE, na.strings = '-9999') %>%
  drop_na(change_annual_LAI_500m)


## Calculating change in litter fall
# annual total litter fall
VIs_sites$change_annual_total_litterfall = log(
  VIs_sites$Response_Post_Mean_Tot_Litterfall/
    VIs_sites$Pre_Mean_Tot_Litterfall_g.m2.day)
# annual leaf litter fall
# VI$change_annual_leaf_litterfall = log(VI$Response_Post_Mean_Leaf_fall 
#                                        / VI$Pre_Mean_Leaf_fall)


### Stats
## Regressions
# LAI vs log transformed soil P
LAIsoilP <- lm(change_annual_LAI_500m ~ log(soil_P), data = VIs_sites)
summary(LAIsoilP)
tab_model(lme_ndvi)
ggsave("NDVI_MODIS_site_mixedeffects.png", width = 5, height = 5, path = data_directory)

# Change annual ground litter fall vs LAI
LAIaTL <- lm(change_annual_total_litterfall ~ change_annual_LAI_500m, data = VIs_sites)
summary(LAIaTL)


## Mixed effects
# Wind speed effects on LAI vs log transformed soil phosphorous
LAIsoilPWS <- lmer(change_annual_LAI_500m ~ log(soil_P) + (1|Region), data = VIs_sites)
summary(LAIsoilPWS)

# Wind speed effects on LAI vs change annual ground litter fall
LAIaTLWS <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + peak_wind_speed_ms 
                    + (1|Region), data = VIs_sites)
summary(LAIaTLWS)

# Site effects on LAI vs log transformed soil phosphorous
LAIsoilPS <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + (1|Region), data = VIs_sites)
summary(LAIsoilPS)

# Site effects on LAI vs change annual ground litter fall
LAIaTLS <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + (1|Region), data = VIs_sites)
summary(LAIaTLS)

