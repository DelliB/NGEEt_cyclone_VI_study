# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggcorrplot)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
VI <- read.csv(file.path(data_directory, "case_study_data.csv"), stringsAsFactors = FALSE)

model1 <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms + soil.P..mg.kg., 
             data = VI)
summary(model1)
# try this model with only 0 and negative change in VI values

AIC(model1, model2)

shapiro.test(residuals(model1)) # significance is greater then 0.05, so we don't need
# to transform the soil P in model 1

model2 <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms + log(soil.P..mg.kg.), 
             data = VI)
summary(model2)

# created new dataframe; subsetting data
VInegative <- VI %>%
  filter(change_annual_LAI_500m < 0)

model3 <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms + soil.P..mg.kg., 
             data = VInegative)
#str(VInegative)
summary(model3)

model1b <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms, data = VI)
summary(model1b)

model1c <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms, data = VInegative)
summary(model1c)

model1bsp <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms + soil.P..mg.kg., 
                data = VI)
summary(model1bsp)

lmer # from report and we did this together
# what factors repeat the most? and use this to decide what is the random effect
# could be region, site, cylone

model1csp <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms + soil.P..mg.kg., 
                data = VInegative)
summary(model1csp)


## July 2nd, 2021
# wind speed in the VI and soil P relationship
EVIsPws <- lm(change_annual_EVI_250m ~ (soil.P..mg.kg.) * peak_wind_speed_ms, data = VI)
summary(EVIsPws)

NDVIsPws <- lm(change_annual_NDVI_250m ~ (soil.P..mg.kg.) * peak_wind_speed_ms, data = VI)
summary(NDVIsPws)

LAIsPws <- lm(change_annual_LAI_500m ~ (soil.P..mg.kg.) * peak_wind_speed_ms, data = VI)
summary(LAIsPws)

# soil P in the VI and wind speed relationship
EVIsPws2 <- lm(change_annual_EVI_250m ~ peak_wind_speed_ms * (soil.P..mg.kg.), data = VI)
summary(EVIsPws2)

NDVIsPws2 <- lm(change_annual_NDVI_250m ~ peak_wind_speed_ms * (soil.P..mg.kg.), data = VI)
summary(NDVIsPws2)

LAIsPws2 <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms * (soil.P..mg.kg.), data = VI)
summary(LAIsPws2)

### Mixed effects
## load packages
library(lme4)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyverse)

## Models
# NDVI
# include just site, do not need the cyclone name
# include the wind speed and phosphorus together with the delta VI
lme_ndvi <- lmer(change_annual_NDVI_250m ~ peak_wind_speed_ms + (1|Site), 
                 data = VI)
summary(lme_ndvi)
tab_model(lme_ndvi)
ggsave("NDVI_MODIS_site_mixedeffects.png", width = 5, height = 5, path = data_directory)

lme_ndvi2 <- lmer(change_annual_NDVI_250m ~ log(soil.P..mg.kg.) + 
                   (1|Cyclone_name), data = VI)
summary(lme_ndvi2)
tab_model(lme_ndvi2)
ggsave("NDVI_MODIS_distname_mixedeffects.png", width = 5, height = 5, path = data_directory)

# EVI
lme_evi <- lmer(change_annual_EVI_250m ~ log(soil.P..mg.kg.) + (1|Site), 
                 data = VI)
summary(lme_evi)
tab_model(lme_evi)
ggsave("EVI_MODIS_site_mixedeffects.png", width = 5, height = 5, path = data_directory)

lme_evi2 <- lmer(change_annual_EVI_250m ~ log(soil.P..mg.kg.) + 
                  (1|Cyclone_name), data = VI)
summary(lme_evi2)
tab_model(lme_evi2)
ggsave("EVI_MODIS_distname_mixedeffects.png", width = 5, height = 5, path = data_directory)

# LAI
# include this in the powerpoint for Lara as well, with the table
lme_lai <- lmer(change_annual_LAI_500m ~ peak_wind_speed_ms + (1|Site), 
                 data = VI)
summary(lme_lai)
tab_model(lme_lai)
ggsave("LAI_MODIS_site_mixedeffects.png", width = 5, height = 5, path = data_directory)

lme_lai2 <- lmer(change_annual_LAI_250m ~ log(soil.P..mg.kg.) + 
                  (1|Cyclone_name), data = VI)
summary(lme_lai2)
tab_model(lme_lai2)
ggsave("LAI_MODIS_distname_mixedeffects.png", width = 5, height = 5, path = data_directory)

# Model for delta VI and wind speed
LAIws <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms, data = VI)
summary(LAIws)

NDVIws <- lm(change_annual_NDVI_250m ~ peak_wind_speed_ms, data = VI)
summary(NDVIws)

EVIws <- lm(change_annual_EVI_250m ~ peak_wind_speed_ms, data = VI)
summary(EVIws)



