# Dellena Bloom
# July 12th, 2021
# Semi-partial and partial correlations

### Organizing Environment
# load packages
library(ppcor)
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggcorrplot)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
VI <- read.csv(file.path(data_directory, "case_study_data.csv"), stringsAsFactors = FALSE)

# clean data
uncleaned_data <- cbind(VI$peak_wind_speed_ms, VI$change_annual_LAI_500m, 
                        VI$soil.P..mg.kg.)
cleaned_data <- na.omit(uncleaned_data)
names <- c("Windspeed","LAIchange","soilP")
colnames(cleaned_data) <- names
cleaned_data <- as.data.frame(cleaned_data)

## Partial Correlation
# Calculates the correlation between change in LAI and soil P while controlling for 
# the wind speed
pcor.test(cleaned_data$LAIchange, cleaned_data$soilP, 
          cleaned_data$Windspeed, method="pearson")
# Result: not statistically significant, but the outcome table indicates that the 
# partial correlation between change in LAI and soil P after entirely controlling 
# for the effects of sum of learning = -0.262.
# Overall, the partial correlation coefficient is a measure of the strength of the 
# linear relationship between two variables after entirely controlling for the 
# effects of other variables.


## Semi-Partial Correlation
# Calculates the correlation between all change in LAI and the parts of soil P
# that are independent of wind speed
spcor.test(cleaned_data$LAIchange, cleaned_data$soilP, 
           cleaned_data$Windspeed, method="pearson")
# Result: not significant, but the outcome table indicates that the semi-partial 
# correlation between all of change in LAI and that part of soil P 
# which is independent of wind speed = -0.196.
# Overall, the semi-partial correlation coefficient is the correlation between 
# all of Y and that part of X which is independent of Z. That is the effect of Z 
# has been removed from X but not from Y.




