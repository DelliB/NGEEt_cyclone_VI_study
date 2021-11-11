# Dellena Bloom
# November 8-11th, 2021
# Checking metadata ranges and categories for metadata

# load packages
library(tidyverse)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

## Case-study metadata
# load data
VI <- read.csv(file.path(data_directory, "case_study_data.csv"), stringsAsFactors = FALSE, na.strings = '-9999')

# summary of VI data-frame
summary(VI)

# categorical variables
unique(levels(as.factor(VI$Authors)))
unique(levels(as.factor(VI$Basin)))
unique(levels(as.factor(VI$Region)))
unique(levels(as.factor(VI$Area)))
unique(levels(as.factor(VI$Cyclone_name)))
unique(levels(as.factor(VI$Site)))
unique(levels(as.factor(VI$Forest_type)))
unique(levels(as.factor(VI$Holdridge_life_zone)))


## MODIS-recovery metadata
# load data
MR <- read.csv(file.path(data_directory, "MODIS_recovery.csv"), stringsAsFactors = FALSE, na.strings = '-9999')

# summary of VI data-frame
summary(MR)

# categorical variables
unique(levels(as.factor(MR$region)))


## Landsat-recovery metadata
# load data
LR <- read.csv(file.path(data_directory, "Landsat_recovery.csv"), stringsAsFactors = FALSE, na.strings = '-9999')

# summary of VI data-frame
summary(LR)

# categorical variables
unique(levels(as.factor(LR$region)))


