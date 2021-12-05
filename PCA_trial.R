# Dellena Bloom
# November, 22nd, 2021
# trying PCA for the NGEE-tropics project

# load libraries
library(tidyverse)
library(factoextra)

# Dellena file path
data_directory1 <- '~/Documents/GitHub/NGEEtropics_SULI_spring'
data_directory2 <- '~/Documents/Current Kueppers'

# load data
VI <- read.csv(file.path(data_directory1, "case_study_data.csv"), stringsAsFactors = FALSE)

# select columns for analysis
VI.PCA <- VI[,c("Site", "Holdridge_ID", "soil.P", "change_annual_LAI_500m", 
                 "peak_wind_speed_ms", "Cyclone_name")]

# write csv to add ID columns for non-numeric variables
write.csv(VI.PCA, file = "~/Documents/Current Kueppers/case_study_for_PCA.csv")

# get number of unique variable names for non-numeric categories
unique(levels(as.factor(VI.PCA$Site)))
unique(levels(as.factor(VI.PCA$Cyclone_name)))

# read in new csv
VI2 <- read.csv(file.path(data_directory2, "case_study_for_PCA.csv"), na.strings = "-9999")

# select only numeric columns from VI2
VI2.PCA <- VI2[,c("change_annual_LAI_500m", "Holdridge_ID", "soil.P",  
                 "peak_wind_speed_ms")]
# include change in litterfall (calculate and create a column), longitude, 
# cyclone frequency, Gale wind duration, time since last storm, MAT/MAP 
# (climate variable), parent material, cyclone rainfall, elevation
# maybe include a few other VIs and see if its related to the resolution or 
# if its due to the VI
# must add to the csv: 

# select for non-NA values
VI2.PCA.B <- VI2.PCA %>%
  drop_na()

# compute PCA
VI.pca <- prcomp(VI2.PCA.B, scale = TRUE)

# visualize PCA components with Scree plot
fviz_eig(VI.pca)

# visualize PCA with PCA plot
fviz_pca_var(VI.pca,
             #col.var = "contrib", # Color by contributions to the PC
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# visualize PCA with biplot
fviz_pca_biplot(VI.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

