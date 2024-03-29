# Dellena Bloom
# November, 22nd, 2021
# trying PCA for the NGEE-tropics project

# load libraries
library(tidyverse)
library(factoextra)
library(ggbiplot)
library(ggcorrplot)
library(ggpubr)
library(multcompView)

# Dellena file path
#data_directory1 <- '~/Documents/GitHub/NGEEtropics_SULI_spring'
data_directory <- '~/Documents/Kueppers Lab'

# load data
VI <- read.csv(file.path(data_directory, "case_study_data.csv"), stringsAsFactors = FALSE, na.strings = '-9999')

## Calculating change in LF
# annual TL
VI$change_annual_total_litterfall = log(VI$Response_Post_Mean_Tot_Litterfall 
                                        / VI$Pre_Mean_Tot_Litterfall_g.m2.day)


# select columns for analysis
names(VI)



# LAI
#VI.PCA <- VI %>% select() # another way to select the columns
VI.PCA <- VI[,c("change_annual_LAI_500m", "Holdridge_ID", "soil_P", "Longitude", "gale.wind.duration..minutes.",
                "peak_wind_speed_ms", "change_annual_total_litterfall", 
                "Years_since_last_storm", "Elevation_m", "Cyclone_frequency", "Parent_material", "MAT_MAP_ratio_X100",
                "Cyclone_Rainfall_mm", "Region", "N")]

# drop NAs
VI.PCA <- VI.PCA %>%
  drop_na()

# write csv to add ID columns for non-numeric variables
write.csv(VI.PCA, file = "~/Documents/Kueppers Lab/case_study_for_PCA.csv")

# get number of unique variable names for non-numeric categories
unique(levels(as.factor(VI.PCA$Parent_material)))
#unique(levels(as.factor(VI.PCA$Cyclone_name)))

# read in new csv
VI2 <- read.csv(file.path(data_directory, "case_study_for_PCA.csv"), na.strings = "NA")

# select only numeric columns and remove parent materials
VI2.PCA <- VI2[,c("change_annual_LAI_500m", "Holdridge_ID", "soil_P", "Longitude", "gale.wind.duration..minutes.",
                "peak_wind_speed_ms", "change_annual_total_litterfall",
                "Years_since_last_storm", "Elevation_m", "Cyclone_frequency", "Parent_material_ID", "MAT_MAP_ratio_X100",
                "Cyclone_Rainfall_mm")]
# maybe include a few other VIs and see if its related to the resolution or 
# if its due to the VI

# rename columns
VI2.PCA <- dplyr::rename(VI2.PCA, "change annual LAI (500m)" = change_annual_LAI_500m)
VI2.PCA <- dplyr::rename(VI2.PCA, "Holdridge Life Zone" = Holdridge_ID)
VI2.PCA <- dplyr::rename(VI2.PCA, "soil phosphorus" = soil_P)
VI2.PCA <- dplyr::rename(VI2.PCA, "wind duration" = gale.wind.duration..minutes.)
VI2.PCA <- dplyr::rename(VI2.PCA, "wind speed" = peak_wind_speed_ms)
VI2.PCA <- dplyr::rename(VI2.PCA, "change TL" = change_annual_total_litterfall)
VI2.PCA <- dplyr::rename(VI2.PCA, "years since last storm" = Years_since_last_storm)
VI2.PCA <- dplyr::rename(VI2.PCA, "elevation" = Elevation_m)
VI2.PCA <- dplyr::rename(VI2.PCA, "cyclone frequency" = Cyclone_frequency)
VI2.PCA <- dplyr::rename(VI2.PCA, "parent material" = Parent_material_ID)
VI2.PCA <- dplyr::rename(VI2.PCA, "MAT:MAP ratio" = MAT_MAP_ratio_X100)
VI2.PCA <- dplyr::rename(VI2.PCA, "cyclone rainfall" = Cyclone_Rainfall_mm)

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

# to make plot with color
country <- c(rep("Caribbean", 2), rep("Mexico", 2), rep("Caribbean", 4), rep("Taiwan", 8), rep("Caribbean", 2), "Australia")
print(country)
LAI <- ggbiplot(VI.pca, ellipse = TRUE, obs.scale = 1, var.scale = 1, groups = country) +
  labs(color = "Region") +
  ylim(-4.8, 4.1) +
  xlim(-4.2, 5.5) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        legend.text = element_text(size = 13, color = "black"),
        legend.title = element_text(size = 13, color = "black"))
ggsave("PCA.png", width = 7, height = 6, path = data_directory)



# EVI
VI.PCA <- VI[,c("change_annual_EVI_250m", "Holdridge_ID", "soil_P", "Longitude", "gale.wind.duration..minutes.",
                "peak_wind_speed_ms", "change_annual_total_litterfall", 
                "Years_since_last_storm", "Elevation_m", "Cyclone_frequency", "Parent_material", "MAT_MAP_ratio_X100",
                "Cyclone_Rainfall_mm", "Region", "N")]

# drop NAs
VI.PCA <- VI.PCA %>%
  drop_na()

# write csv to add ID columns for non-numeric variables
write.csv(VI.PCA, file = "~/Documents/Kueppers Lab/case_study_for_PCA.csv")

# get number of unique variable names for non-numeric categories
unique(levels(as.factor(VI.PCA$Parent_material)))
#unique(levels(as.factor(VI.PCA$Cyclone_name)))

# read in new csv
VI2 <- read.csv(file.path(data_directory, "case_study_for_PCA.csv"), na.strings = "NA")

# select only numeric columns and remove parent materials
VI2.PCA <- VI2[,c("change_annual_EVI_250m", "Holdridge_ID", "soil_P", "Longitude", "gale.wind.duration..minutes.",
                  "peak_wind_speed_ms", "change_annual_total_litterfall",
                  "Years_since_last_storm", "Elevation_m", "Cyclone_frequency", "Parent_material_ID", "MAT_MAP_ratio_X100",
                  "Cyclone_Rainfall_mm")]
# maybe include a few other VIs and see if its related to the resolution or 
# if its due to the VI

# rename columns
VI2.PCA <- dplyr::rename(VI2.PCA, "change annual EVI (250m)" = change_annual_EVI_250m)
VI2.PCA <- dplyr::rename(VI2.PCA, "Holdridge Life Zone" = Holdridge_ID)
VI2.PCA <- dplyr::rename(VI2.PCA, "soil phosphorus" = soil_P)
VI2.PCA <- dplyr::rename(VI2.PCA, "wind duration" = gale.wind.duration..minutes.)
VI2.PCA <- dplyr::rename(VI2.PCA, "wind speed" = peak_wind_speed_ms)
VI2.PCA <- dplyr::rename(VI2.PCA, "change TL" = change_annual_total_litterfall)
VI2.PCA <- dplyr::rename(VI2.PCA, "years since last storm" = Years_since_last_storm)
VI2.PCA <- dplyr::rename(VI2.PCA, "elevation" = Elevation_m)
VI2.PCA <- dplyr::rename(VI2.PCA, "cyclone frequency" = Cyclone_frequency)
VI2.PCA <- dplyr::rename(VI2.PCA, "parent material" = Parent_material_ID)
VI2.PCA <- dplyr::rename(VI2.PCA, "MAT:MAP ratio" = MAT_MAP_ratio_X100)
VI2.PCA <- dplyr::rename(VI2.PCA, "cyclone rainfall" = Cyclone_Rainfall_mm)

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

# to make plot with color
country <- c(rep("Caribbean", 2), rep("Mexico", 2), rep("Caribbean", 4), rep("Taiwan", 8), rep("Caribbean", 2), "Australia")
print(country)
EVI <- ggbiplot(VI.pca, ellipse = TRUE, obs.scale = 1, var.scale = 1, groups = country) +
  labs(color = "Region") +
  ylim(-4.8, 4.1) +
  xlim(-4.2, 5.5) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        legend.position = "right",
        legend.text = element_text(size = 13, color = "black"),
        legend.title = element_text(size = 13, color = "black"))
ggsave("PCA.png", width = 7, height = 6, path = data_directory)


ggarrange(LAI, EVI, ncol = 2, labels = c("A", "B"))
ggsave("PCA_analysis.png", width = 10, height = 9, path = data_directory)


