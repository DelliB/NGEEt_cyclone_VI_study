# Dellena Bloom
# November, 22nd, 2021
# trying PCA for the NGEE-tropics project

# load libraries
library(tidyverse)
library(factoextra)
library(ggbiplot)

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
country <- c("Australia", rep("Taiwan", 4), rep("Caribbean", 4), rep("Taiwan", 4), rep("Caribbean", 2), rep("Mexico", 2), rep("Caribbean", 2))
print(country)
ggbiplot(VI.pca, ellipse = TRUE, obs.scale = 1, var.scale = 1, groups = country) +
  labs(color = "Region") +
  ylim(-4.8, 4.1) +
  xlim(-4.2, 5.5) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 13, color = "black"),
        legend.title = element_text(size = 13, color = "black"))
ggsave("PCA.png", width = 7, height = 6, path = data_directory)




# Barbara's code
#packages
library(devtools)
install_github("vqv/ggbiplot")
library(grid)

#reading data
PCA_soil <- VI2.PCA.B#PCA_soil_updated (including averages 0-50 cm)
names(PCA_soil)
str(PCA_soil)#n=45 of 23 variables

#changing data types
PCA_soil$FT=as.factor(PCA_soil$FT)
PCA_soil$Plot=as.factor(PCA_soil$Plot)
PCA_soil$Depth=as.factor(PCA_soil$Depth)
head(PCA_soil)

#setting up dataframe with the desired columns from original dataset
biplot2<-PCA_soil[,c(1:14)]

names(biplot2)=c("change_annual_LAI_500m", "Holdridge_ID", "soil_P", "Longitude", "gale.wind.duration..minutes.",
                 "peak_wind_speed_ms", "change_annual_total_litterfall", "change_subannual_total_litterfall",
                 "Years_since_last_storm", "Elevation_m", "Cyclone_frequency", "Parent_material_ID", "MAT_MAP_ratio_X100",
                 "Cyclone_Rainfall_mm")
names(biplot2)

#creating vector with the code I desired to color the points with
#you can do the same for region instead of the numbers within " "
yearspl2<-c(rep("Caribbean",8),rep("Mexico", 2), rep("Taiwan",4))
yearspl2

#running the pca
pca_soil <- prcomp(biplot2, center = TRUE,scale. = TRUE)#standardized data
summary(pca_soil)
str(pca_soil)
pca_soil$rotation

#ploting the pca biplot
ggbiplot(pca_soil, varname.size=6.5,ellipse=TRUE,varname.abbrev=FALSE,labels.size = 7,groups=yearspl2,obs.scale=1)+theme_minimal()+theme(legend.position = "top")+       # Thicker line
  geom_point(aes(shape = yearspl2,color=yearspl2),         # Hollow squares
             size = 5.5)+
  scale_shape(solid=FALSE)+theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+labs(x="PC1 (48%)",y="PC2 (14%)")



