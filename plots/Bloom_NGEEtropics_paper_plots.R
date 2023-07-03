# Dellena Bloom
# February 3rd, 2022
# NGEE-tropics paper plots


### Organizing Environment
## Load packages and data
# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggcorrplot)
library(factoextra)
library(ggbiplot)
#library(packcircles)

# Dellena file path
#data_directory <- '~/Documents/Kueppers lab'
data_directory <- 'private/data'

# load data
recovery <- read.csv(file.path(data_directory, "MODIS_recovery.csv"), 
                     stringsAsFactors= FALSE, na.strings = '-9999')
LFreco <- read.csv(file.path(data_directory, "MODIS_recovery_bb.csv"), 
                   stringsAsFactors= FALSE)
VIs_sites <- read.csv(file.path(data_directory, "case_study_data.csv"), 
                      stringsAsFactors= FALSE, na.strings = '-9999') %>%
  drop_na(change_annual_LAI_500m)


## Axis labels
# Change in LAI and EVI
EVI = expression(Delta*"EVI annual (250m)")
LAI = expression(Delta*"LAI annual (500m)")
# Litter fall
dTL1 = expression(Delta*"TL annual")
dLL1 = expression(Delta*"LL annual")
# Recovery
LAIreco = expression(Delta*"LAI 500m [LAI/LAI]")
EVIreco = expression(Delta*"EVI 250m [EVI/EVI]")
time = "Time since disturbance (months)"
# Soil phosphorus
sP = expression(paste(log[10],"(soil P (mg/kg))"))


## Calculating change in litter fall
# annual total litter fall
VIs_sites$change_annual_total_litterfall = log(
  VIs_sites$Response_Post_Mean_Tot_Litterfall/
    VIs_sites$Pre_Mean_Tot_Litterfall_g.m2.day)
# annual leaf litter fall
# VI$change_annual_leaf_litterfall = log(VI$Response_Post_Mean_Leaf_fall 
#                                        / VI$Pre_Mean_Leaf_fall)


### Plots
## Figure 2: Point plot of change in EVI and change in LAI
# Adding case study column with site and cyclone name
VIs_sites_map <- VIs_sites %>%
  mutate(case_study_names = paste(Site, "|", Cyclone_name))

## LAI and EVI separate
# LAI
LAI_cases <- ggplot(VIs_sites_map, aes(x = change_annual_LAI_500m, y = 
                                    case_study_names, color = Site)) +
  geom_point(size = 3) +
  geom_vline(xintercept=c(0), color = "gray", linetype = "dashed") +
  labs(y = "", x = LAI) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0.25, y = 1,
           label = c("Baseline"))

# EVI
EVI_cases <- ggplot(VIs_sites_map, aes(x = change_annual_EVI_250m, y = 
                                         case_study_names, color = Site)) +
  geom_point(size = 3) +
  geom_vline(xintercept=c(0), color = "gray", linetype = "dashed") +
  labs(y = "", x = EVI) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0.2, y = 1,
           label = c("Baseline"))

# Join figures together and save
Cases_LAI_EVI <- ggarrange(LAI_cases, EVI_cases, nrow = 1, ncol = 2, 
                        labels = c("A","B"))
print(Cases_LAI_EVI)
ggsave("NGEE_paper_Fig_2.png", width = 11, height = 6, 
       path = data_directory, dpi = 300, units = "in")

## LAI and EVI together
# Making column with both LAI and EVI
VIs_sites_VIcombos <- VIs_sites_map %>%
  pivot_longer(cols = c(change_annual_EVI_250m, change_annual_LAI_500m),
               names_to = "VIs", values_to = "value")

VIs_sites_VIcombos["VIs"][VIs_sites_VIcombos["VIs"] == "change_annual_EVI_250m"] <- "Change in EVI (250m)"
VIs_sites_VIcombos["VIs"][VIs_sites_VIcombos["VIs"] == "change_annual_LAI_500m"] <- "Change in LAI (500m)"

# Plot
ggplot(VIs_sites_VIcombos, aes(x = value, y = case_study_names, color = VIs)) +
  geom_point(size = 3) +
  geom_vline(xintercept=c(0), color = "gray", linetype = "dashed") +
  labs(y = "", x = LAI, color = "") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 13, color = "Black"),
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0.25, y = 1, label = c("Baseline"))

# Save figure
ggsave("NGEE_paper_Fig_2_v2.png", width = 8, height = 6, 
       path = data_directory, dpi = 300, units = "in")


## Figure 3: Litter fall (total and leaf) and change in LAI and EVI correlation
# LAI
TLLAI <- ggplot(VIs_sites, aes(x = change_annual_LAI_500m, y = 
                          change_annual_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  labs(y = dTL1, x = LAI) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -0.2, y = -.30,
           label = c("Baseline"))

# LLLAI <- ggplot(VIs_sites, aes(x = change_annual_LAI_500m, y = 
#                           change_annual_leaf_litterfall)) +
#   geom_point(size = 3) +
#   geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
#   stat_cor(method = "pearson", size = 4,
#            aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
#   labs(y = dLL1, x = LAI) +
#   geom_vline(xintercept=c(0), color = "gray") +  
#   geom_hline(yintercept=c(0), color = "gray") +
#   theme_bw() +
#   theme(panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.position = "none",
#         axis.title = element_text(size = 13, color = "Black"),
#         axis.text = element_text(size = 13, color = "Black"),
#         panel.grid.minor = element_blank())

# EVI
TLEVI <- ggplot(VIs_sites, aes(x = change_annual_EVI_250m, y = 
                          change_annual_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  labs(y = "", x = EVI) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -0.2, y = -.3,
           label = c("Baseline"))

# LLEVI <- ggplot(VIs_sites, aes(x = change_annual_EVI_250m, y = 
#                           change_annual_leaf_litterfall)) +
#   geom_point(size = 3) +
#   geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
#   stat_cor(method = "pearson", size = 4,
#            aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
#   labs(y = dLL1, x = EVI) +
#   geom_vline(xintercept=c(0), color = "gray") +  
#   geom_hline(yintercept=c(0), color = "gray") +
#   theme_bw() +
#   theme(panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.position = "none",
#         axis.title = element_text(size = 13, color = "Black"),
#         axis.text = element_text(size = 13, color = "Black"),
#         panel.grid.minor = element_blank())

# Join figures together and save
# LF_LAI_EVI <- ggarrange(TLLAI, LLLAI, TLEVI, LLEVI, nrow = 2, ncol = 2, 
#                         labels = c("A","B","C","D"))
LF_LAI_EVI <- ggarrange(TLLAI, TLEVI, nrow = 1, ncol = 2, 
                        labels = c("A","B"))
print(LF_LAI_EVI)
ggsave("NGEE_paper_Fig_3.png", width = 7, height = 6, 
       path = data_directory, dpi = 300, units = "in")


## Figure 4: Recovery of LAI and EVI
# Inclusion of litter fall
LFerror <- LFreco$Pantropical_Litterfall_Std_dev/
  sqrt(LFreco$Number_Case_Studies)
LFreco$min = LFreco$Pantropical_Litterfall_Mean - (1.96*LFerror)
LFreco$max = LFreco$Pantropical_Litterfall_Mean + (1.96*LFerror)

# LAI
# 95% CI ribbon calculation
LAIm <- recovery %>%
  drop_na() %>%
  group_by(month) %>%
  summarize(m = mean(LAI_change_500m),
            SD = sd(LAI_change_500m),
            n = length(LAI_change_500m))
LAIm <- merge(LAIm, recovery, by = "month")
error <- LAIm$SD/sqrt(LAIm$n)

# upper and lower ribbon bounds
LAIm$min = LAIm$m - (1.96*error)
LAIm$max = LAIm$m + (1.96*error)

# plot
LAIreco <- ggplot(LAIm, aes(x = month, y = LAI_change_500m, group = case_study, color = region)) +
  geom_line(data = LAIm[!is.na(LAIm$LAI_change_500m),], linetype = "dashed") +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical Δ LAI"), lwd=1) +
  geom_ribbon(data = LFreco, aes(x = month, ymin = CI_lower_bound, ymax = CI_upper_bound), 
              fill = "gray", alpha = 0.25, color = NA) +
  geom_line(data = LFreco, aes(x = month, y = Pantropical_Litterfall_Mean, 
                               color = "Pantropical litterfall"), lwd=1) +
  labs(x = "", y = LAIreco, color = "Region") +
  ylim(-1.5, 1.5) +
  geom_hline(yintercept=c(0), color = "gray") +
  #scale_color_manual(values = c("red2", "yellow3", "springgreen4", "black", 
  #                             "deepskyblue3", "orchid")) +
  scale_colour_manual("Region", 
                      breaks = c("Australia", "Caribbean", "Mexico", 
                                 "Pantropical litterfall", "Pantropical Δ LAI", 
                                 "Pantropical Δ EVI", "Taiwan"),
                      values = c("Australia"="red2", "Caribbean"="yellow3", 
                                 "Mexico"="springgreen4", "Pantropical litterfall"=
                                   "black", "Pantropical Δ LAI"="deepskyblue3", 
                                 "Pantropical Δ EVI"= "purple4", "Taiwan"="orchid")) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 13, color = "black"),
        legend.text = element_text(size = 13, color = "black"),
        plot.title = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  #labs(caption = "Solid lines are pantropical mean and shaded areas represent 95% CI") +
  guides(color = guide_legend(ncol=4, byrow=FALSE))

# EVI
# 95% CI ribbon calculation
EVIm <- recovery %>%
  drop_na() %>%
  group_by(month) %>%
  summarize(m = mean(EVI_change_250m),
            SD = sd(EVI_change_250m),
            n = length(EVI_change_250m))
EVIm <- merge(EVIm, recovery, by = "month")
error <- EVIm$SD/sqrt(EVIm$n)

# upper and lower ribbon bounds
EVIm$min = EVIm$m - (1.96 * error)
EVIm$max = EVIm$m + (1.96 * error)

# plot
EVIreco <- ggplot(EVIm, aes(x = month, y = EVI_change_250m, group = 
                              case_study, color = region)) +
  geom_line(data = EVIm[!is.na(EVIm$EVI_change_250m),], linetype = "dashed") +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "purple1", alpha = 0.02, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical Δ EVI"), lwd=1) +
  geom_ribbon(data = LFreco, aes(x = month, ymin = CI_lower_bound, ymax = 
                                   CI_upper_bound), 
              fill = "gray", alpha = 0.25, color = NA) +
  geom_line(data = LFreco, aes(x = month, y = Pantropical_Litterfall_Mean, 
                               color = "Pantropical litterfall"), lwd=1) +
  labs(x = time, y = EVIreco, color = "Region") +
  scale_y_continuous(breaks=c(-1, 0, 1)) +
  geom_hline(yintercept=c(0), color = "gray") +
  scale_color_manual(values = c("red2", "yellow3", "springgreen4", "black", 
                                "purple4", "orchid")) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        legend.title = element_text(size = 13, color = "black"),
        legend.text = element_text(size = 13, color = "black"),
        plot.title = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        plot.caption = element_text(size = 10, color = "black"),
        panel.grid.minor = element_blank()) +
  labs(caption = "Solid lines are pantropical mean and shaded areas 
       represent 95% CI")

# Join figures together and save
reco_EVI_LAI <- ggarrange(LAIreco, EVIreco, nrow = 2, labels = c("A", "B"))
print(reco_EVI_LAI)
ggsave("NGEE_paper_Fig_4.png", width = 8, height = 7, path = data_directory)


## Figure 5: Soil phosphorus and change in LAI and EVI correlation
# LAI
PLAI <- ggplot(VIs_sites, aes(x = log10(soil_P), y = 
                                change_annual_LAI_500m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(1.8, 3.5) +
  labs(x = sP, y = LAI) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 3.25, y = 0.05,
           label = c("Baseline"))

# EVI
PEVI <- ggplot(VIs_sites, aes(x = log10(soil_P), 
                              y = change_annual_EVI_250m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(1.8, 3.5) +
  labs(x = sP, y = EVI) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 1.98, y = 0.3,
           label = c("n = 21"))

# Join figures together and save
P_LAI_EVI <- ggarrange(PLAI, PEVI, nrow = 1, ncol = 2, 
                        labels = c("A","B"))
print(P_LAI_EVI)
ggsave("NGEE_paper_Fig_5.png", width = 6, height = 4, 
       path = data_directory, dpi = 300, units = "in")


## Figure 6: PCA results for LAI and EVI
# LAI
# selecting columns
VI.PCA <- VIs_sites[,c("change_annual_LAI_500m", "Holdridge_ID", "soil_P", 
                "Longitude", "Region")] #, "gale.wind.duration..minutes.",
                # "peak_wind_speed_ms", "change_annual_total_litterfall",
                # "Years_since_last_storm", "Elevation_m", "Cyclone_frequency",
                # "Parent_material", "MAT_MAP_ratio_X100",
                # "Cyclone_Rainfall_mm", "Region", "N")]

# drop NAs
VI.PCA <- VI.PCA %>%
  drop_na()

# adds numeric factor column for parent material
#VI2 <- transform(VI.PCA, Parent_material_ID = as.numeric
#                 (factor(Parent_material)))

# select only numeric columns and remove parent materials
VI2.PCA <- VI.PCA[,c("change_annual_LAI_500m", "Holdridge_ID", "soil_P", 
                  "Longitude")] #, "gale.wind.duration..minutes.",
                  # "peak_wind_speed_ms", "change_annual_total_litterfall",
                  # "Years_since_last_storm", "Elevation_m", "Cyclone_frequency",
                  # "Parent_material_ID", "MAT_MAP_ratio_X100",
                  # "Cyclone_Rainfall_mm")]

# rename columns
VI2.PCA <- dplyr::rename(VI2.PCA, "change annual LAI (500m)" = 
                           change_annual_LAI_500m)
VI2.PCA <- dplyr::rename(VI2.PCA, "Holdridge Life Zone" = Holdridge_ID)
VI2.PCA <- dplyr::rename(VI2.PCA, "soil phosphorus" = soil_P)
# VI2.PCA <- dplyr::rename(VI2.PCA, "wind duration" =
#                            gale.wind.duration..minutes.)
# VI2.PCA <- dplyr::rename(VI2.PCA, "wind speed" = peak_wind_speed_ms)
# VI2.PCA <- dplyr::rename(VI2.PCA, "change TL" = change_annual_total_litterfall)
# VI2.PCA <- dplyr::rename(VI2.PCA, "years since last storm" =
#                            Years_since_last_storm)
# VI2.PCA <- dplyr::rename(VI2.PCA, "elevation" = Elevation_m)
# VI2.PCA <- dplyr::rename(VI2.PCA, "cyclone frequency" = Cyclone_frequency)
# VI2.PCA <- dplyr::rename(VI2.PCA, "parent material" = Parent_material_ID)
# VI2.PCA <- dplyr::rename(VI2.PCA, "MAT:MAP ratio" = MAT_MAP_ratio_X100)
# VI2.PCA <- dplyr::rename(VI2.PCA, "cyclone rainfall" = Cyclone_Rainfall_mm)

# compute PCA
VI.pca <- prcomp(VI2.PCA, scale = TRUE)

# compute eigenvalue for variable selection
eigenvalue <- VI.pca$sdev ^ 2

# select for eigenvalue > 1
eigenvalue2 <- eigenvalue > 1
#eigenvalue3 <- eigenvalue2 %>%
#  as.numeric()
#eigenvalue4 <- eigenvalue3 == 1

# include countries from VI2 or VI.PCA in the order that they appear in the csv
country <- c(rep("Caribbean", 2), rep("Mexico", 2), rep("Caribbean", 4), 
             rep("Taiwan", 10), rep("Caribbean", 2), "Australia")
print(country)

# plot
LAI_PCA <- ggbiplot(VI.pca, ellipse = TRUE, obs.scale = 1, var.scale = 1, 
                    groups = country) +
  labs(color = "Region") +
  ylim(-5.2, 4.1) +
  xlim(-4.2, 5.5) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        legend.text = element_text(size = 13, color = "black"),
        legend.title = element_text(size = 13, color = "black"))
print(LAI_PCA)


# EVI
# selecting columns
VI.PCA <- VIs_sites[,c("change_annual_EVI_250m", "Holdridge_ID", "soil_P", 
                "Longitude", "Region")] #, "gale.wind.duration..minutes.",
                # "peak_wind_speed_ms", "change_annual_total_litterfall", 
                # "Years_since_last_storm", "Elevation_m", "Cyclone_frequency", 
                # "Parent_material", "MAT_MAP_ratio_X100",
                # "Cyclone_Rainfall_mm", "Region", "N")]

# drop NAs
VI.PCA <- VI.PCA %>%
  drop_na()

# adds numeric factor column for parent material
# VI2 <- transform(VI.PCA, Parent_material_ID = as.numeric
#                  (factor(Parent_material)))

# select only numeric columns and remove parent materials
VI2.PCA <- VI.PCA[,c("change_annual_EVI_250m", "Holdridge_ID", "soil_P", 
                  "Longitude")] #, "gale.wind.duration..minutes.",
                  # "peak_wind_speed_ms", "change_annual_total_litterfall",
                  # "Years_since_last_storm", "Elevation_m", "Cyclone_frequency", 
                  # "Parent_material_ID", "MAT_MAP_ratio_X100",
                  # "Cyclone_Rainfall_mm")]

# rename columns
VI2.PCA <- dplyr::rename(VI2.PCA, "change annual EVI (250m)" = 
                           change_annual_EVI_250m)
VI2.PCA <- dplyr::rename(VI2.PCA, "Holdridge Life Zone" = Holdridge_ID)
VI2.PCA <- dplyr::rename(VI2.PCA, "soil phosphorus" = soil_P)
# VI2.PCA <- dplyr::rename(VI2.PCA, "wind duration" = 
#                            gale.wind.duration..minutes.)
# VI2.PCA <- dplyr::rename(VI2.PCA, "wind speed" = peak_wind_speed_ms)
# VI2.PCA <- dplyr::rename(VI2.PCA, "change TL" = change_annual_total_litterfall)
# VI2.PCA <- dplyr::rename(VI2.PCA, "years since last storm" = 
#                            Years_since_last_storm)
# VI2.PCA <- dplyr::rename(VI2.PCA, "elevation" = Elevation_m)
# VI2.PCA <- dplyr::rename(VI2.PCA, "cyclone frequency" = Cyclone_frequency)
# VI2.PCA <- dplyr::rename(VI2.PCA, "parent material" = Parent_material_ID)
# VI2.PCA <- dplyr::rename(VI2.PCA, "MAT:MAP ratio" = MAT_MAP_ratio_X100)
# VI2.PCA <- dplyr::rename(VI2.PCA, "cyclone rainfall" = Cyclone_Rainfall_mm)

# compute PCA
VI.pca <- prcomp(VI2.PCA, scale = TRUE)

# compute eigenvalue for variable selection
eigenvalue <- VI.pca$sdev ^ 2

# select for eigenvalue > 1
eigenvalue2 <- eigenvalue > 1

# include countries from VI2 in the order that they appear in the csv
country <- c(rep("Caribbean", 2), rep("Mexico", 2), rep("Caribbean", 4), 
             rep("Taiwan", 10), rep("Caribbean", 2), "Australia")
print(country)

# plot
EVI_PCA <- ggbiplot(VI.pca, ellipse = TRUE, obs.scale = 1, var.scale = 1, 
         groups = country) +
  labs(color = "Region") +
  ylim(-4.6, 4.7) +
  xlim(-4.2, 5.5) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        legend.text = element_text(size = 13, color = "black"),
        legend.title = element_text(size = 13, color = "black"))
print(EVI_PCA)

# Join figures together and save
LAI_EVI_PCA <- ggarrange(LAI_PCA, EVI_PCA, ncol = 2, labels = c("A", "B"), 
                         common.legend = TRUE, legend="right")
print(LAI_EVI_PCA)
ggsave("NGEE_paper_Fig_6.png", width = 11, height = 9, 
       path = data_directory, dpi = 300, units = "in")




