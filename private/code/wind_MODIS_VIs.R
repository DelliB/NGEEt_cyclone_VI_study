# Dellena Bloom
# July 2nd, 2021
# Plots with wind and change in VIs

### Organizing Environment
# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggcorrplot)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
VI <- read.csv(file.path(data_directory, "case_study_data.csv"), stringsAsFactors = FALSE)

# axis labels
x = expression(paste(log[10],"(soil P (mg/kg))"))
dTL1 = expression(Delta*"TL annual")
dTL2 = expression(Delta*"TL sub-annual")
dLL1 = expression(Delta*"LL annual")
dLL2 = expression(Delta*"LL sub-annual")
dNDVI1 = expression(Delta*"NDVI annual (250m)")
dNDVI2 = expression(Delta*"NDVI sub-annual (250m)")
dEVI1 = expression(Delta*"EVI annual (250m)")
dEVI2 = expression(Delta*"EVI sub-annual (250m)")
dLAI1 = expression(Delta*"LAI annual (500m)")
dLAI2 = expression(Delta*"LAI sub-annual (500m)")
wind = "Peak wind speed (m/s)"

## Plots
# NDVI
ggplot(VI, aes(x = peak_wind_speed_ms, y = change_annual_NDVI_250m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = wind, y = dNDVI1) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) 
ggsave("wind_speed_MODIS_NDVI.png", width = 6, height = 4, path = data_directory)

# EVI
ggplot(VI, aes(x = peak_wind_speed_ms, y = change_annual_EVI_250m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = wind, y = dEVI1) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("wind_speed_MODIS_EVI.png", width = 6, height = 4, path = data_directory)

# LAI
ggplot(VI, aes(x = peak_wind_speed_ms, y = change_annual_LAI_500m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = wind, y = dLAI1) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("wind_speed_MODIS_LAI.png", width = 6, height = 4, path = data_directory)



