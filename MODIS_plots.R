# Dellena Bloom
# June 28th to July 7th, 2021
# Scatter plots for MODIS data results and correlation plot

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

## Calculating change in LF
# annual TL
VI$change_annual_total_litterfall = log(VI$Response_Post_Mean_Tot_Litterfall 
                                  / VI$Pre_Mean_Tot_Litterfall_g.m2.day)
# annual LL
VI$change_annual_leaf_litterfall = log(VI$Response_Post_Mean_Leaf_fall 
                                 / VI$Pre_Mean_Leaf_fall)
# sub-annual TL
VI$change_subannual_total_litterfall = log(VI$Response_Post_Mean_Tot_Litterfall
                                 / VI$Subannual_Pre_mean_Tot_Litterfall)
# sub-annual LL
VI$change_subannual_leaf_litterfall = log(VI$Response_Post_Mean_Leaf_fall 
                                / VI$Subannual_Pre_Mean_Leaf_fall)

### Scatter plots
## NDVI
# Change in litterfall and delta NDVI
TLNDVI1 <- ggplot(VI, aes(x = change_annual_NDVI_250m, y = change_annual_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  labs(y = dTL1, x = dNDVI1) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -0.2, y = -.3,
           label = c("Baseline"))

TLNDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI_250m, y = change_subannual_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL2, x = dNDVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLNDVI1 <- ggplot(VI, aes(x = change_annual_NDVI_250m, y = change_annual_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dNDVI1) +
  geom_vline(xintercept=c(0), color = "gray") +  
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLNDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI_250m, y = change_subannual_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL2, x = dNDVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

TL <- ggarrange(TLNDVI1, TLNDVI2, ncol = 2, labels = c("a", "b"))
LL <- ggarrange(LLNDVI1, LLNDVI2, ncol = 2, labels = c("c", "d"))
LF_NDVI <- ggarrange(TL, LL, nrow = 2, labels = c("", ""))

print(LF_NDVI)
ggsave("MODIS_Results_TL_LL_NDVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta NDVI
PNDVI1 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_NDVI_250m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4.2, 7.9) +
  labs(x = x, y = dNDVI1) +
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
  annotate("text", x = 7, y = 0.025,
           label = c("Baseline"))

PNDVI2 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_NDVI_250m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4.2, 7.9) +
  labs(x = x, y = dNDVI2) +
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

ggarrange(PNDVI1, PNDVI2, ncol = 2, labels = c("a", "b"))
ggsave("MODIS_Results_soilP_NDVI.png", width = 6, height = 4, path = data_directory)


## EVI
# Change in litterfall and delta EVI
TLEVI1 <- ggplot(VI, aes(x = change_annual_EVI_250m, y = change_annual_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  labs(y = dTL1, x = dEVI1) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -0.2, y = -.3,
           label = c("Baseline"))

TLEVI2 <- ggplot(VI, aes(x = change_sub.annual_EVI_250m, y = change_subannual_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL2, x = dEVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLEVI1 <- ggplot(VI, aes(x = change_annual_EVI_250m, y = change_annual_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dEVI1) +
  geom_vline(xintercept=c(0), color = "gray") +  
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLEVI2 <- ggplot(VI, aes(x = change_sub.annual_EVI_250m, y = change_subannual_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL2, x = dEVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

TL <- ggarrange(TLEVI1, TLEVI2, ncol = 2, labels = c("a", "b"))
LL <- ggarrange(LLEVI1, LLEVI2, ncol = 2, labels = c("c", "d"))
LF_EVI <- ggarrange(TL, LL, nrow = 2, labels = c("", ""))

print(LF_EVI)
ggsave("MODIS_Results_TL_LL_EVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta EVI
PEVI1 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_EVI_250m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4.2, 7.9) +
  labs(x = x, y = dEVI1) +
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
  annotate("text", x = 7, y = 0.025,
           label = c("Baseline"))

PEVI2 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_EVI_250m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4.2, 7.9) +
  labs(x = x, y = dEVI2) +
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

ggarrange(PEVI1, PEVI2, ncol = 2, labels = c("a", "b"))
ggsave("MODIS_Results_soilP_EVI.png", width = 6, height = 4, path = data_directory)


## LAI
# Change in litterfall and delta LAI
TLLAI1 <- ggplot(VI, aes(x = change_annual_LAI_500m, y = change_annual_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  labs(y = dTL1, x = dLAI1) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -0.2, y = -.30,
           label = c("Baseline"))

TLLAI2 <- ggplot(VI, aes(x = change_sub.annual_LAI_500m, y = change_subannual_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL2, x = dLAI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLLAI1 <- ggplot(VI, aes(x = change_annual_LAI_500m, y = change_annual_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dLAI1) +
  geom_vline(xintercept=c(0), color = "gray") +  
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLLAI2 <- ggplot(VI, aes(x = change_sub.annual_LAI_500m, y = change_subannual_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL2, x = dLAI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

TL <- ggarrange(TLLAI1, TLLAI2, ncol = 2, labels = c("a", "b"))
LL <- ggarrange(LLLAI1, LLLAI2, ncol = 2, labels = c("c", "d"))
LF_LAI <- ggarrange(TL, LL, nrow = 2, labels = c("", ""))

print(LF_LAI)
ggsave("MODIS_Results_TL_LL_LAI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta LAI
PLAI1 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_LAI_500m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4.2, 7.9) +
  labs(x = x, y = dLAI1) +
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
  annotate("text", x = 7, y = 0.05,
           label = c("Baseline"))

PLAI2 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_LAI_500m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4.2, 7.9) +
  labs(x = x, y = dLAI2) +
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

ggarrange(PLAI1, PLAI2, ncol = 2, labels = c("a", "b"))
ggsave("MODIS_Results_soilP_LAI.png", width = 6, height = 4, path = data_directory)


### Correlation plots
## Organizing data
# renaming columns
# VIs
VI <- VI %>%
  rename("annual change in NDVI 250m" = change_annual_NDVI_250m)
VI <- VI %>%
  rename("annual change in EVI 250m" = change_annual_EVI_250m)
VI <- VI %>%
  rename("annual change in LAI 500m" = change_annual_LAI_500m)
VI <- VI %>%
  rename("sub-annual change in NDVI 250m" = change_sub.annual_NDVI_250m)
VI <- VI %>%
  rename("sub-annual change in EVI 250m" = change_sub.annual_EVI_250m)
VI <- VI %>%
  rename("sub-annual change in LAI 500m" = change_sub.annual_LAI_500m)

# litterfall
VI <- VI %>%
  rename("annual change in TL" = change_annual_total_litterfall)
VI <- VI %>%
  rename("sub-annual change in TL" = change_subannual_total_litterfall)
VI <- VI %>%
  rename("annual change in LL" = change_annual_leaf_litterfall)
VI <- VI %>%
  rename("sub-annual change in LL" = change_subannual_leaf_litterfall)

# environmental variables
VI <- VI %>%
  rename("wind speed (kt)" = peak_wind_speed_ms)
VI$soil.P..mg.kg. <- log(VI$soil.P..mg.kg.)
VI <- VI %>%
  rename("soil phosphorus (mg/kg)" = soil.P..mg.kg.)

## All
# selecting columns
names(VI)
cor_new <- VI[,c(22, 80, 88, 89, 90, 91, 30, 31, 38, 39, 46, 47)]
cor_new <- na.omit(cor_new)
str(cor_new)#check and see if you got it right

# correlations
corr_1to21 <- round(cor(cor_new,method="pearson"),1)
p.mat_1to21 <- cor_pmat(corr_1to21)
corr_1to21

# make figure
Fig_corr <- ggcorrplot(corr_1to21, hc.order = TRUE, type = "lower", 
                     hc.method = "ward.D2", outline.col = "white", 
                     p.mat = p.mat_1to21, method="square", ggtheme=ggplot2::theme_bw(),
                     show.legend=TRUE, legend.title="Pearson's r", lab=TRUE, 
                     lab_size=5, tl.cex=14,colors = c("#003f5c", "white", 
                                                      "#ffa600", pch.cex=20, 
                                                      nbreaks = 8,legend.text.cex=20))
Fig_corr

# save figure
ggsave(filename = "All_MODIS_corr.png",
       plot = Fig_corr, width = 12, height = 12, units = 'cm',
       scale = 2, dpi = 600, path = data_directory)


## NDVI
# selecting columns
names(VI)
cor_new <- VI[,c(22, 80, 88, 89, 90, 91, 38, 39)]
cor_new <- na.omit(cor_new)
str(cor_new)#check and see if you got it right

# correlations
corr_1to21 <- round(cor(cor_new,method="pearson"),1)
p.mat_1to21 <- cor_pmat(corr_1to21)
corr_1to21

# make figure
Fig_corr <- ggcorrplot(corr_1to21, hc.order = TRUE, type = "lower", 
                     hc.method = "ward.D2", outline.col = "white", 
                     p.mat = p.mat_1to21, method="square", ggtheme=ggplot2::theme_bw(),
                     show.legend=TRUE, legend.title="Pearson's r", lab=TRUE, 
                     lab_size=5, tl.cex=14,colors = c("#003f5c", "white", 
                                                      "#ffa600", pch.cex=20, 
                                                      nbreaks = 8,legend.text.cex=20))
Fig_corr

# save figure
ggsave(filename = "NDVI_MODIS_corr.png",
       plot = Fig_corr, width = 12, height = 12, units = 'cm',
       scale = 2, dpi = 600, path = data_directory)

# EVI
# selecting columns
names(VI)
cor_new <- VI[,c(22, 80, 88, 89, 90, 91, 30, 31)]
cor_new <- na.omit(cor_new)
str(cor_new)#check and see if you got it right

# correlations
corr_1to21 <- round(cor(cor_new,method="pearson"),1)
p.mat_1to21 <- cor_pmat(corr_1to21)
corr_1to21

# make figure
Fig_corr <- ggcorrplot(corr_1to21, hc.order = TRUE, type = "lower", 
                       hc.method = "ward.D2", outline.col = "white", 
                       p.mat = p.mat_1to21, method="square", ggtheme=ggplot2::theme_bw(),
                       show.legend=TRUE, legend.title="Pearson's r", lab=TRUE, 
                       lab_size=5, tl.cex=14,colors = c("#003f5c", "white", 
                                                        "#ffa600", pch.cex=20, 
                                                        nbreaks = 8,legend.text.cex=20))
Fig_corr

# save figure
ggsave(filename = "EVI_MODIS_corr.png",
       plot = Fig_corr, width = 12, height = 12, units = 'cm',
       scale = 2, dpi = 600, path = data_directory)

# LAI
# selecting columns
names(VI)
cor_new <- VI[,c(22, 80, 88, 89, 90, 91, 46, 47)]
cor_new <- na.omit(cor_new)
str(cor_new)#check and see if you got it right

# correlations
corr_1to21 <- round(cor(cor_new,method="pearson"),1)
p.mat_1to21 <- cor_pmat(corr_1to21)
corr_1to21

# make figure
Fig_corr <- ggcorrplot(corr_1to21, hc.order = TRUE, type = "lower", 
                       hc.method = "ward.D2", outline.col = "white", 
                       p.mat = p.mat_1to21, method="square", ggtheme=ggplot2::theme_bw(),
                       show.legend=TRUE, legend.title="Pearson's r", lab=TRUE, 
                       lab_size=5, tl.cex=14,colors = c("#003f5c", "white", 
                                                        "#ffa600", pch.cex=20, 
                                                        nbreaks = 8,legend.text.cex=20))
Fig_corr

# save figure
ggsave(filename = "LAI_MODIS_corr.png",
       plot = Fig_corr, width = 12, height = 12, units = 'cm',
       scale = 2, dpi = 600, path = data_directory)


