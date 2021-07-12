# Dellena Bloom
# July 2nd, 2021
# Plots with only negative change in MODIS VI and new calculation of delta LF

# Code for color change
+scale_color_manual(values=c("#1dabe6","#b35a2d","#c3ced0","#ffa600","#665191","#af060f"))
# regions are in alphabetical order: Australia, Guadaloupe, Hawaii, Mexico, Puerto Rico, Taiwan

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

# Calculating change in LF
VI$change_total_litterfallc = log(VI$Response_Post_Mean_Tot_Litterfall 
                                / VI$Pre_Mean_Tot_Litterfall_g.m2.day)
VI$change_leaf_litterfallc = log(VI$Response_Post_Mean_Leaf_fall 
                                / VI$Pre_Mean_Leaf_fall)
# how to do natural log? -> the natural log is represented by just log
# log10 is log10

# Selecting only zero and negative changes in VI
LAIneg <- VI %>%
  filter(change_annual_LAI_500m <= 0) #%>%
  #filter(change_sub.annual_LAI_500m)

EVIneg <- VI %>%
  filter(change_annual_EVI_250m <= 0) #%>%
  #filter(change_sub.annual_EVI_500m)

NDVIneg <- VI %>%
  filter(change_annual_NDVI_250m <= 0) #%>%
  #filter(change_sub.annual_NDVI_500m)
# should I be filtering for both negative change in annual and sub?

### Plots with new change in LF calculation
## NDVI
# Change in litterfall and delta NDVI
TLNDVI1 <- ggplot(NDVIneg, aes(x = change_annual_NDVI_250m, y = change_total_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
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
  annotate("text", x = -0.2, y = -.30,
           label = c("Baseline"))

TLNDVI2 <- ggplot(NDVIneg, aes(x = change_sub.annual_NDVI_250m, y = change_total_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL1, x = dNDVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLNDVI1 <- ggplot(NDVIneg, aes(x = change_annual_NDVI_250m, y = change_leaf_litterfallc)) +
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

LLNDVI2 <- ggplot(NDVIneg, aes(x = change_sub.annual_NDVI_250m, y = change_leaf_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dNDVI2) +
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
LF_NDVI <- annotate_figure(LF_NDVI, top = text_grob("Filtered for negative change in NDVI", 
                                    color = "black", size = 14))

print(LF_NDVI)
ggsave("Neg_MODIS_Results_TL_LL_NDVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta NDVI
PNDVI1 <- ggplot(NDVIneg, aes(x = log(soil.P..mg.kg.), y = change_annual_NDVI_250m)) +
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

PNDVI2 <- ggplot(NDVIneg, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_NDVI_250m)) +
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

P_NDVI <- ggarrange(PNDVI1, PNDVI2, ncol = 2, labels = c("a", "b"))
P_NDVI <- annotate_figure(P_NDVI, top = text_grob("Filtered for negative change in NDVI", 
                                         color = "black", size = 14))
print(P_NDVI)
ggsave("Neg_MODIS_Results_soilP_NDVI.png", width = 6, height = 4, path = data_directory)


## EVI
# Change in litterfall and delta EVI
TLEVI1 <- ggplot(EVIneg, aes(x = change_annual_EVI_250m, y = change_total_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
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
  annotate("text", x = -0.2, y = -.30,
           label = c("Baseline"))

TLEVI2 <- ggplot(EVIneg, aes(x = change_sub.annual_EVI_250m, y = change_total_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL1, x = dEVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLEVI1 <- ggplot(EVIneg, aes(x = change_annual_EVI_250m, y = change_leaf_litterfallc)) +
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

LLEVI2 <- ggplot(EVIneg, aes(x = change_sub.annual_EVI_250m, y = change_leaf_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dEVI2) +
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
LF_EVI <- annotate_figure(LF_EVI, top = text_grob("Filtered for negative change in EVI", 
                                         color = "black", size = 14))

print(LF_EVI)
ggsave("Neg_MODIS_Results_TL_LL_EVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta EVI
PEVI1 <- ggplot(EVIneg, aes(x = log(soil.P..mg.kg.), y = change_annual_EVI_250m)) +
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

PEVI2 <- ggplot(EVIneg, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_EVI_250m)) +
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

P_EVI <- ggarrange(PEVI1, PEVI2, ncol = 2, labels = c("a", "b"))
P_EVI <- annotate_figure(P_EVI, top = text_grob("Filtered for negative change in EVI", 
                                         color = "black", size = 14))
print(P_EVI)
ggsave("Neg_MODIS_Results_soilP_EVI.png", width = 6, height = 4, path = data_directory)


## LAI
# Change in litterfall and delta LAI
TLLAI1 <- ggplot(LAIneg, aes(x = change_annual_LAI_500m, y = change_total_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
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
  annotate("text", x = -0.2, y = -.40,
           label = c("Baseline"))

TLLAI2 <- ggplot(LAIneg, aes(x = change_sub.annual_LAI_500m, y = change_total_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL1, x = dLAI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLLAI1 <- ggplot(LAIneg, aes(x = change_annual_LAI_500m, y = change_leaf_litterfallc)) +
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

LLLAI2 <- ggplot(LAIneg, aes(x = change_sub.annual_LAI_500m, y = change_leaf_litterfallc)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dLAI2) +
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
LF_LAI <- annotate_figure(LF_LAI, top = text_grob("Filtered for negative change in LAI", 
                                       color = "black", size = 14))

print(LF_LAI)
ggsave("Neg_MODIS_Results_TL_LL_LAI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta LAI
PLAI1 <- ggplot(LAIneg, aes(x = log(soil.P..mg.kg.), y = change_annual_LAI_500m)) +
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

PLAI2 <- ggplot(LAIneg, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_LAI_500m)) +
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

P_LAI <- ggarrange(PLAI1, PLAI2, ncol = 2, labels = c("a", "b"))
P_LAI <- annotate_figure(P_LAI, top = text_grob("Filtered for negative change in LAI", 
                                       color = "black", size = 14))
print(P_LAI)
ggsave("Neg_MODIS_Results_soilP_LAI.png", width = 6, height = 4, path = data_directory)





### Plots with old change in LF calculation
## NDVI
# Change in litterfall and delta NDVI
TLNDVI1 <- ggplot(NDVIneg, aes(x = change_annual_NDVI_250m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
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
  annotate("text", x = -0.2, y = -.30,
           label = c("Baseline"))

TLNDVI2 <- ggplot(NDVIneg, aes(x = change_sub.annual_NDVI_250m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL1, x = dNDVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLNDVI1 <- ggplot(NDVIneg, aes(x = change_annual_NDVI_250m, y = change_leaf_litterfall)) +
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

LLNDVI2 <- ggplot(NDVIneg, aes(x = change_sub.annual_NDVI_250m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dNDVI2) +
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
LF_NDVI <- annotate_figure(LF_NDVI, top = text_grob("Filtered for negative change in NDVI", 
                                                    color = "black", size = 14))

print(LF_NDVI)
ggsave("oc_Neg_MODIS_Results_TL_LL_NDVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta NDVI
PNDVI1 <- ggplot(NDVIneg, aes(x = log(soil.P..mg.kg.), y = change_annual_NDVI_250m)) +
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

PNDVI2 <- ggplot(NDVIneg, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_NDVI_250m)) +
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

P_NDVI <- ggarrange(PNDVI1, PNDVI2, ncol = 2, labels = c("a", "b"))
P_NDVI <- annotate_figure(P_NDVI, top = text_grob("Filtered for negative change in NDVI", 
                                                  color = "black", size = 14))
print(P_NDVI)
ggsave("oc_Neg_MODIS_Results_soilP_NDVI.png", width = 6, height = 4, path = data_directory)


## EVI
# Change in litterfall and delta EVI
TLEVI1 <- ggplot(EVIneg, aes(x = change_annual_EVI_250m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
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
  annotate("text", x = -0.2, y = -.30,
           label = c("Baseline"))

TLEVI2 <- ggplot(EVIneg, aes(x = change_sub.annual_EVI_250m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL1, x = dEVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLEVI1 <- ggplot(EVIneg, aes(x = change_annual_EVI_250m, y = change_leaf_litterfall)) +
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

LLEVI2 <- ggplot(EVIneg, aes(x = change_sub.annual_EVI_250m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dEVI2) +
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
LF_EVI <- annotate_figure(LF_EVI, top = text_grob("Filtered for negative change in EVI", 
                                                  color = "black", size = 14))

print(LF_EVI)
ggsave("oc_Neg_MODIS_Results_TL_LL_EVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta EVI
PEVI1 <- ggplot(EVIneg, aes(x = log(soil.P..mg.kg.), y = change_annual_EVI_250m)) +
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

PEVI2 <- ggplot(EVIneg, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_EVI_250m)) +
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

P_EVI <- ggarrange(PEVI1, PEVI2, ncol = 2, labels = c("a", "b"))
P_EVI <- annotate_figure(P_EVI, top = text_grob("Filtered for negative change in EVI", 
                                                color = "black", size = 14))
print(P_EVI)
ggsave("oc_Neg_MODIS_Results_soilP_EVI.png", width = 6, height = 4, path = data_directory)


## LAI
# Change in litterfall and delta LAI
TLLAI1 <- ggplot(LAIneg, aes(x = change_annual_LAI_500m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
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
  annotate("text", x = -0.2, y = -.40,
           label = c("Baseline"))

TLLAI2 <- ggplot(LAIneg, aes(x = change_sub.annual_LAI_500m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dTL1, x = dLAI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLLAI1 <- ggplot(LAIneg, aes(x = change_annual_LAI_500m, y = change_leaf_litterfall)) +
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

LLLAI2 <- ggplot(LAIneg, aes(x = change_sub.annual_LAI_500m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = dLL1, x = dLAI2) +
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
LF_LAI <- annotate_figure(LF_LAI, top = text_grob("Filtered for negative change in LAI", 
                                                  color = "black", size = 14))

print(LF_LAI)
ggsave("oc_Neg_MODIS_Results_TL_LL_LAI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta LAI
PLAI1 <- ggplot(LAIneg, aes(x = log(soil.P..mg.kg.), y = change_annual_LAI_500m)) +
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

PLAI2 <- ggplot(LAIneg, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_LAI_500m)) +
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

P_LAI <- ggarrange(PLAI1, PLAI2, ncol = 2, labels = c("a", "b"))
P_LAI <- annotate_figure(P_LAI, top = text_grob("Filtered for negative change in LAI", 
                                                color = "black", size = 14))
print(P_LAI)
ggsave("oc_Neg_MODIS_Results_soilP_LAI.png", width = 6, height = 4, path = data_directory)
