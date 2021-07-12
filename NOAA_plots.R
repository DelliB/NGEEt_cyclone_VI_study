# Dellena Bloom
# July 8th, 2021
# Scatter plots for NOAA results

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
dNDVI1 = expression(Delta*"NDVI annual (5km)")
dNDVI2 = expression(Delta*"NDVI sub-annual (5km)")

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
TLNDVI1 <- ggplot(VI, aes(x = change_annual_NDVI_5km, y = change_annual_total_litterfall)) +
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
  annotate("text", x = -0.2, y = -.3,
           label = c("Baseline"))

TLNDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI_5km, y = change_subannual_total_litterfall)) +
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

LLNDVI1 <- ggplot(VI, aes(x = change_annual_NDVI_5km, y = change_annual_leaf_litterfall)) +
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

LLNDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI_5km, y = change_subannual_leaf_litterfall)) +
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
ggsave("NOAA_Results_TL_LL_NDVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta NDVI
PNDVI1 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_NDVI_5km)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black") +
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

PNDVI2 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_NDVI_5km)) +
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
ggsave("NOAA_Results_soilP_NDVI.png", width = 6, height = 4, path = data_directory)


