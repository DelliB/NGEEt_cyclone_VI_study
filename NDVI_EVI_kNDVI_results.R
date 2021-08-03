# Dellena Bloom
# May 28th, 2021
# Plots for Results section with all 3 Landsat calculated VIs

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
dNDVI1 = expression(Delta*"NDVI annual")
dNDVI2 = expression(Delta*"NDVI sub-annual")
dEVI1 = expression(Delta*"EVI annual")
dEVI2 = expression(Delta*"EVI sub-annual")
dkNDVI1 = expression(Delta*"kNDVI annual")
dkNDVI2 = expression(Delta*"kNDVI sub-annual")

### Plots
## NDVI
# Change in litterfall and delta NDVI
TLNDVI1 <- ggplot(VI, aes(x = change_annual_NDVI_90m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
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
  annotate("text", x = -0.1, y = -2,
           label = c("Baseline"))

TLNDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI_90m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
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

LLNDVI1 <- ggplot(VI, aes(x = change_annual_NDVI_90m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
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

LLNDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI_90m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
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

print(LF_NDVI)
ggsave("Landsat_TL_LL_NDVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta NDVI
PNDVI1 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_NDVI_90m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4, 7) +
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
  annotate("text", x = 5, y = 0.023,
           label = c("Baseline"))

PNDVI2 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_NDVI_90m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4, 7) +
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
ggsave("Landsat_soilP_NDVI.png", width = 6, height = 4, path = data_directory)


## EVI
# Change in litterfall and delta EVI
TLEVI1 <- ggplot(VI, aes(x = change_annual_EVI_90m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
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
  annotate("text", x = -0.1, y = -2,
           label = c("Baseline"))

TLEVI2 <- ggplot(VI, aes(x = change_sub.annual_EVI_90m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
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

LLEVI1 <- ggplot(VI, aes(x = change_annual_EVI_90m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
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

LLEVI2 <- ggplot(VI, aes(x = change_sub.annual_EVI_90m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
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

print(LF_EVI)
ggsave("Landsat_TL_LL_EVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta EVI
PEVI1 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_EVI_90m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4, 7) +
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
  annotate("text", x = 6.5, y = 0.023,
           label = c("Baseline"))

PEVI2 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_EVI_90m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4, 7) +
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
ggsave("Landsat_soilP_EVI.png", width = 6, height = 4, path = data_directory)


## kNDVI
# Change in litterfall and delta kNDVI
TLkNDVI1 <- ggplot(VI, aes(x = change_annual_kNDVI_90m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  labs(y = dTL1, x = dkNDVI1) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -0.1, y = -2,
           label = c("Baseline"))

TLkNDVI2 <- ggplot(VI, aes(x = change_sub.annual_kNDVI_90m, y = change_total_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
  labs(y = dTL1, x = dkNDVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLkNDVI1 <- ggplot(VI, aes(x = change_annual_kNDVI_90m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
  labs(y = dLL1, x = dkNDVI1) +
  geom_vline(xintercept=c(0), color = "gray") +  
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

LLkNDVI2 <- ggplot(VI, aes(x = change_sub.annual_kNDVI_90m, y = change_leaf_litterfall)) +
  geom_point(size = 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  ylim(-5, 20) +
  labs(y = dLL1, x = dkNDVI2) +
  geom_vline(xintercept=c(0), color = "gray") +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())

TL <- ggarrange(TLkNDVI1, TLkNDVI2, ncol = 2, labels = c("a", "b"))
LL <- ggarrange(LLkNDVI1, LLkNDVI2, ncol = 2, labels = c("c", "d"))
LF_kNDVI <- ggarrange(TL, LL, nrow = 2, labels = c("", ""))

print(LF_kNDVI)
ggsave("Landsat_TL_LL_kNDVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta kNDVI
PkNDVI1 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_kNDVI_90m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4, 7) +
  labs(x = x, y = dkNDVI1) +
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
  annotate("text", x = 6.5, y = 0.023,
           label = c("Baseline"))

PkNDVI2 <- ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_sub.annual_kNDVI_90m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(4, 7) +
  labs(x = x, y = dkNDVI2) +
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

ggarrange(PkNDVI1, PkNDVI2, ncol = 2, labels = c("a", "b"))
ggsave("Landsat_soilP_kNDVI.png", width = 6, height = 4, path = data_directory)


## Correlation plot
# renaming columns
NDVI <- NDVI %>%
  rename("storm frequency" = Storm_frequency)
NDVI <- NDVI %>%
  rename("annual change in NDVI" = NDVI1)
NDVI <- NDVI %>%
  rename("sub-annual change in NDVI" = NDVI2)
NDVI <- NDVI %>%
  rename("soil phosphorus" = soil.P)
NDVI <- NDVI %>%
  rename("distance to cyclone" = Distance_km)
NDVI <- NDVI %>%
  rename("wind speed" = wind_kts)
#check column number for the variables you want to include in the correlation plot
names(NDVI)
# add a line here
cor_new <- NDVI[,c(12,14,15,17,39,48)]
cor_new <- na.omit(cor_new)
str(cor_new)
corr_1to21 <- round(cor(cor_new,method="pearson"),1)
p.mat_1to21 <- cor_pmat(corr_1to21)
corr_1to21
# make figure
Fig_corr<-ggcorrplot(corr_1to21, hc.order = TRUE, type = "lower",hc.method = "ward.D2",
                     outline.col = "white", p.mat = p.mat_1to21, method="square",
                     ggtheme=ggplot2::theme_bw(),
                     show.legend=TRUE, legend.title="Pearson's r", lab=TRUE, 
                     lab_size=5, tl.cex=14,colors = c("#003f5c", "white", 
                                                      "#ffa600",pch.cex=20,nbreaks = 8,legend.text.cex=20))
Fig_corr
ggsave(filename = "Fig_corr.png",
       plot = Fig_corr, width = 12, height = 12, units = 'cm',
       scale = 2, dpi = 600, path = data_directory)
