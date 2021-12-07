# Dellena Bloom
# August 9th, 2021
# Recovery timeseries for MODIS LAI, EVI, and NDVI

### Organizing Environment
# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggcorrplot)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
recovery <- read.csv(file.path(data_directory, "MODIS_recovery.csv"), stringsAsFactors= FALSE, na.strings = '-9999')
LFreco <- read.csv(file.path(data_directory, "MODIS_recovery_bb.csv"), stringsAsFactors= FALSE)

# axis labels
LAIreco = expression(Delta*"LAI 500m [LAI/LAI]")
EVIreco = expression(Delta*"EVI 250m [EVI/EVI]")
NDVIreco = expression(Delta*"NDVI 250m [NDVI/NDVI]")
x = "Time since disturbance (months)"
#Pantropical_LAI = c("Pantropical", expression(Delta*"LAI"))

### Timeseries
## Pantropical data calculation
# LAI
LAIm <- recovery %>%
  drop_na() %>%
  group_by(month) %>%
  summarize(m = mean(LAI_change_500m),
            SD = sd(LAI_change_500m),
            n = length(LAI_change_500m))
LAIm <- merge(LAIm, recovery, by = "month")
# took "qnorm(0.95) *" out from the beginning of the equation
error <- LAIm$SD/sqrt(LAIm$n)
# this is standard error
# .975 if having a normal distribution and .95 if not?
LAIm$min = LAIm$m - (1.96*error)
LAIm$max = LAIm$m + (1.96*error)

# LF
# took "qnorm(0.95) *" out from beginning of the equation
LFerror <- LFreco$Pantropical_Litterfall_Std_dev/sqrt(LFreco$Number_Case_Studies)
LFreco$min = LFreco$Pantropical_Litterfall_Mean - (1.96*LFerror)
LFreco$max = LFreco$Pantropical_Litterfall_Mean + (1.96*LFerror)

## Plot
# LAI
ggplot(LAIm, aes(x = month, y = LAI_change_500m, group = case_study, color = region)) +
  geom_line(data = LAIm[!is.na(LAIm$LAI_change_500m),], linetype = "dashed") +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical Δ LAI"), lwd=1) +
  geom_ribbon(data = LFreco, aes(x = month, ymin = CI_lower_bound, ymax = CI_upper_bound), 
              fill = "gray", alpha = 0.25, color = NA) +
  geom_line(data = LFreco, aes(x = month, y = Pantropical_Litterfall_Mean, 
                               color = "Pantropical litterfall"), lwd=1) +
  labs(x = x, y = LAIreco, color = "Region") +
  ylim(-1.5, 1.5) +
  geom_hline(yintercept=c(0), color = "gray") +
  scale_color_manual(values = c("red2", "yellow3", "springgreen4", "black", 
                                "deepskyblue3", "orchid")) +
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
  labs(caption = "Solid lines are pantropical mean and shaded areas represent 95% CI") +
  guides(color = guide_legend(nrow=2,byrow=TRUE))
ggsave("recovery_LAI_LF_500m.png", width = 7, height = 7, path = data_directory)

#guides(color=guide_legend(nrow=2, byrow=TRUE)) +
#  scale_color_manual(values = c( "Pantropical mean" = "light blue", "Pantropical LF mean" = "purple", 
 #                                "Australia" = "red", "Mexico" = "green", "Caribbean" = "tan", "Taiwan" = "pink"))

LAI <- ggplot(LAIm, aes(x = month, y = LAI_change_500m, group = case_study, color = region)) +
  geom_line(data = LAIm[!is.na(LAIm$LAI_change_500m),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = "", y = LAIreco, color = "Region") +
  ylim(-1.5, 1.5) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 13, color = "black"),
        legend.text = element_text(size = 13, color = "black"),
        plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))
print(LAI)


## Pantropical data calculation
# EVI
EVIm <- recovery %>%
  drop_na() %>%
  group_by(month) %>%
  summarize(m = mean(EVI_change_250m),
            SD = sd(EVI_change_250m),
            n = length(EVI_change_250m))
EVIm <- merge(EVIm, recovery, by = "month")
error <- qnorm(0.975) * EVIm$SD/sqrt(EVIm$n)
# this is standard error
# .975 if having a normal distribution and .95 if not?
EVIm$min = EVIm$m - error
EVIm$max = EVIm$m + error

## Plot
# EVI
ggplot(EVIm, aes(x = month, y = EVI_change_250m, group = case_study, color = region)) +
  geom_line(data = EVIm[!is.na(EVIm$EVI_change_250m),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = x, y = EVIreco, color = "Region") +
  ylim(-1, 1) +
  geom_hline(yintercept=c(0), color = "gray") +
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
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("recovery_EVI_250m.png", width = 7, height = 5, path = data_directory)

EVI <- ggplot(EVIm, aes(x = month, y = EVI_change_250m, group = case_study, color = region)) +
  geom_line(data = EVIm[!is.na(EVIm$EVI_change_250m),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = x, y = EVIreco, color = "Region") +
  ylim(-.75, .75) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        #legend.title = element_text(size = 13, color = "black"),
        #legend.text = element_text(size = 13, color = "black"),
        plot.title = element_blank(),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))
print(EVI)


## Pantropical data calculation
# EVI
NDVIm <- recovery %>%
  drop_na() %>%
  group_by(month) %>%
  summarize(m = mean(NDVI_change_250m),
            SD = sd(NDVI_change_250m),
            n = length(NDVI_change_250m))
NDVIm <- merge(NDVIm, recovery, by = "month")
error <- qnorm(0.975) * NDVIm$SD/sqrt(NDVIm$n)
# this is standard error
# .975 if having a normal distribution and .95 if not?
NDVIm$min = NDVIm$m - error
NDVIm$max = NDVIm$m + error

## Plot
# NDVI
ggplot(NDVIm, aes(x = month, y = NDVI_change_250m, group = case_study, color = region)) +
  geom_line(data = NDVIm[!is.na(NDVIm$NDVI_change_250m),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = x, y = NDVIreco, color = "Region") +
  ylim(-1, 1) +
  geom_hline(yintercept=c(0), color = "gray") +
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
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("recovery_NDVI_250m.png", width = 7, height = 5, path = data_directory)

NDVI <- ggplot(NDVIm, aes(x = month, y = NDVI_change_250m, group = case_study, color = region)) +
  geom_line(data = NDVIm[!is.na(NDVIm$NDVI_change_250m),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = x, y = NDVIreco, color = "Region") +
  ylim(-.6, .6) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        #legend.title = element_text(size = 13, color = "black"),
        #legend.text = element_text(size = 13, color = "black"),
        plot.title = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))
print(NDVI)


### Arranging plots together
ggarrange(LAI, EVI, nrow = 2, labels = c("A", "B"))
ggsave("MODIS_recovery.png", width = 7, height = 9, path = data_directory)

