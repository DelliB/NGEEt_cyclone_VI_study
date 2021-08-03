# Dellena Bloom
# May 28th, 2021
# Plots for comparing old NDVI and making the timeseries

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
recovery <- read.csv(file.path(data_directory, "Recovery_collection.csv"), stringsAsFactors= FALSE)

# axis labels
doNDVI1 = expression(Delta*"old NDVI annual")
doNDVI2 = expression(Delta*"old NDVI sub-annual")
dnNDVI1 = expression(Delta*"new NDVI annual")
dnNDVI2 = expression(Delta*"new NDVI sub-annual")
dEVI1 = expression(Delta*"EVI annual")
dEVI2 = expression(Delta*"EVI sub-annual")
dkNDVI1 = expression(Delta*"kNDVI annual")
dkNDVI2 = expression(Delta*"kNDVI sub-annual")
NDVIreco = expression(Delta*"NDVI Recovery")
EVIreco = expression(Delta*"EVI Recovery")
kNDVIreco = expression(Delta*"kNDVI Recovery")


### Plots
## Comparisons between new and old data
## new NDVI
# old NDVI vs new delta NDVI
NDVI1 <- ggplot(VI, aes(x = change_annual_NDVI.1, y = change_annual_NDVI)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(-.32, -0.1) +
  labs(x = doNDVI1, y = dnNDVI1) +
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

NDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI.1, y = change_sub.annual_NDVI)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = doNDVI2, y = dnNDVI2) +
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

ggarrange(NDVI1, NDVI2, ncol = 2, labels = c("a", "b"))
ggsave("old_new_NDVI.png", width = 6, height = 4, path = data_directory)

## new NDVI
# old NDVI vs new delta EVI
NDVI1 <- ggplot(VI, aes(x = change_annual_NDVI.1, y = change_annual_EVI)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(-.32, -0.1) +
  labs(x = doNDVI1, y = dEVI1) +
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

NDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI.1, y = change_sub.annual_EVI)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = doNDVI2, y = dEVI2) +
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

ggarrange(NDVI1, NDVI2, ncol = 2, labels = c("a", "b"))
ggsave("oldNDVI_newEVI.png", width = 6, height = 4, path = data_directory)

## new kNDVI
# old NDVI vs new delta kNDVI
kNDVI1 <- ggplot(VI, aes(x = change_annual_NDVI.1, y = change_annual_kNDVI)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  xlim(-.32, -0.1) +
  labs(x = doNDVI1, y = dkNDVI1) +
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

kNDVI2 <- ggplot(VI, aes(x = change_sub.annual_NDVI.1, y = change_sub.annual_kNDVI)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = doNDVI2, y = dkNDVI2) +
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

ggarrange(kNDVI1, kNDVI2, ncol = 2, labels = c("a", "b"))
ggsave("oldNDVI_newkNDVI.png", width = 6, height = 4, path = data_directory)


### Timeseries
## Pantropical data calculation
# NDVI
NDVIm <- recovery %>%
  drop_na() %>%
  group_by(month) %>%
  summarize(m = mean(NDVI_change),
            SD = sd(NDVI_change),
            n = length(NDVI_change))
NDVIm <- merge(NDVIm, recovery, by = "month")
error <- qnorm(0.975) * NDVIm$SD/sqrt(NDVIm$n)
# this is standard error
# .975 if having a normal distribution and .95 if not?
NDVIm$min = NDVIm$m - error
NDVIm$max = NDVIm$m + error

# EVI
EVIm <- recovery %>%
  drop_na() %>%
  group_by(month) %>%
  summarize(m = mean(EVI_change),
            SD = sd(EVI_change),
            n = length(EVI_change))
EVIm <- merge(EVIm, recovery, by = "month")
error <- qnorm(0.975) * EVIm$SD/sqrt(EVIm$n)
# this is standard error
# .975 if having a normal distribution and .95 if not?
EVIm$min = EVIm$m - error
EVIm$max = EVIm$m + error

## Plots
# NDVI
ggplot(NDVIm, aes(x = month, y = NDVI_change, group = case_study, color = region)) +
  geom_line(data=NDVIm[!is.na(NDVIm$NDVI_change),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
        fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = "Time (months)", y = NDVIreco, color = "Region") +
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
ggsave("recovery_NDVI.png", width = 7, height = 5, path = data_directory)

# EVI
ggplot(EVIm, aes(x = month, y = EVI_change, group = case_study, color = region)) +
  geom_line(data=EVIm[!is.na(EVIm$EVI_change),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = "Time (months)", y = EVIreco, color = "Region") +
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
ggsave("recovery_EVI.png", width = 7, height = 5, path = data_directory)

# kNDVI
ggplot(recovery, aes(x = month, y = recovery_change, group = case_study, color = region)) +
  geom_line(data=NDVITSm[!is.na(NDVITSm$recovery_change),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = "Time (months)", y = NDVIreco, color = "Region") +
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
ggsave("recovery_NDVI.png", width = 6, height = 5, path = data_directory)

