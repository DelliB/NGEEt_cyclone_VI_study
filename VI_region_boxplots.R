# Dellena Bloom
# June 1st, 2021
# Making boxplots of VIs by region for annual talk

## Prepare environment
# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
VI <- read.csv(file.path(data_directory, "case_study_data.csv"), stringsAsFactors = FALSE)

# axis labels
dNDVI1 = expression(Delta*"NDVI annual")
dNDVI2 = expression(Delta*"NDVI sub-annual")
dEVI1 = expression(Delta*"EVI annual")
dEVI2 = expression(Delta*"EVI sub-annual")
dkNDVI1 = expression(Delta*"kNDVI annual")
dkNDVI2 = expression(Delta*"kNDVI sub-annual")

# eliminating Hawaii
VI <- VI[which(VI$Region != "Hawaii"),]
  
# TUKEY
generate_label_df <- function(TUKEY, variable){
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}
model=lm(VI$change_sub.annual_kNDVI~VI$Region)
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'VI$Region', conf.level=0.95)
labels <- generate_label_df(TUKEY , "VI$Region")

# Kruskal-Wallis test
kruskal.test(change_annual_NDVI ~ Region, data = VI)

## Boxplots
# NDVI
NDVIa <- ggplot(VI, aes(x = Region, y = change_annual_NDVI, color = Region)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dNDVI1) +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0.7, y = 0.023,
           label = c("Baseline")) 

NDVIsa <- ggplot(VI, aes(x = Region, y = change_sub.annual_NDVI, color = Region)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dNDVI2) +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank()) 

NDVI <- ggarrange(NDVIa, NDVIsa, nrow = 2, labels = c("a", "b"))
print(NDVI)
ggsave("NDVI_region_boxplot.png", width = 6, height = 8, path = data_directory)

# EVI
EVIa <- ggplot(VI, aes(x = Region, y = change_annual_EVI, color = Region)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dEVI1) +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0.9, y = 0.023,
           label = c("Baseline"))

EVIsa <- ggplot(VI, aes(x = Region, y = change_sub.annual_EVI, color = Region)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dEVI2) +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank())

EVI <- ggarrange(EVIa, EVIsa, nrow = 2, labels = c("a", "b"))
print(EVI)
ggsave("EVI_region_boxplot.png", width = 6, height = 8, path = data_directory)

# kNDVI
kNDVIa <- ggplot(VI, aes(x = Region, y = change_annual_kNDVI, color = Region)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dkNDVI1) +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0.9, y = 0.023,
           label = c("Baseline")) 

kNDVIsa <- ggplot(VI, aes(x = Region, y = change_sub.annual_kNDVI, color = Region)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dkNDVI2) +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank()) 

kNDVI <- ggarrange(kNDVIa, kNDVIsa, nrow = 2, labels = c("a", "b"))
print(kNDVI)
ggsave("kNDVI_region_boxplot.png", width = 6, height = 8, path = data_directory)




