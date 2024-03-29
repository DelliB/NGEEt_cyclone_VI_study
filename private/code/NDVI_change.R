# Dellena Bloom
# February 10th, 2021
# Plot the dependence of change in NDVI to Holdridge life zone of the area

# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
NDVI <- read.csv(file.path(data_directory, "new_Disturbance_details_NDVI.csv"), stringsAsFactors = FALSE)
#NDVI <- read.csv(file = "~/Documents/Kueppers lab/Disturbance_details_NDVI.csv")

# data transformations
NDVI$TL_change_1 <- scale(NDVI$TL_change_1)
NDVI$LL_change_1 <- scale(NDVI$LL_change_1)
NDVI$TL_change_2 <- scale(NDVI$TL_change_2)
NDVI$LL_change_2 <- scale(NDVI$LL_change_2)

# Tukey
generate_label_df <- function(TUKEY, variable){
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}
model=lm(NDVI$NDVI2~NDVI$P_class)
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'NDVI$P_class', conf.level=0.95)
labels<-generate_label_df(TUKEY , "NDVI$P_class")

## NEW
# NDVI1 vs life zone (not sig diff using TUKEY)
ggplot(NDVI, aes(x = Holdridge, y = NDVI1, color = Holdridge)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Holdridge Life Zone", y = "Change in NDVI1") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("NDVI1_Holdridge_box.png", width = 8, height = 5, path = data_directory)

# NDVI2 vs life zone
ggplot(NDVI, aes(x = Holdridge, y = NDVI2, color = Holdridge)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Holdridge Life Zone", y = "Change in NDVI2") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
    annotate("text", x = 1:2, y = .25:.25,
             label = c("A", "AB")) +
    annotate("text", x = 3:4, y = .25:.25,
             label = c("AB","AB")) +
    annotate("text", x = 5:6, y = .25:.25,
             label = c("A", "AB")) +
    annotate("text", x = 7:8, y = .25:.25,
             label = c("AB", "B")) +
    annotate("text", x = 9, y = .25,
           label = c("AB"))
ggsave("NDVI2_Holdridge_box.png", width = 8, height = 5, path = data_directory)

# plot of NDVI1 in wet vs dry tropical forest
ggplot(NDVI, aes(x = Broad.Life.Zone, y = NDVI1, color = Broad.Life.Zone)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Wet versus Dry forests", y = "Change in NDVI1") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("NDVI1_WvD_box.png", width = 6, height = 5, path = data_directory)

# plot of NDVI2 in wet vs dry tropical forest
ggplot(NDVI, aes(x = Broad.Life.Zone, y = NDVI2, color = Broad.Life.Zone)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Wet versus Dry forests", y = "Change in NDVI2") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("NDVI2_WvD_box.png", width = 6, height = 5, path = data_directory)

# soil P and NDVI1
ggplot(NDVI, aes(x = soil.P, y = NDVI1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = "Soil Phosphorous (mg/kg)", y = "Change in NDVI1") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("new_NDVI1_P_box.png", width = 6, height = 5, path = data_directory)

# soil P and NDVI2
ggplot(NDVI, aes(x = soil.P, y = NDVI2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = "Soil Phosphorous (mg/kg)", y = "Change in NDVI2") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("new_NDVI2_P_box.png", width = 6, height = 5, path = data_directory)

# soil P and TL1
ggplot(NDVI, aes(x = soil.P, y = TL_change_1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = "Soil Phosphorous (mg/kg)", y = "Change in non-seasonal Total Litterfall") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("new_TL1_P_box.png", width = 6, height = 5, path = data_directory)

# soil P and LL1
ggplot(NDVI, aes(x = soil.P, y = LL_change_1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = "Soil Phosphorous (mg/kg)", y = "Change in non-seasonal Leaf Litterfall") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("new_LL1_P_box.png", width = 6, height = 5, path = data_directory)

# soil P and TL2
ggplot(NDVI, aes(x = soil.P, y = TL_change_2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = "Soil Phosphorous (mg/kg)", y = "Change in seasonal Total Litterfall") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("new_TL2_P_box.png", width = 6, height = 5, path = data_directory)

# soil P and LL2
ggplot(NDVI, aes(x = soil.P, y = LL_change_2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = "Soil Phosphorous (mg/kg)", y = "Change in seasonal Leaf Litterfall") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("new_LL2_P_box.png", width = 6, height = 5, path = data_directory)


