# Dellena Bloom
# March 8th, 2021
# Completing new analyses

# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(MASS)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
NDVI <- read.csv(file.path(data_directory, "new_Disturbance_details_NDVI.csv"), stringsAsFactors = FALSE)

# data transformations
NDVInodry <- NDVI[which(NDVI$Broad.Life.Zone != "Dry"),]
NDVIdry <- NDVI[which(NDVI$Broad.Life.Zone == "Dry"),]

# standardize change in litterfall
NDVI$TL_change_1 <- scale(NDVI$TL_change_1)
NDVI$LL_change_1 <- scale(NDVI$LL_change_1)
NDVI$TL_change_2 <- scale(NDVI$TL_change_2)
NDVI$LL_change_2 <- scale(NDVI$LL_change_2)
# ordering HLZ classifications
NDVI$Holdridge <- factor(NDVI$Holdridge, levels=c("Subtropical Dry", "Subtropical Premontane Dry", "Tropical Dry", "Subtropical Moist", "Subtropical Lowermontane Moist", "Tropical Moist", "Subtropical Wet", "Subtropical Lowermontane Wet", "Subtropical Lowermontane Rain"))
x = expression(paste(log[10],"(soil phosphorus (mg/kg))"))
dNDVI1 = expression(Delta*"NDVI annual")
dNDVI2 = expression(Delta*"NDVI sub-annual")

# Tukey
generate_label_df <- function(TUKEY, variable){
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}
model=lm(NDVI$NDVI2~NDVI$Holdridge)
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, 'NDVI$Holdridge', conf.level=0.95)
labels<-generate_label_df(TUKEY , "NDVI$Holdridge")

# Kruskal-Wallis test
kruskal.test(NDVI2 ~ Holdridge, data = NDVI)

# Pairwise comparison
pairwise.wilcox.test(NDVI$NDVI2, NDVI$Holdridge,
                     p.adjust.method = "BH")

# checking for residuals
Model <- lm(NDVI2_pre~soil.P, NDVInodry)
shapiro.test(residuals(Model))

# checking the dependence of soil P on broad forest type
model <- lm(NDVI1~Broad.Life.Zone * log(soil.P), data = NDVI)
summary(model)

# plots
# HLZ histogram
ggplot(NDVI, aes(x = factor(Holdridge), color = Holdridge))+
  geom_bar(stat="count", width=0.7, aes(fill = Holdridge)) +
  labs(y = "Number of Observations", x = "Holdridge Life Zone") +
  geom_vline(xintercept=c(3.5, 6.5), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        plot.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 2, y = 6,
           label = c("7")) +
  annotate("text", x = 5, y = 6,
           label = c("9")) +
  annotate("text", x = 8, y = 6,
           label = c("13"))
ggsave("Holdridge_histogram.png", width = 8, height = 5, path = data_directory)

# NDVI1 vs HLZ
ggplot(NDVI, aes(x = Holdridge, y = NDVI1, color = Holdridge)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Holdridge Life Zone", y = "Change in non-seasonal NDVI") +
  geom_vline(xintercept=c(3.5, 6.5), color = "gray") +
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
        panel.grid.minor = element_blank()) +
  annotate("text", x = 1:2, y = .25:.25,
           label = c("AB", "A")) +
  annotate("text", x = 3:4, y = .25:.25,
           label = c("AB","AB")) +
  annotate("text", x = 5:6, y = .25:.25,
           label = c("A", "B")) +
  annotate("text", x = 7:8, y = .25:.25,
           label = c("AB", "AB")) +
  annotate("text", x = 9, y = .25,
           label = c("AB"))
ggsave("new_NDVI1_Holdridge_box.png", width = 8, height = 5, path = data_directory)

# NDVI2 vs HLZ
ggplot(NDVI, aes(x = Holdridge, y = NDVI2, color = Holdridge)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Holdridge Life Zone", y = "Change in seasonal NDVI") +
  geom_vline(xintercept=c(3.5, 6.5), color = "gray") +
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
        panel.grid.minor = element_blank()) +
  annotate("text", x = 1:2, y = .25:.25,
           label = c("A", "A")) +
  annotate("text", x = 3:4, y = .25:.25,
           label = c("AB","AB")) +
  annotate("text", x = 5:6, y = .25:.25,
           label = c("A", "B")) +
  annotate("text", x = 7:8, y = .25:.25,
           label = c("AB", "AB")) +
  annotate("text", x = 9, y = .25,
           label = c("AB"))
ggsave("new_NDVI2_Holdridge_box.png", width = 8, height = 5, path = data_directory)

# Broad HLZ classifications
ggplot(NDVI, aes(x = factor(Broad.Life.Zone)))+
  geom_bar(stat="count", width=0.7, fill = "green") +
  labs(x = "Broad Holdridge Life Zone Determinations", y = "Number of Observations") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("broad_Holdridge_n_box.png", width = 6, height = 5, path = data_directory)

# Broad HLZ vs NDVI1
ggplot(NDVI, aes(x = Broad.Life.Zone, y = NDVI1)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Broad Holdridge Life Zone Classifications", y = "Change in non-seasonal NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("NDVI1_broad_Holdridge_box.png", width = 6, height = 5, path = data_directory)

# Broad HLZ vs NDVI2
ggplot(NDVI, aes(x = Broad.Life.Zone, y = NDVI2)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Broad Holdridge Life Zone Classifications", y = "Change in seasonal NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("NDVI2_broad_Holdridge_box.png", width = 6, height = 5, path = data_directory)

# NDVI1 and log transformed soil P
ggplot(NDVI, aes(x = log(soil.P), y = NDVI1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = x, y = "Change in non-seasonal NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("NDVI1_soilP_log.png", width = 5, height = 5, path = data_directory)

# NDVI2 and soil P
ggplot(NDVI, aes(x = soil.P, y = NDVI2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = "Soil Phosphorus (mg/kg)", y = "Change in seasonal NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("NDVI2_soilP.png", width = 5, height = 5, path = data_directory)

# NDVI1 and log transformed soil P, with color by HLZ
ggplot(NDVI, aes(x = log(soil.P), y = NDVI1, color = Holdridge)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"), group = 1)) +
  labs(x = x, y = dNDVI1) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 9, color = "black"),
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(title = "", 
        override.aes = aes(label = ""))) +
  annotate("text", x = 7, y = 0.023,
           label = c("Baseline"))
ggsave("NDVI1_soilP_log_HLZ2.png", width = 8, height = 5, path = data_directory)

# NDVI2 and log transformed soil P, with color by HLZ
ggplot(NDVI, aes(x = log(soil.P), y = NDVI2, color = Holdridge)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"), group = 1)) +
  labs(x = x, y = dNDVI2) +
  geom_hline(yintercept=c(0), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 9, color = "black"),
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(title = "", 
        override.aes = aes(label = ""))) +
  annotate("text", x = 7, y = 0.023,
           label = c("Baseline"))
ggsave("NDVI2_soilP_log_HLZ2.png", width = 8, height = 5, path = data_directory)

# pre-NDVI1 and soil P with log transformation
ggplot(NDVI, aes(x = log(soil.P), y = NDVI1_pre)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = x, y = "Non-seasonal pre-cyclone NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("pre_NDVI1_soilP_log.png", width = 5, height = 5, path = data_directory)

# pre-NDVI2 and soil P with log transformation
ggplot(NDVI, aes(x = log(soil.P), y = NDVI2_pre)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(x = x, y = "Seasonal pre-cyclone NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("pre_NDVI2_soilP_log.png", width = 5, height = 5, path = data_directory)

# only dry forest pre-NDVI1 and soil P
ggplot(NDVIdry, aes(x = soil.P, y = NDVI1_pre)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "only dry forests included", x = "Soil Phosphorus (mg/kg)", y = "Non-seasonal pre-cyclone NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("dry_pre_NDVI1_soilP.png", width = 5, height = 5, path = data_directory)

# only dry forest pre-NDVI2 and soil P
ggplot(NDVIdry, aes(x = soil.P, y = NDVI2_pre)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "only dry forests included", x = "Soil Phosphorus (mg/kg)", y = "Seasonal pre-cyclone NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("dry_pre_NDVI2_soilP.png", width = 5, height = 5, path = data_directory)

# without dry forest pre-NDVI1 and soil P
ggplot(NDVInodry, aes(x = soil.P, y = NDVI1_pre)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "dry forests excluded", x = "Soil Phosphorus (mg/kg)", y = "Non-seasonal pre-cyclone NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("nodry_pre_NDVI1_soilP.png", width = 5, height = 5, path = data_directory)

# without dry forest pre-NDVI2 and soil P with log transformation
ggplot(NDVInodry, aes(x = log(soil.P), y = NDVI2_pre)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(title = "dry forests excluded", x = x, y = "Seasonal pre-cyclone NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("nodry_pre_NDVI2_soilP_log.png", width = 5, height = 5, path = data_directory)
