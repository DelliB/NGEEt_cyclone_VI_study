# Dellena Bloom
# March 16th, 2021
# Plots for SULI spring Results section

### Organizing Environment
# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggcorrplot)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
NDVI <- read.csv(file.path(data_directory, "case_study_data.csv"), stringsAsFactors = FALSE)

# data frame organizations
#NDVInodry <- NDVI[which(NDVI$Broad.Life.Zone != "Dry"),]
#NDVIdry <- NDVI[which(NDVI$Broad.Life.Zone == "Dry"),]

# ordering HLZ classifications
#NDVI$Holdridge <- factor(NDVI$Holdridge, levels=c("Subtropical Dry", "Subtropical Premontane Dry", "Tropical Dry", "Subtropical Moist", "Subtropical Lowermontane Moist", "Tropical Moist", "Subtropical Wet", "Subtropical Lowermontane Wet", "Subtropical Lowermontane Rain"))

# axis labels
x = expression(paste(log[10],"(soil P (mg/kg))"))
dTL1 = expression(Delta*"TL annual")
dTL2 = expression(Delta*"TL sub-annual")
dLL1 = expression(Delta*"LL annual")
dLL2 = expression(Delta*"LL sub-annual")
dNDVI1 = expression(Delta*"NDVI annual")
dNDVI2 = expression(Delta*"NDVI sub-annual")

# TUKEY
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
labels <- generate_label_df(TUKEY , "NDVI$Holdridge")

### Plots
# Holdridge life zone histogram; Figure 1
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
           label = c("12"))
ggsave("Holdridge_histogram.png", width = 8, height = 5, path = data_directory)

# Change in litterfall (TL and LL) and delta NDVI (1 and 2) correlations; Figure 3
TLNDVI1 <- ggplot(NDVI, aes(x = X_NDVI.annual, y = X_TL.annual)) +
  geom_point(size = 1) +
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
  annotate("text", x = -0.115, y = -40,
           label = c("Baseline"))

TLNDVI2 <- ggplot(NDVI, aes(x = X_NDVI.sub.annual, y = X_TL.sub.annual)) +
  geom_point(size = 1) +
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

LLNDVI1 <- ggplot(NDVI, aes(x = X_NDVI.annual, y = X_LL.annual)) +
  geom_point(size = 1) +
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

LLNDVI2 <- ggplot(NDVI, aes(x = X_NDVI.sub.annual, y = X_LL.sub.annual)) +
  geom_point(size = 1) +
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
LF_NDVI <- annotate_figure(LF_NDVI, top = text_grob("Litterfall and NDVI spatially grouped", 
                                    color = "black", size = 14))
print(LF_NDVI)

ggsave("Results_TL_LL_NDVI.png", width = 6, height = 6, path = data_directory)

# Soil P vs delta NDVI (1 and 2); Figure 4
PNDVI1 <- ggplot(NDVI, aes(x = log(soil.P), y = NDVI1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
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
  annotate("text", x = 6.5, y = 0.023,
           label = c("Baseline"))

PNDVI2 <- ggplot(NDVI, aes(x = log(soil.P), y = NDVI2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, linetype = "dashed", color = "black") +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~"))) +
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

ggsave("Results_soilP_NDVI.png", width = 6, height = 4, path = data_directory)

# Broad HLZ vs delta NDVI (1 and 2); Figure 7
BHLZNDVI1 <- ggplot(NDVI, aes(x = Broad.Life.Zone, y = NDVI1)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dNDVI1) +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0.7, y = 0.023,
           label = c("Baseline")) +
  annotate("text", x = 1:2, y = .25:.25,
           label = c("A", "A")) +
  annotate("text", x = 3, y = .25,
           label = c("A"))

BHLZNDVI2 <- ggplot(NDVI, aes(x = Broad.Life.Zone, y = NDVI2)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dNDVI2) +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 1:2, y = .25:.25,
           label = c("A", "A")) +
  annotate("text", x = 3, y = .25,
           label = c("A"))

BHLZ <- ggarrange(BHLZNDVI1, BHLZNDVI2, nrow = 2, labels = c("a", "b"))
annotate_figure(BHLZ, bottom = text_grob("Moisture Based Forest Types", size = 13,
                                         color = "black"))

ggsave("Results_BHLZ_NDVI.png", width = 5, height = 8, path = data_directory)

# Narrow HLZ vs delta NDVI (1 and 2); Figure 6
HLZNDVI1 <- ggplot(NDVI, aes(x = Holdridge, y = NDVI1, color = Holdridge)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(y = dNDVI1) +
  geom_hline(yintercept=c(0), color = "gray") +
  geom_vline(xintercept=c(3.5, 6.5), color = "gray") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 1, y = 0.023,
           label = c("Baseline")) +
  annotate("text", x = 1:2, y = .25:.25,
           label = c("AB", "A")) +
  annotate("text", x = 3:4, y = .25:.25,
           label = c("AB","AB")) +
  annotate("text", x = 5:6, y = .25:.25,
           label = c("A", "B")) +
  annotate("text", x = 7:8, y = .25:.25,
           label = c("AB", "AB")) +
  annotate("text", x = 9, y = .25,
           label = c("AB")) +
  annotate("text", x = 2, y = -.7,
           label = c("Dry")) +
  annotate("text", x = 5, y = -.7,
           label = c("Moist")) +
  annotate("text", x = 8, y = -.7,
           label = c("Wet"))

HLZNDVI2 <- ggplot(NDVI, aes(x = Holdridge, y = NDVI2, color = Holdridge)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=1) +
  labs(x = "Holdridge Life Zone", y = dNDVI2) +
  geom_vline(xintercept=c(3.5, 6.5), color = "gray") +
  theme_bw() +
  geom_hline(yintercept=c(0), color = "gray") +
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

ggarrange(HLZNDVI1, HLZNDVI2, nrow = 2, labels = c("a", "b"), common.legend = TRUE, 
          legend = "right")

ggsave("Results_HLZ_NDVI.png", width = 8, height = 7, path = data_directory)

# Correlation plot; Figure 5
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


