# Dellena Bloom
# July 28th, 2021
# Wind ranks (Saffir) and change in LAI plots

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
x = expression(paste(ln,"(soil P (mg/kg))"))
dTL1 = expression(Delta*"TL annual")
dLL1 = expression(Delta*"LL annual")
dLAI1 = expression(Delta*"LAI annual (500m)")

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

## Assigning wind ranks
#S <- rep(0, nrow(VI))
#VI$peak_wind_speed_ms[VI$peak_wind_speed_ms < 43] = "Low"
#VI$peak_wind_speed_ms[(VI$peak_wind_speed_ms >= 43) & (VI$peak_wind_speed_ms < 58)] = "Median"
#VI$peak_wind_speed_ms[VI$peak_wind_speed_ms >= 58] = "High"
#VI$S <- VI[which(VI$peak_wind_speed_ms < 43)] <- "Low"
#VI$S <- VI[which((VI$peak_wind_speed_ms >= 43) & (VI$peak_wind_speed_ms < 58))] <- "Median"
#VI$S <- VI[which(VI$peak_wind_speed_ms >= 58)] <- "High"
#print(S)
#S <- as.data.frame(S)

## Make dfs for wind ranks
Low <- VI[VI$Saffir == "Low",] # 31
Medium <- VI[VI$Saffir == "Medium",] # 7
High <- VI[VI$Saffir == "High",] # 2


### Plots
## Litterfall
# Change in litterfall and delta NDVI
TL <- ggplot(Medium, aes(x = change_annual_LAI_500m, y = change_annual_total_litterfall)) +
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
  annotate("text", x = -0.2, y = -.3,
           label = c("Baseline"))

LL <- ggplot(Medium, aes(x = change_annual_LAI_500m, y = change_annual_leaf_litterfall)) +
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

LF_NDVI <- ggarrange(TL, LL, nrow = 2, labels = c("", ""))

print(LF_LAI)
ggsave("MODIS_Results_TL_LL_NDVI.png", width = 6, height = 6, path = data_directory)


## soil phosphorus
ggplot(Low, aes(x = log(soil.P..mg.kg.), y = change_annual_LAI_500m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
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
  annotate("text", x = 7, y = 0.025,
           label = c("Baseline"))

ggplot(Medium, aes(x = log(soil.P..mg.kg.), y = change_annual_LAI_500m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", linetype = "dashed") +
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
  annotate("text", x = 7, y = 0.025,
           label = c("Baseline"))
