# Dellena Bloom
# March 11th, 2021
# Grouped site correlaitons with litterfall

# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
NDVI <- read.csv(file.path(data_directory, "new_Disturbance_details_NDVI.csv"), stringsAsFactors = FALSE)

# standardize change in litterfall
NDVI$TL_change_1 <- scale(NDVI$TL_change_1)
NDVI$LL_change_1 <- scale(NDVI$LL_change_1)
NDVI$TL_change_2 <- scale(NDVI$TL_change_2)
NDVI$LL_change_2 <- scale(NDVI$LL_change_2)

# Grouped sites NDVI1 and Total Litterfall
ggplot(NDVI, aes(y = G_TL_1, x = G_NDVI1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "annual Total Litterfall (g/m2/day)", x = "Change in annual NDVI",
       title = "Spatially Grouped Litterfall and NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("grouped_NDVI1_GTL.png", width = 5, height = 5, path = data_directory)

# Grouped sites NDVI1 and Leaf Litterfall
ggplot(NDVI, aes(y = G_LL_1, x = G_NDVI1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "annual Leaf Litterfall (g/m2/day)", x = "Change in annual NDVI",
       title = "Spatially Grouped Litterfall and NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("grouped_NDVI1_GLL.png", width = 5, height = 5, path = data_directory)

# Grouped sites NDVI2 and Total Litterfall
ggplot(NDVI, aes(y = G_TL_2, x = G_NDVI2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "sub-annual Total Litterfall (g/m2/day)", x = "Change in sub-annual NDVI",
       title = "Spatially Grouped Litterfall and NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("grouped_NDVI2_GTL.png", width = 5, height = 5, path = data_directory)

# Grouped sites NDVI2 and Leaf Litterfall
ggplot(NDVI, aes(y = G_LL_2, x = G_NDVI2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "sub-annual Leaf Litterfall (g/m2/day)", x = "Change in sub-annual NDVI",
       title = "Spatially Grouped Litterfall and NDVI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("grouped_NDVI2_GLL.png", width = 5, height = 5, path = data_directory)

# Grouped sites NDVI1 and NDVI2
ggplot(NDVI, aes(y = NDVI2, x = G_NDVI1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Change in seasonal NDVI", x = "Change in grouped non-seasonal NDVI",
       title = "Grouped sites") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("groupedNDVI1_NDVI2.png", width = 5, height = 5, path = data_directory)

# Grouped sites NDVI2 and NDVI1
ggplot(NDVI, aes(y = NDVI1, x = G_NDVI2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Change in non-seasonal NDVI", x = "Change in grouped seasonal NDVI",
       title = "Grouped Sites") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("NDVI1_groupedNDVI2.png", width = 5, height = 5, path = data_directory)

# Grouped sites NDVI2 and grouped sites NDVI1
ggplot(NDVI, aes(y = G_NDVI1, x = G_NDVI2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Change in grouped non-seasonal NDVI", x = "Change in grouped seasonal NDVI",
       title = "Grouped Sites") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("groupedNDVI1_groupedNDVI2.png", width = 5, height = 5, path = data_directory)

