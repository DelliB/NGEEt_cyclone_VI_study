# Dellena Bloom
# February 22nd, 2021
# Comparing field and RS data for Barbara

# load packages
library(tidyverse)
library(ggpubr)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
NDVI <- read.csv(file.path(data_directory, "new_Disturbance_details_NDVI.csv"), stringsAsFactors = FALSE)

# data transformations
NDVI$TL_change_1 <- scale(NDVI$TL_change_1)
NDVI$LL_change_1 <- scale(NDVI$LL_change_1)
NDVI$TL_change_2 <- scale(NDVI$TL_change_2)
NDVI$LL_change_2 <- scale(NDVI$LL_change_2)

# Histogram for soil P distribution
ggplot(NDVI, aes(x = soil.P)) + 
  geom_histogram(color="black", fill="white") +
  labs(x = "Soil Phosphorus", y = "Number of Observations") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("soilP_n_histogram.png", width = 7, height = 5, path = data_directory)

# Boxplot for Holdridge zone n
ggplot(NDVI, aes(x=factor(Holdridge), color = Holdridge))+
  geom_bar(stat="count", width=0.7, fill = "white") +
  labs(x = "Holdridge Life Zone", y = "Number of Observations") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("Holdridge_n_box.png", width = 8, height = 5, path = data_directory)

# Boxplot for Holdridge zone n
NDVIP <- NDVI[which(NDVI$P_class != "NA"),]
ggplot(NDVIP, aes(x=factor(P_class)))+
  geom_bar(stat="count", width=0.7, fill = "green") +
  labs(x = "Soil Phosphorous Class", y = "Number of Observations") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_blank(),
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank())
ggsave("soilP_n_box.png", width = 6, height = 5, path = data_directory)

# NDVI1 vs NDVI2
ggplot(NDVI, aes(y = NDVI1, x = NDVI2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "NDVI1", x = "NDVI2") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) 
ggsave("NDVI1_NDVI2.png", width = 5, height = 5, path = data_directory)

# TL1 pre and NDVI1 pre
ggplot(NDVI, aes(x = (NDVI1_pre/1000), y = TL_pre_1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4, label.x = 3,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Total Litterfall non-seasonal pre", x = "NDVI1 pre-avg") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())
ggsave("pre_NDVI1_TL1.png", width = 5, height = 5, path = data_directory)

# TL2 pre and NDVI2 pre
ggplot(NDVI, aes(x = (NDVI2_pre/1000), y = TL_pre_2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4, label.x = 3,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Total Litterfall seasonal pre", x = "NDVI2 pre-avg") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) 
ggsave("pre_NDVI2_TL2.png", width = 5, height = 5, path = data_directory)

# TL post and NDVI post
ggplot(NDVI, aes(x = (NDVI1_post/1000), y = TL_post_1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4, label.x = 1,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Total Litterfall post", x = "NDVI1 and NDVI2 post-avg") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())
ggsave("post_NDVI_TL12.png", width = 5, height = 5, path = data_directory)

# TL1 change and NDVI1
ggplot(NDVI, aes(x = NDVI1, y = TL_change_1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Change in non-seasonal Total Litterfall", x = "Change in NDVI1") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())
ggsave("change_NDVI1_TL1.png", width = 5, height = 5, path = data_directory)

# TL2 change and NDVI2
ggplot(NDVI, aes(x = NDVI2, y = TL_change_2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Change in seasonal Total Litterfall", x = "Change in NDVI2") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())
ggsave("change_NDVI2_TL2.png", width = 5, height = 5, path = data_directory)

# LL pre and NDVI1 pre
ggplot(NDVI, aes(x = (NDVI1_pre/1000), y = LL_pre_1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4, label.x = 3,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Leaf Litterfall non-seasonal pre", x = "NDVI1 pre-avg") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())
ggsave("pre_NDVI1_LL1.png", width = 5, height = 5, path = data_directory)

# LL pre and NDVI2 pre
ggplot(NDVI, aes(x = (NDVI2_pre/1000), y = LL_pre_2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4, label.x = 3,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Leaf Litterfall seasonal pre", x = "NDVI2 pre-avg") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank()) 
ggsave("pre_NDVI2_LL2.png", width = 5, height = 5, path = data_directory)

# LL post and NDVI post
ggplot(NDVI, aes(x = (NDVI1_post/1000), y = LL_post_1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4, label.x = 1,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Leaf Litterfall post", x = "NDVI1 and NDVI2 post-avg") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())
ggsave("post_NDVI_LL1.png", width = 5, height = 5, path = data_directory)

# LL change and NDVI1
ggplot(NDVI, aes(x = NDVI1, y = LL_change_1)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", label.x = -0.2,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Change in non-seasonal Leaf Litterfall", x = "Change in NDVI1") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())
ggsave("change_NDVI1_LL1.png", width = 5, height = 5, path = data_directory)

# LL change and NDVI2
ggplot(NDVI, aes(x = NDVI2, y = LL_change_2)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE) +
  stat_cor(method = "pearson", size = 4,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  labs(y = "Change in seasonal Leaf Litterfall", x = "Change in NDVI2") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 13, color = "Black"),
        axis.text = element_text(size = 13, color = "Black"),
        panel.grid.minor = element_blank())
ggsave("change_NDVI2_LL2.png", width = 5, height = 5, path = data_directory)

