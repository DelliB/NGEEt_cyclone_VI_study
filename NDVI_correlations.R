# Dellena Bloom
# February 22nd, 2021
# Comparing field and RS data for Barbara

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
NDVI <- read.csv(file.path(data_directory, "new_Disturbance_details_NDVI.csv"), stringsAsFactors = FALSE)

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

# TL pre and NDVI1 pre
ggplot(NDVI, aes(x = NDVI1_pre, y = TL_pre)) +
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

# TL pre and NDVI2 pre
ggplot(NDVI, aes(x = NDVI1_pre, y = TL_pre)) +
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

# TL post and NDVI post


# TL change and NDVI1 and 2


# LL pre and NDVI1 and 2 pre


# LL post and NDVI post


# LL change and NDVI1 and 2

