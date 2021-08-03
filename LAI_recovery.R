### Organizing Environment
# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggcorrplot)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
recovery <- read.csv(file.path(data_directory, "LAI500m_recovery.csv"), stringsAsFactors= FALSE)

# axis labels
LAIreco = expression(Delta*"LAI Recovery")

### Timeseries
## Pantropical data calculation
# EVI
LAIm <- recovery %>%
  drop_na() %>%
  group_by(month) %>%
  summarize(m = mean(LAI_change_500m),
            SD = sd(LAI_change_500m),
            n = length(LAI_change_500m))
LAIm <- merge(LAIm, recovery, by = "month")
error <- qnorm(0.975) * LAIm$SD/sqrt(LAIm$n)
# this is standard error
# .975 if having a normal distribution and .95 if not?
LAIm$min = LAIm$m - error
LAIm$max = LAIm$m + error

## Plot
# LAI
ggplot(LAIm, aes(x = month, y = LAI_change_500m, group = case_study, color = region)) +
  geom_line(data = LAIm[!is.na(LAIm$LAI_change_500m),]) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max), 
              fill = "light blue", alpha = 0.05, color = NA) +
  geom_line(aes(x = month, y = m, color = "Pantropical mean")) +
  labs(x = "Time (months)", y = LAIreco, color = "Region") +
  ylim(-1.5, 1.5) +
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
ggsave("recovery_LAI.png", width = 7, height = 5, path = data_directory)

