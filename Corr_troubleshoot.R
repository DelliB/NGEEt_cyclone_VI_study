# Dellena Bloom
# July 15th and July 22nd, 2021
# Correlation plot troubleshooting for LAI

## Organizing Environment
# Load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggcorrplot)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# Load data
VI <- read.csv(file.path(data_directory, "case_study_data.csv"), 
               stringsAsFactors = FALSE)

# axis labels
x = expression(paste(ln,"(soil P (mg/kg))"))
dLAI1 = expression(Delta*"LAI annual (500m)")
dLAI2 = expression(Delta*"LAI sub-annual (500m)")

# setup dataframe
VI <- VI %>%
  select(soil.P..mg.kg., change_annual_LAI_500m, change_annual_EVI_250m)

## Change in LAI vs soil P scatterplot for comparison
ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_LAI_500m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black") +
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
  annotate("text", x = 7, y = 0.05,
           label = c("Baseline"))

## Correlation plots
# Renaming variables
VI <- VI %>%
  rename("annual change in LAI 500m" = change_annual_LAI_500m)
VI$soil.P..mg.kg. <- log(VI$soil.P..mg.kg.)
VI <- VI %>%
  rename("soil phosphorus" = soil.P..mg.kg.)
VI <- VI %>%
  rename("annual change in EVI 250m" = change_annual_EVI_250m)

# Corr plot
names(VI)
cor_new <- VI[,c(1, 2, 3)]
cor_new <- na.omit(cor_new) # has 21 values in length
str(cor_new) #check and see if you got it right

# correlations
corr_1to21 <- round(cor(cor_new, method = "pearson"), 1)
p.mat_1to21 <- cor_pmat(corr_1to21)
corr_1to21

# make figure
Fig_corr <- ggcorrplot(corr_1to21, hc.order = TRUE, type = "lower", 
                       hc.method = "ward.D2", outline.col = "white", 
                       p.mat = p.mat_1to21, method="square", 
                       ggtheme=ggplot2::theme_bw(), show.legend=TRUE, 
                       legend.title="Pearson's r", lab=TRUE, lab_size=5, 
                       tl.cex=14,colors = c("#003f5c", "white", "#ffa600", 
                                            pch.cex=20, nbreaks = 8, 
                                            legend.text.cex=20))
Fig_corr


