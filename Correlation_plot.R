# Dellena Bloom
# March 23rd, 2021
# Correlation plot

# load packages
library(tidyverse)
library(ggcorrplot)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
NDVI <- read.csv(file.path(data_directory, "new_Disturbance_details_NDVI.csv"), stringsAsFactors = FALSE)

#prepare data frame including the variables of interest

#check column number for the variables you want to include in the correlation plot
names(NDVI)

#here include the column number of each variable you want to include in the correlation plot
cor_new <- NDVI[,c(12,35,44)]
str(cor_new)#check and see if you got it right

corr_1to21 <- round(cor(cor_new,method="pearson"),1)
p.mat_1to21 <- cor_pmat(corr_1to21)

Fig_corr<-ggcorrplot(corr_1to21, hc.order = TRUE, type = "lower",hc.method = "ward.D2",
                      outline.col = "white", p.mat = p.mat_1to21,method="square",ggtheme=ggplot2::theme_bw(),
                     show.legend=TRUE, legend.title="Pearson's r", lab=TRUE, lab_size=5, tl.cex=14,colors = c("#003f5c", "white", "#ffa600",pch.cex=20,nbreaks = 8,legend.text.cex=20))+
  font("legend.text",size=14)+font("legend.title", size=16)
Fig_corr

ggsave(filename = "Fig_corr.png",
       plot = Fig_corr, width = 12, height = 12, units = 'cm',
       scale = 2, dpi = 600)

#You can explore other hexidecimanl color codes here https://projects.susielu.com/viz-palette?colors=[%22#ffd700%22,%22#ffb14e%22,%22#fa8775%22,%22#ea5f94%22,%22#cd34b5%22,%22#9d02d7%22,%22#7142ff%22,%22#7c1717%22,%22#6f6565%22,%22#000000%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22
##END##