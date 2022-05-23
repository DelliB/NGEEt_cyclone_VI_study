# Dellena Bloom
# July 15th and July 22nd, 2021
# Adding regression equation to plots and mixed-effects

### Organizing Environment
# load packages
library(tidyverse)
library(ggpubr)
library(multcompView)
library(ggpmisc)
library(lme4)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
VI <- read.csv(file.path(data_directory, "case_study_data.csv"), stringsAsFactors = FALSE)

# axis labels
x = expression(paste(ln,"(soil P (mg/kg))"))
dLAI1 = expression(Delta*"LAI annual (500m)")
dLAI2 = expression(Delta*"LAI sub-annual (500m)")
dTL1 = expression(Delta*"TL annual")

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


### Regressions
## without log transformation of soil P
LAIsoilP <- lm(change_annual_LAI_500m ~ soil.P..mg.kg., data = VI)
summary(LAIsoilP)
# Estimate = -0.00045; Intercept = -0.053; Multiple R^2 = 0.1611; p-values = 0.0713


## with log transformation of soil P
LAIsoilPln <- lm(change_annual_LAI_500m ~ log(soil.P..mg.kg.), data = VI)
summary(LAIsoilPln)
# Estimate/slope = -0.19528; Intercept = 0.87; Multiple R^2 = 0.223; p-value = 0.0307


## with log transformation of soil P and wind speed considered
LAIsoilPln <- lm(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + peak_wind_speed_ms, data = VI)
summary(LAIsoilPln)
# how should I write this equation?
# WSEstimate = -0.015386; sPEstimate = -0.073314; Intercept = 0.795254; Multiple R^2 = 0.4582; 
# p-value = 0.00743


## Wind speed and change in LAI
LAIws <- lm(change_annual_LAI_500m ~ peak_wind_speed_ms, data = VI)
summary(LAIws)
# Estimate = -0.017885; Intercept = 0.471484; Multiple R^2 = 0.442; p-value = 0.001898


## LAI and total ground litter fall
# if one predictor then use regular R-squared, but use adjusted with multiple
LAIaTL <- lm(change_annual_total_litterfall ~ change_annual_LAI_500m, data = VI)
summary(LAIaTL)
# Estimate = -3.1142; Intercept = 1.7115; Multiple R^2 = 0.2741; p-values = 0.01485


### Mixed effects
# Mixed effects- model that includes both random and fixed effects
## Region
# use region as a mixed-effect because it combines the effects of site and cyclone
# The t-value is significant when greater then the absolute value of 2
# soil P
LAIsoilPr <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + (1|Region), data = VI)
summary(LAIsoilPr)
# the effects from the soil P alone are not significant (t-value = -1.693)

# Wind speed and soil P
LAIsoilPrws <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + peak_wind_speed_ms 
                      + (1|Region), data = VI)
# random effect is region (is not expected to affect the change in LAI) and fixed effects 
# are wind speed and soil P (because these are known to effect the damage from a cyclone)
# fixed effects- different values are expected to have different effects
# In these models, multiplications allow to asses interactions thresholds and points 
# where effects are not had anymore
summary(LAIsoilPrws)
# the effect from the wind speed is significant (t-value = -2.509) and negative while soil P's 
# effect is not-significant (t-value = -0.690) when considering the mixed effects from Region

# Testing interactions between soil P and wind speed
LAIsoilPr <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) * peak_wind_speed_ms + 
                    (1|Region), data = VI)
summary(LAIsoilPr)
# testing interactive effect of soil P and wind speed on change in LAI
# gives a new estimate for the interaction and variance fell to 0, meaning it did not work 
# super well; the t-value was also too small to be significant


## Site
# soil P
LAIsoilPS <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + (1|Site), data = VI)
summary(LAIsoilPS)
# significant negative effect of soil P when considering mixed effects from site (t-value = -2.335)

# Wind speed and soil P
LAIsoilPSws <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + peak_wind_speed_ms 
                      + (1|Site), data = VI)
summary(LAIsoilPSws)
# wind speed has a significant effect (t-value = -2.509) while soil P does not (t-value = -0.690) 
# when considering mixed-effects of site


## Cyclone
# soil P
LAIsoilPc <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + (1|Cyclone_name), data = VI)
summary(LAIsoilPc)
# soil P does not have a significant impact when considering cyclone's mixed-effects (t-value = -1.232)

# Wind speed and soil P
LAIsoilPcws <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + peak_wind_speed_ms 
                      + (1|Cyclone_name), data = VI)
summary(LAIsoilPcws)
# wind speed has a significant effect (t-value = -3.127), while soil P does not (t-value = -0.339) 
# in considering mixed-effects on cyclone


## Site and Cyclone
# soil P
LAIsoilPsc <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + (1|Site) + (1|Cyclone_name), 
                   data = VI)
summary(LAIsoilPsc)
# soil P does not have a significant effect (t-value = -1.322) in considering mixed effects of site 
# and cyclone

# Wind speed and soil P
LAIsoilPscws <- lmer(change_annual_LAI_500m ~ log(soil.P..mg.kg.) + peak_wind_speed_ms + (1|Site)
                      + (1|Cyclone_name), data = VI)
summary(LAIsoilPscws)
# wind speed has a significant effect (t-value = -2.975), while soil P does not (t-value = -0.372)
# in considering the mixed-effects of site and cyclone


### Plots
# change in LAI vs soil P with regression equation
ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_LAI_500m)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black") +
  stat_regline_equation(label.y = .52) +
  stat_cor(label.y = .42, aes(label = paste(..rr.label.., ..p.label.., 
           sep = "~`, `~"))) +
  xlim(4.2, 7.6) +
  labs(x = x, y = dLAI1) +
  geom_hline(yintercept=c(0), color = "gray", linetype = "dashed") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 7.4, y = 0.05,
           label = c("Baseline"))
ggsave("MODIS_LAI_soilP_equation.png", width = 4, height = 3, path = data_directory)

# change in LAI vs soil P with regression equation and wind speed by color
ggplot(VI, aes(x = log(soil.P..mg.kg.), y = change_annual_LAI_500m, 
               color = peak_wind_speed_ms)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", 
              formula = my.formula) +
  stat_regline_equation(label.y = .52) +
  stat_cor(label.y = .42, aes(label = paste(..rr.label.., ..p.label.., 
           sep = "~`, `~"))) +
  xlim(4.2, 7.6) +
  labs(x = x, y = dLAI1, color = "Wind\nspeed\n(m/s)") +
  geom_hline(yintercept=c(0), color = "gray", linetype = "dashed") +
  scale_color_gradient(low = "light blue", high = "navy") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 7.2, y = 0.05,
           label = c("Baseline"))
ggsave("MODIS_LAI_soilP_wind_equation.png", width = 4, height = 3, path = data_directory)

# change in LAI vs change in total LF
ggplot(VI, aes(x = change_annual_LAI_500m, y = change_annual_total_litterfall)) +
  geom_point(size = 1) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", 
              formula = my.formula) +
  stat_regline_equation(label.y = 6) +
  stat_cor(label.y = 5.5, aes(label = paste(..rr.label.., ..p.label.., 
           sep = "~`, `~"))) +
  labs(x = dLAI1, y = dTL1) +
  geom_hline(yintercept=c(0), color = "gray", linetype = "dashed") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        plot.title = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 13, color = "black"),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -0.65, y = 0.3,
           label = c("Baseline"))
ggsave("MODIS_LAI_TLF_equation.png", width = 3, height = 3, path = data_directory)


### testing plot
## LAI and litterfall regression
LAIaTL <- lm(change_annual_total_litterfall ~ change_annual_LAI_500m, data = VI)
summary(LAIaTL)

# plot1
coeff=coefficients(LAIaTL)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(LAIaTL, main=eq) +
abline(reg, col="blue")

# plot2
plot(change_annual_total_litterfall ~ change_annual_LAI_500m, data = VI) +
  abline(LAIaTL, col="blue")


