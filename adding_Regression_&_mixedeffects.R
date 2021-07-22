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


### Regressions
## without log transformation of soil P
LAIsoilP <- lm(change_annual_LAI_500m ~ soil.P..mg.kg., data = VI)
summary(LAIsoilP)
# Estimate = -0.00045; Intercept = -0.053; Multiple R^2 = 0.1611; p-values = 0.0713


## with log transformation of soil P
LAIsoilPln <- lm(change_annual_LAI_500m ~ log(soil.P..mg.kg.), data = VI)
summary(LAIsoilPln)
# Estimate/slope = -0.19528; Intercept = 0.87; Multiple R^2 = 0.223; p-value = 0.0307


### Mixed effects
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
summary(LAIsoilPrws)
# the effect from the wind speed is significant (t-value = -2.509) and negative while soil P's 
# effect is not-significant (t-value = -0.690) when considering the mixed effects from Region


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
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color = "black", 
              formula = my.formula) +
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

