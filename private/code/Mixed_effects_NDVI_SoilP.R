## Running linear mixed effects model for NDVI as a function of soil P

library(lme4)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tidyverse)

ndvi<-read.csv(file.choose())
str(ndvi)
head(ndvi)

data_directory <- '~/Documents/Kueppers lab'

#I think this is the best mixed effects model
lme_ndvi<-lmer(NDVI1~log(soil.P)+(1|Site.name), data=ndvi)#+(1|Site)
summary(lme_ndvi)
tab_model(lme_ndvi)
#ggsave("Results_mixedeffects_sitename.png", width = 5, height = 5, path = data_directory)
r.squaredGLMM(lme_ndvi)
pred<-predict(lme_ndvi)
pred
fitted(lme_ndvi)

##Plotting predictions vs observations
xyplot(pred~ndvi$NDVI1)



lme_ndvi2<-lmer(NDVI1~log(soil.P)+(1|Site.name/Dist.name), data=ndvi)#+(1|Site)
summary(lme_ndvi2)
tab_model(lme_ndvi2)

lme_ndvi3<-lmer(NDVI1~log(soil.P)+(1|Dist.name), data=ndvi)#+(1|Site)
summary(lme_ndvi3)
tab_model(lme_ndvi3)

lme_ndvi4<-lmer(NDVI1~log(soil.P)+(1|Site.name)+(1|Dist.name), data=ndvi)#+(1|Site)
summary(lme_ndvi4)
tab_model(lme_ndvi4)

lme_ndvi5<-lmer(NDVI1~log(soil.P)+(1|Dist.name|Site.name), data=ndvi)#+(1|Site)
summary(lme_ndvi5)
tab_model(lme_ndvi5)

AIC(lme_ndvi,lme_ndvi2,lme_ndvi3,lme_ndvi4)
