#Load packages
library(psych)
library(r2glmm)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cAIC4)
library(MuMIn)
library(tidyverse)


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"] 
  se <- se.fixef * sdx/sdy 
  return(data.frame(stdcoef = sc, stdse = se))
  }


#Load the data sets:

data3<-read.csv("https://tinyurl.com/b385chpu")
data4<-read.csv("https://tinyurl.com/4f8thztv")

#Check out the data in data3
str(data3)
#I will make sex and hospital into factors:
data_3c<-data3 %>% mutate(sex=factor(sex))
levels(data_3c$sex)
data_3c<-data_3c %>% mutate(hospital=factor(hospital))
levels(data_3c$hospital)
#I notice when checking the levels of sex that there are three levels: female, male and woman. I will fix the error and make "woman" into "female". 
data_3c<-data_3c %>% mutate(sex=replace(sex,sex=="woman", "female"))
#and then remove "woman" as a factor (as it is still indicated as a level with 0 values in summary)
data_3c<-data_3c %>% droplevels(data_3c$sex)

#I will run a summary of the data
summary(data_3c)

#I notice that household income variable has a participant with an income value of -7884. I am not too concerned as I know that income will not be used in the analysis (which is why I also will not be concerned with the participant with a very high income) but I will have a closer look at the participant to see if they should be removed. 
#I am unsure of what to do. On one hand the other values look fine so it could just be an error made and that the minus sign has to be removed HOWEVER, the mean household income is around 70000 and the person has -7884, so there could be a missing digit. 
#Once again I will not use the household income variable for analysis so I could include the participant anyways and change the value to missing. 
#I have decided to replace it with N/A:
data_3c<-data_3c %>% mutate(household_income=na_if(household_income, "-7884")) 

#I'll make a big thing with scatterplots for each predictor and the outcome with the hospital differences. See if it makes sense. 
plotpainagehospital<-data_3c %>% ggplot()+aes(x=age, y=pain, color=hospital)+geom_point()+geom_smooth(method="lm", se=F)
plotpainsexhospital<-data_3c %>% ggplot()+aes(x=sex, y=pain, color=hospital)+geom_point()+geom_smooth(method="lm", se=F)
plotpainSTAIhospital<-data_3c %>% ggplot()+aes(x=STAI_trait, y=pain, color=hospital)+geom_point()+geom_smooth(method="lm", se=F)
plotpainPCShospital<-data_3c %>% ggplot()+aes(x=pain_cat, y=pain, color=hospital)+geom_point()+geom_smooth(method="lm", se=F)
plotpaincortisolhospital<-data_3c %>% ggplot()+aes(x=cortisol_serum, y=pain, color=hospital)+geom_point()+geom_smooth(method="lm", se=F)
plotpainMAAShospital<-data_3c %>% ggplot()+aes(x=mindfulness, y=pain, color=hospital)+geom_point()+geom_smooth(method="lm", se=F)

mixedlinearmodelplots<-grid.arrange(plotpainagehospital, plotpainsexhospital, plotpainSTAIhospital, plotpainPCShospital, plotpaincortisolhospital, plotpainMAAShospital, nrow=2)

#These plots are very messy! But oh well...

#I'll make a random intercept model with pain as outcome, random intercept is hospital 
#and fixed effect predictors is age, sex, STAI, PCS, cortisol (serum), MAAS. 
modelwithhospital = lmer(pain ~ sex+ age+ STAI_trait+ pain_cat+cortisol_serum+mindfulness + (1 | hospital), data = data_3c) 
summary(modelwithhospital)

#RSS
sum(residuals(modelwithhospital)^2)

#cAIC
cAIC(modelwithhospital)$caic

#Marginal R^2+CI
r2beta(modelwithhospital, method="nsj", data=data_3c)

#CI for the model
confint(modelwithhospital)

#Marginal and conditional R squared values
r.squaredGLMM(modelwithhospital)

#Std coefficients:
stdCoef.merMod(modelwithhospital)

#Data 4
summary(data4)

#Make hospital a factor
data_4c<-data4 %>% mutate(hospital=factor(hospital))
levels(data_4c$hospital)

#Make sex a factor
data_4c<-data4 %>% mutate(sex=factor(sex))
levels(data_4c$sex)

summary(data_4c)

#I will make a prediction using the hospital model
prediction_hospitalmodel<-predict(modelwithhospital, data_4c, allow.new.levels=TRUE)

#RSS
RSS_modelhospitalpredict=sum((data_3c[,"pain"]- prediction_hospitalmodel)^2)
RSS_modelhospitalpredict

#TSS
TSS_modelhospitalpredict=sum((data_4c$pain - predict(modelwithhospital))^2)

#Calculate the R^2:
R2modeldata4<-1-(RSS_modelhospitalpredict/TSS_modelhospitalpredict)
R2modeldata4

#I will make a new model on data set 3. Pain is outcome. The most influential predictor from the model with hospital is cortisol serum. 

#I'll make a plot:
slopepaincortisolhospital<-data_3c %>% ggplot()+aes(y=pain, x=cortisol_serum, color=hospital)+geom_point(size=3)+geom_smooth(method="lm", se=F, fullrange=TRUE)

#I'll make separate plots for each hopital.
slopepaincortisolhospital2<-data_3c %>% ggplot()+aes(y=pain, x=cortisol_serum, color=hospital)+geom_point(size=3)+geom_smooth(method="lm", se=F, fullrange=TRUE)+ facet_wrap(~hospital, ncol = 2)

#I'll make a random slope model:
randomslopemodel<-lmer(pain~cortisol_serum+(cortisol_serum|hospital),data=data_3c)
#When doing this I get a warning of singular fit. 

#RSS
sum(residuals(randomslopemodel)^2)

#cAIC
cAIC(randomslopemodel)$caic

#Marginal R^2+CI
r2beta(randomslopemodel, method="nsj", data=data_3c)

#CI for the model
confint(randomslopemodel)

#Marginal and conditional R squared values
r.squaredGLMM(randomslopemodel)

#Std coefficients:
stdCoef.merMod(randomslopemodel)

summary(randomslopemodel)


