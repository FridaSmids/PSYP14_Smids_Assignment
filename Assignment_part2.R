#Part 2. 
#I will load packages
library(psych)
library(ggplot2)
library(gridExtra)
library(lm.beta)
library(tidyverse)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)


#Assignment part 2
#load dataset
data_1<-read.csv("https://tinyurl.com/ha-dataset1")

#I will make the same changes to the data set that I did in part 1 of the assignment:
#make sex a factor:
data_1<-data_1 %>% mutate(sex=factor(sex))
levels(data_1$sex)
#Change STAI_trait value 4.2 to 42:
data_1c<-data_1 %>% mutate(STAI_trait=replace(STAI_trait, STAI_trait==4.2, 42))
#Change STAI_trait back to numeric:
as.numeric(data_1c$STAI_trait)
#Change data point in the pain variable that is 55, to 5. 
data_1c<-data_1c %>% mutate(pain=replace(pain, pain==55, 5))

#I will make the model with all variables except saliva
fullmodel<-lm(pain~age+sex+STAI_trait+pain_cat+cortisol_serum+mindfulness+IQ+weight+household_income, data=data_1c)
fullmodelsummary<-summary(fullmodel)
AIC(fullmodel)
lm.beta(fullmodel)
confint(fullmodel)

#I will check the full model's assumptions: 

#Outliers. Most of the predictors have already been plotted with the outcome. I will make plots for IQ, weight and household income as well. 
FMpainIQ<-data_1c %>% ggplot()+aes(x=IQ, y=pain)+geom_point()+geom_smooth(method="lm")
FMpainweight<-data_1c %>% ggplot()+aes(x=weight, y=pain)+geom_point()+geom_smooth(method="lm")
FMpainincome<-data_1c %>% ggplot()+aes(household_income, y=pain)+geom_point()+geom_smooth(method="lm")

M2painage<-data_1c %>% ggplot()+aes(x=age, y=pain)+geom_point()+geom_smooth(method="lm")
M2painsex<-data_1c %>% ggplot()+aes(x=sex, y=pain)+geom_point()+geom_smooth(method="lm")
M2painSTAI<-data_1c %>% ggplot()+aes(x=STAI_trait, y=pain)+geom_point()+geom_smooth(method="lm")
M2painpaincat<-data_1c %>% ggplot()+aes(x=pain_cat, y=pain)+geom_point()+geom_smooth(method="lm")
M2painmindful<-data_1c %>% ggplot()+aes(x=mindfulness, y=pain)+geom_point()+geom_smooth(method="lm")
M2painserum<-data_1c %>% ggplot()+aes(x=cortisol_serum, y=pain)+geom_point()+geom_smooth(method="lm")

plots_fullmodel<-grid.arrange(M2painage, M2painsex, M2painSTAI, M2painpaincat, M2painmindful, M2painserum, FMpainincome, FMpainweight, FMpainIQ, nrow=3)

#Cook's distance:
fullmodel %>% plot(which=4)
#All are below 0.05, again with quite a few being above 0.025

#Normality:
fullmodel %>% plot(which=2)
residuals_fullmodel<-enframe(residuals(fullmodel))
residuals_fullmodel %>% ggplot()+aes(x=value)+geom_histogram()
#It looks like the residuals are relatively normally distributed with some values being potentially problematic.
#I'll check the skew and kurtosis to be safe:
describe(residuals(fullmodel))
#The skew is at -0.17 and kurtosis is -0.05, this suggests its fine. 

#Linearity:
fullmodel %>% residualPlots()
#Looks good

#Homoscedasticty:
fullmodel %>% plot(which=3)
fullmodel %>% bptest()
fullmodel %>% ncvTest()
#Looks good. 

#No multicollinearity:
fullmodel %>% vif()
#All good. 

#I will now run a backward regression on this model.

backward_model<-step(fullmodel, direction="backward")

summary(backward_model)
AIC(backward_model)
lm.beta(backward_model)
confint(backward_model)

#I will do model diagnostic on the backward_model. 
#outliers: Cook's distance:
backward_model %>% plot(which=4)
#Once again there are no values above .06 but no values below .07. 

#Normality:
backward_model %>% plot(which=2)
residuals_backward<-enframe(residuals(backward_model))
residuals_backward %>% ggplot()+aes(x=value)+geom_histogram()

#I'll check the skew and kurtosis to be safe:
describe(residuals(backward_model))
#The skew is at -0.18 and kurtosis is 0.08, this suggests its fine. 

#Linearity:
backward_model %>% residualPlots()
#Looks good

#Homoscedasticty:
backward_model %>% plot(which=3)
backward_model %>% bptest()
backward_model %>% ncvTest()
#Looks good. 

#No multicollinearity:
backward_model %>% vif()
#All good. 

#I will do an ANOVA comparing the backward model and the full model:
anova(backward_model, fullmodel)

#I will do an ANOVA comparing the backward model and the final model from part 1. 
theorybased_model<-lm(pain~age+sex+STAI_trait+pain_cat+cortisol_serum+mindfulness,data=data_1c)
anova(theorybased_model, backward_model)

#I download the new data:

data2<-read.csv("https://tinyurl.com/87v6emky")
#I'll check the structure and summary of data 2
str(data2)
summary(data2)
#I'll make sex a factor: 
data_2c<-data2 %>% mutate(sex=factor(sex))
levels(data_2c$sex)

#I will use the new data set to test the two models. 
prediction_theorymodel<-predict(theorybased_model, data_2c)
prediction_backmodel<-predict(backward_model, data_2c)

#I then calculate the sum of squared residuals:
RSS_theory=sum((data_2c[,"pain"]- prediction_theorymodel)^2)
RSS_backward=sum((data_2c[,"pain"]- prediction_backmodel)^2)
RSS_theory
RSS_backward




