#Packages. 
library(psych)
library(gridExtra)
library(lm.beta)
library(ggplot2)
library(tidyverse)

#load dataset
data_1<-read.csv("https://tinyurl.com/ha-dataset1")
str(data_1)
#make gender factors
data_1<-data_1 %>% mutate(sex=factor(sex))
levels(data_1$sex)
#get a summary of the data
summary=describe(data_1)
#I have identified that in the STAI_trait variable one data point that is incorrect. The data point is 4.2, I will assume that it is supposed to be 42.0 and change it. 
data_1c<-data_1 %>% mutate(STAI_trait=replace(STAI_trait, STAI_trait==4.2, 42))
summary(data_1corrected)
#I noticed that the edit I did above in the STAI_trait variable made it a character variable as opposed to a numerical so I will change that. 
as.numeric(data_1corrected$STAI_trait)
#I have decided to redo the change of the data point. 
data_1c<-data_1 %>% mutate(STAI_trait=replace(STAI_trait, STAI_trait==4.2, 42))
summary(data_1c)
#additionally there is one data point in the pain variable that is 55, I will assume it is supposed to be 5 and will change it to that. 
data_1c<-data_1c %>% mutate(pain=replace(pain, pain==55, 5))
summary(data_1c)
#I will make some relevant plots to view the data: 
#I will make a histogram of the distribution of the outcome variable (pain)
Paindistribution<-data_1c %>% ggplot()+aes(x=pain)+geom_histogram()
plot(Paindistribution)
#the outcome variable looks normally distributed. 
#I will make a table with the descriptive statistics.
descriptives<-data_1c %>% describe()
#The descriptive data gave very large decimal points so I will make those into two decimal points. This table will be used to make it easier for me when writing up the data.  
descriptivesrounded<-data_1c %>% describe() %>% round(digits=2)
#The first model has two predictors and one outcome so I will make two scatter plots that visualise them.
#The outcome will be on the y axis and the predictors will be on the x axis. 
painageplot<-data_1c %>% ggplot()+aes(x=age, y=pain)+geom_point()+geom_smooth(method="lm")
painsexplot<-data_1c %>% ggplot()+aes(x=sex, y=pain)+geom_point()+geom_smooth(method="lm")
#The plot about is fine but not very useful. I will do an additional plot with sex and age included
painagesexplot<-data_1c %>% ggplot()+aes(x=age, y=pain, color=sex) +geom_point()+geom_smooth(method="lm")+scale_color_manual(values=c('Dark Grey','Black')
painagesexplot+scale_color_manual(labels=c("Female","Male"))                                                                                                                            
painagesexplot+ggtitle("Model 1: The Effect of Age and Sex on Percieved Pain 5 Hours Post Surgery")+xlab("Age")+ylab("Self-rated Pain (1-10)")+labs(color="Sex")
#The plot above is a good plot to showcase model 1 I think. 

#I will now make model 1: 
model1<-lm(pain~age+sex, data=data_1c)
model1summary<-summary(model1)
AIC(model1)
lm.beta(model1)
confint(model1)

#I will now make model 2:
model2<-lm(pain~age+sex+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness, data=data_1c)
model2summary<-summary(model2)
AIC(model2)
lm.beta(model2)
confint(model2)

#I will check the assumptions for model 2: Outliers, high residual error, high leverage, normality, linearity, homoscedasticity, no multicollinearity
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(tidyverse)

#Check for outliers, high residual error and high leverage:
#I'll start by making 7 scatterplots

M2painage<-data_1c %>% ggplot()+aes(x=age, y=pain)+geom_point()+geom_smooth(method="lm")
M2painsex<-data_1c %>% ggplot()+aes(x=sex, y=pain)+geom_point()+geom_smooth(method="lm")
M2painSTAI<-data_1c %>% ggplot()+aes(x=STAI_trait, y=pain)+geom_point()+geom_smooth(method="lm")
M2painpaincat<-data_1c %>% ggplot()+aes(x=pain_cat, y=pain)+geom_point()+geom_smooth(method="lm")
M2painmindful<-data_1c %>% ggplot()+aes(x=mindfulness, y=pain)+geom_point()+geom_smooth(method="lm")
M2painserum<-data_1c %>% ggplot()+aes(x=cortisol_serum, y=pain)+geom_point()+geom_smooth(method="lm")
M2painsaliva<-data_1c %>% ggplot()+aes(x=cortisol_saliva, y=pain)+geom_point()+geom_smooth(method="lm")
model2plots<-grid.arrange(M2painage, M2painsex, M2painSTAI, M2painpaincat, M2painmindful, M2painserum, M2painsaliva, nrow=3)

#Cook's distance:
model2 %>% plot(which=5)
model2 %>% plot(which=4)

#I see some potential outliers. I will slice the data and make sure that there are no errors. I can not access the id number of all cases with higher than 0.025...
data_1c %>% slice(c(47,74,86)) 

#Looking at the Cook's distance results it seems like there are some values that are larger than the others. 
#They are all however below 1, which would be okay, unless I follow the other threshold 4/160=0.025. 

#Normality:
model2 %>% plot(which=2)
residuals_model2<-enframe(residuals(model2))
residuals_model2 %>% ggplot()+aes(x=value)+geom_histogram()
#It looks like the residuals are relatively normally distributed with some values being potentially problematic. 
#I'll check the skew and kurtosis to be safe: 
describe(residuals(model2))
#The skew is at -0.14 and kurtosis is 0, this suggest that the distribution of the residuals are not problematic. 

#Linearity:
model2 %>% residualPlots()
#Looks good. 

#Homoscedasticty:
model2 %>% plot(which=3)
model2 %>% bptest()
model2 %>% ncvTest()
#Looks good. 

#No multicollinearity:
model2 %>% vif()
#Cortisol serum and saliva violate this assumption. 
#Because the serum and saliva both measure cortisol, it makes sense that they would be highly correlated with oneanother. 
#The variables are not nested, indicating that the issue is not a structural multicollinearity but rather data collinearity. 
#I will confirm with a correlation matrix:
data_1c %>%
  select(age, sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness) %>%
  pairs.panels(col = "red", lm = T)
cor.test(data_1c$cortisol_saliva, data_1c$cortisol_serum)

#I will make a third model with age, sex, cortisol(serum), mindfulness, pain catastrophising, STAI trait as predictors and pain as outcome. 
model2<-lm(pain~age+sex+STAI_trait+pain_cat+cortisol_serum+cortisol_saliva+mindfulness, data=data_1c)
model2summary<-summary(model2)
AIC(model2)
lm.beta(model2)
confint(model2)

#model 3:
model3<-lm(pain~age+sex+STAI_trait+pain_cat+cortisol_serum+mindfulness, data=data_1c)
model3summary<-summary(model3)
AIC(model3)
lm.beta(model3)
confint(model3)

#I will check all the assumptions for model 3:

#outliers: since the actual data has not changed I will not do new scatterplots for the individual predictor's relation to the outcome. 
#Cook's distance:
model3 %>% plot(which=5)
model3 %>% plot(which=4)
#Same issue as with model 2, but none of the values exceed 1. 

#Normality:
model3 %>% plot(which=2)
residuals_model3<-enframe(residuals(model3))
residuals_model3 %>% ggplot()+aes(x=value)+geom_histogram()
#again it looks like the residuals are relatively normally distributed with some values being potentially problematic. 
#I'll check the skew and kurtosis to be safe: 
describe(residuals(model3))
#The skew and kurtosis is at -0.18 and 0.06, and are therefore not problematic. 

#Linearity:
model3 %>% residualPlots()
#Looks fine. 

#Homoscedasticty:
model3 %>% plot(which=3)
model3 %>% bptest()
model3 %>% ncvTest()
#Looks fine. 

#No multicollinearity:
model3 %>% vif()
#looks good

#I will do an ANOVA to compare the first and the third model. 
anova(model1, model3)



