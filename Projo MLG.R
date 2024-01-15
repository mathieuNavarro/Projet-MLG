rm(list=ls())
library(tidyverse)
library(lme4) # lmer(): To fit mixed-model
library(lmerTest) # lmer (): To fit mixed-model and diplay p-values
library(nlme) # To fit mixed-model
library(lattice) # To plot mixed-model
library(nlme) 
library(plotly)
library(ggplot2)
library(Metrics)
library(gridExtra)
library(dplyr)
library(MASS)
library(Matrix)

theme_set(theme_bw())

set.seed(2023)

train<-read.csv('train_maladie.csv', header = T, sep = ",",dec=".")
testX<-read.csv('test_X_maladie.csv', header = T, sep = ",",dec=".")

train%>% rmarkdown::paged_table()

#Séparation Train / Validation du modèle 

patients <- 1:42
patients_train <- sample(patients, 30, replace = FALSE)
patients_valid <- patients[!(patients %in% patients_train)]

data_valid <- subset(train, sujet %in% patients_valid)
data_train <- subset(train, sujet %in% patients_train)

#MODELE LINEAIRE BASIQUE
mod1=lm(score~.,data=data_train)
RMSE1= rmse(data_valid$score,predict(mod1, newdata = data_valid))

#SELECTION DE VARIABLE
stepAIC(mod1,~.,trace=T,direction=c("backward"))
mod2=lm(formula = score ~ sujet + age + genre + duree + FF.Abs + FF.RAP + 
          FF.DDP + AV.dB + AV.APQ11 + AV.DDA + BTC1 + BTC2 + CDNL + 
          EFS + VFNL, data = data_train)
RMSE2= rmse(data_valid$score,predict(mod2, newdata = data_valid))

#MODELE MIXTE
mod3=lmer(score ~ age +genre+duree+ (1|  sujet), data = data_train) 
RMSE3= rmse(data_valid$score,predict(mod3, newdata = data_valid))

pred<- fitted(mod3)
RMSE3<-rmse(pred,data_train$score)
pred<-predict(mod3,newdata=data_valid)

#NE MARCHE PAS CAR IL FAUT AVOIR CROISER LES SUJETS DONC ON NE SEPARE PAS PAR SUJET MAIS PAR NORMAL COMME DHAB

library(rsample) 
mydata_split <- initial_split(train, prop = .7)
train <- training(mydata_split)
valid  <- testing(mydata_split)

#MODELE FACILE
mod1=lm(score~.,data=train)
RMSE1= rmse(valid$score,predict(mod1, newdata = valid))

stepAIC(mod1,~.,trace=T,direction=c("backward"))
mod2=lm(formula = score ~ sujet + age + genre + duree + FF.Abs + FF.PPQ5 + 
          FF.DDP + AV + AV.dB + AV.APQ11 + AV.DDA + BTC1 + BTC2 + EFS + 
          VFNL, data = train)
RMSE2= rmse(valid$score,predict(mod2, newdata = valid))

#MODELE MIXTE:

mod3=lmer(score ~ age +genre+duree+ (1+ FF + FF.Abs + FF.RAP + 
                                       FF.PPQ5 + FF.DDP + AV + AV.dB + AV.APQ3 + AV.APQ5 + AV.APQ11 + 
                                       AV.DDA + BTC1 + BTC2 + CDNL + EFS + VFNL|  sujet), data = train) 

ggplot(data=train)  + aes(x = FF, y = FF.Abs) + geom_point()+geom_smooth()

ggplot(data = train)+aes(x=FF,y=FF.RAP)+geom_point()+geom_smooth()

ggplot(data=train)  + aes(x = FF, y = FF.PPQ5) + geom_point()+geom_smooth()

ggplot(data=train)  + aes(x = FF, y = FF.DDP) + geom_point()+geom_smooth()

selected=c(1,5,27,38,40,17)
train %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = FF.Abs, y = score)) + facet_wrap(~ sujet, ncol=2)



ggplot(data=train)  + aes(x = AV, y = AV.dB) + geom_point()+geom_smooth()

ggplot(data = train)+aes(x=AV,y=AV.APQ3)+geom_point()+geom_smooth()

ggplot(data=train)  + aes(x = AV, y = AV.APQ5) + geom_point()+geom_smooth()

ggplot(data=train)  + aes(x = AV, y = AV.APQ11) + geom_point()+geom_smooth()

ggplot(data=train)  + aes(x = AV, y = AV.DDA) + geom_point()+geom_smooth()

selected=c(1,5,27,38,40,17)
train %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = AV, y = score)) + facet_wrap(~ sujet, ncol=2)

ggplot(data=train)  + aes(x = BTC1, y = BTC2) + geom_point()+geom_smooth()

train$genre=as.factor(train$genre)

mod3=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL +(FF.Abs|  sujet), data = train, REML = FALSE) 
RMSE3= rmse(valid$score,predict(mod3, newdata = valid))

genre=train$genre
train=subset(train,select=-genre)
train=scale(train)
train$genre=genre

mod4=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL +(FF.Abs|  sujet), data = train, REML = FALSE) 
RMSE4= rmse(valid$score,predict(mod3, newdata = valid))