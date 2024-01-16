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
library(tidyverse)
library(xgboost)
library(caret)
library(readxl)

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

mod4=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL + FF.Abs +(AV.dB|  sujet), data = train, REML = FALSE) 
RMSE4= rmse(valid$score,predict(mod4, newdata = valid))

genre=train$genre
sujet=train$sujet
score=train$score
duree=train$duree
train=subset(train,select=-genre)
train=subset(train,select=-sujet)
train=subset(train,select=-score)
train=subset(train,select=-duree)
train=scale(train)
train=as.data.frame(train)
train$genre=genre
train$sujet=sujet
train$score=score
train$duree=duree

genre=valid$genre
sujet=valid$sujet
score=valid$score
valid=subset(valid,select=-genre)
valid=subset(valid,select=-sujet)
valid=subset(valid,select=-score)
valid=scale(valid)
valid=as.data.frame(valid)
valid$genre=genre
valid$sujet=sujet
valid$score=score

mod4=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL+AV.dB +(FF.Abs|  sujet), data = train, REML = FALSE) 
RMSE4= rmse(valid$score,predict(mod4, newdata = valid))

mod5=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL + FF.Abs +(AV.dB|  sujet), data = train, REML = FALSE) 
RMSE5= rmse(valid$score,predict(mod5, newdata = valid))

mod6=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL +(FF.Abs+AV.dB|  sujet), data = train, REML = FALSE) 
RMSE6= rmse(valid$score,predict(mod6, newdata = valid))

#TROP GROS MODELE NE CONVERGE PAS
mod7=lmer(score ~ age +genre+duree+FF.Abs+AV.dB+BTC1+BTC2+EFS+VFNL+CDNL +(duree+FF.Abs+AV.dB+BTC1+BTC2+EFS+VFNL+CDNL|  sujet), data = train, REML = FALSE) 
RMSE7= rmse(valid$score,predict(mod7, newdata = valid))

mod8=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL+CDNL +(FF.Abs+AV.dB|  sujet), data = train, REML = FALSE) 
RMSE8= rmse(valid$score,predict(mod8, newdata = valid))

mod9=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL + FF.Abs +CDNL+(AV.dB|  sujet), data = train, REML = FALSE) 
RMSE9= rmse(valid$score,predict(mod9, newdata = valid))

mod10=lmer(score ~ age +genre+duree+BTC1+BTC2+EFS+VFNL + FF.Abs +AV.dB +(1|  sujet), data = train) 
RMSE10= rmse(valid$score,predict(mod10, newdata = valid))

m10update<-update(mod10, REML=FALSE)
RMSE10_UPDATE= rmse(valid$score,predict(m10update, newdata = valid))

sort(as.matrix(AIC(mod4,mod5,mod6,mod7,mod8,mod9,mod10,m10update))[,2])%>%data.frame()%>% rmarkdown::paged_table()
sort(as.matrix(BIC(mod4,mod5,mod6,mod7,mod8,mod9,mod10,m10update))[,2])%>%data.frame()%>% rmarkdown::paged_table()

valid$pred_mod_m1 <- predict(mod7,newdata=valid)

valid %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = duree, y = score), color="red", size=3) + 
  geom_line(aes(x = duree, y = pred_mod_m1)) + facet_wrap(~ sujet, ncol=4) 


train$pred_mod_m1 <- fitted(mod7)

train %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = duree, y = score), color="red", size=3) + 
  geom_line(aes(x = duree, y = pred_mod_m1)) + facet_wrap(~ sujet, ncol=4) 


#XG BOOST
Y_train=train$score
X_train=subset(train,select=-score)
Y_test=valid$score
X_test=subset(valid,select=-score)


xgb_trcontrol = trainControl(method = "cv", number = 5, allowParallel = TRUE, 
                             verboseIter = FALSE, returnData = FALSE)

xgbGrid <- expand.grid(nrounds = c(100,200),  
                       max_depth = c(3, 5, 10, 15, 20),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## valeurs par défaut : 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample=1
)

xgb_model = train(X_train, Y_train, trControl = xgb_trcontrol, tuneGrid = xgbGrid, 
                  method = "xgbTree")

xgb_model$bestTune
predicted = predict(xgb_model, X_test)
residuals = Y_test - predicted
RMSE = sqrt(mean(residuals^2))

pred_mod_m1=predict(xgb_model,X_train)
train$pred_mod_m1=pred_mod_m1
train %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = duree, y = score), color="red", size=3) + 
  geom_line(aes(x = duree, y = pred_mod_m1)) + facet_wrap(~ sujet, ncol=4) 

valid$pred_mod_m1=predicted
valid %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = duree, y = score), color="red", size=3) + 
  geom_line(aes(x = duree, y = pred_mod_m1)) + facet_wrap(~ sujet, ncol=4) 

mod1=lm(score~duree,data=train)
valid$pred_mod1=predict(mod1,valid)
valid %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = duree, y = score), color="red", size=3) + 
  geom_line(aes(x = duree, y = pred_mod1)) + facet_wrap(~ sujet, ncol=4) 

pred_mod_m1=predict(mod1,train)
train$pred_mod_m1=pred_mod_m1
train %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = duree, y = score), color="red", size=3) + 
  geom_line(aes(x = duree, y = pred_mod_m1)) + facet_wrap(~ sujet, ncol=4) 

#RMSE DE 0.457# IL FAUT DOONC FAIRE MIEUX 

patient1<-train %>% filter(sujet %in% 1)
patient1$duree=round(patient1$duree)

resultat <- aggregate(patient1[c("age","genre","score","FF","FF.Abs","FF.RAP","FF.PPQ5","FF.DDP","AV","AV.dB","AV.APQ3","AV.APQ5","AV.APQ11","AV.DDA","BTC1","BTC2","CDNL","EFS","VFNL")], by = list(patient1$duree), FUN = mean)
ggplot(data=resultat)  + aes(x = FF.Abs, y = score) + geom_point()+geom_smooth()  
ggplot(data=patient1)  + aes(x = FF.Abs, y = score) + geom_point()+geom_smooth()  

train$duree=round(train$duree)
patients<-aggregate(train[c("sujet","age","genre","score","FF","FF.Abs","FF.RAP","FF.PPQ5","FF.DDP","AV","AV.dB","AV.APQ3","AV.APQ5","AV.APQ11","AV.DDA","BTC1","BTC2","CDNL","EFS","VFNL")], by = list(train$duree,train$sujet), FUN = mean)
patients$sujet=patients$Group.2
patients$duree=patients$Group.1
patients=subset(patients,select=-Group.1)
patients=subset(patients,select=-Group.2)

selected=c(3,7,11,17,21,31,32,41)
patients %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x = duree, y = VFNL)) + geom_point(aes(x=duree,y=score),col='red')+ facet_wrap(~ sujet, ncol=2)

patients %>% filter(sujet %in% selected) %>% 
  ggplot() + geom_point(aes(x =VFNL, y = score)) + facet_wrap(~ sujet, ncol=2)

ggplot(data = train)+geom_point(aes(x = duree, y = score))+geom_smooth()

#Modèle non Mixte
data2<-as.data.frame(train$score)
data2$duree<-train$duree
nlm1b <- nls(data2$`train$score` ~  a*x^2+b*x+c, data=data2, start = c(a=1,b=1,c=1), algorithm = "port")
Rmse_1<-rmse(predict(nlm1b,data2),data2$`train$score`)


troisieme_quartile <- quantile(train$duree, 0.75)
valid=train[train$duree>105,]
train=train[train$duree<=105,]
