rm(list=ls())
library(ISLR2)
library(splines)
library(gam)
data('Wage')
attach(Wage)
colnames(Wage)
# maritl, jobclass, health_ins
Wage <- Wage[c('wage','maritl','jobclass','health_ins','health')]
summary(Wage)

train <- sample(1:nrow(Wage),nrow(Wage)*.8)
test <- (-train)

gam1 <- gam(wage~.,data=Wage[train,])
summary(gam1)
preds1 <- predict(gam1,newdata=Wage)
gam1.mse <- mean((preds1-Wage$wage[test])^2)

gam2 <- gam(I(wage>250)~.,data=Wage[train,])
summary(gam2)
preds2 <- predict(gam2,newdata=Wage)
gam2.mse <- mean((preds2-Wage$wage[test])^2)
table(jobclass,I(wage>250))
table(maritl,I(wage>250))
table(health_ins,I(wage>250))
table(health,I(wage>250))

gam3 <- gam(I(wage<85.38)~.,data=Wage[train,])
summary(gam3)
preds3 <- predict(gam3,newdata=Wage)
gam3.mse <- mean((preds3-Wage$wage[test])^2)

write.csv(Wage,'C:/Users/William H/Desktop/Spring_23/ML2/DataSets/wage.csv')
