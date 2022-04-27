rm(list = ls())
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\Lab 5")
#install.packages('naivebayes')
#install.packages('psych')
library(naivebayes)
library(dplyr)
library(ggplot2) 
library(psych)
credit=read.csv("CreditWorthiness.csv")
str(credit)
credit$credit.status <- as.factor(credit$credit.status)
credit$education <- as.factor(credit$education)
credit$m.status <- as.factor(credit$m.status)
credit$Oparties <- as.factor(credit$Oparties)
credit$Duration <- as.factor(credit$Duration)
credit$inPlans <- as.factor(credit$inPlans)
credit$JobType <- as.factor(credit$JobType)
credit$Ndepend <- as.factor(credit$Ndepend)
credit$telephone <- as.factor(credit$telephone)
credit$foreign <- as.factor(credit$foreign)
credit$creditScore <- as.factor(credit$creditScore)
str(credit)
pairs.panels(credit) # Check the independance of attributes

credit %>%
  ggplot(aes(x=education,y=JobType,fill=education))+
  geom_boxplot()+
  ggtitle('Admit Box Plot Based on GRE Score')


credit %>%
  ggplot(aes(x=JobType,fill=admit))+
  geom_density(alpha=0.75,color='black')+
  ggtitle('Density')

set.seed(234)
smpl=sample(2,nrow(credit),replace=T,prob=c(0.8,0.2))
train=credit[smpl==1,]
test=credit[smpl==2,]

#P(Admit=1|Rank=1)=?

mdl=naive_bayes(JobType~ .,data=train)
mdl
plot(mdl)

p=predict(mdl,train,type='prob')
head(cbind(p,train))

#To find the accuracy of prediction

p1=predict(mdl,train)
(tab1=table(p1,train$education))
1-sum(diag(tab1))/sum(tab1)

