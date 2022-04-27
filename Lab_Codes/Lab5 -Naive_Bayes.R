rm(list = ls())
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\Lab 5")
#install.packages('naivebayes')
#install.packages('psych')
library(naivebayes)
library(dplyr)
library(ggplot2) 
library(psych)
dt=read.csv("admissions_Naive Bayes.csv")
str(dt)
# we need to convert num to int
dt$admit=as.factor(dt$admit)
dt$rank=as.factor(dt$rank)

pairs.panels(dt) # Check the independance of attributes

dt %>%
  ggplot(aes(x=admit,y=gre,fill=admit))+
  geom_boxplot()+
  ggtitle('Admit Box Plot Based on GRE Score')


dt %>%
  ggplot(aes(x=gre,fill=admit))+
  geom_density(alpha=0.75,color='black')+
  ggtitle('Density')

set.seed(234)
smpl=sample(2,nrow(dt),replace=T,prob=c(0.8,0.2))
train=dt[smpl==1,]
test=dt[smpl==2,]

#P(Admit=1|Rank=1)=?

mdl=naive_bayes(admit~ .,data=train)
mdl
plot(mdl)

p=predict(mdl,train,type='prob')
head(cbind(p,train))

#To find the accuracy of prediction

p1=predict(mdl,train)
(tab1=table(p1,train$admit))
1-sum(diag(tab1))/sum(tab1)

