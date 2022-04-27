rm(list=ls())
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\Lab 3")
football=read.csv("Football.csv")
football
club=c(rep('CSK',17),rep('MI ',17),rep('KKR ',17),rep('DD ',17),rep('RR ',17))
club
wt=c(football$CSK,football$MI,football$KKR,football$DD,football$RR)
wt
df=data.frame(club,wt)
df
library('dplyr')
boxplot(wt~club,data=df,xlab="Team",ylab="weight",main="exercises")
av=aov(wt~club,data=df)
summary(av)

