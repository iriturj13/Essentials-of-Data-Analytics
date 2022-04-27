rm(list=ls())
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\LAB 1")
A = read.csv("Car_sales.csv")


#Linear Regression
RegModd=lm(Sales.in.thousands~Price.in.thousands,A)  #Y_Var~X_Var
summary(RegModd)
attributes(RegModd)
plot(RegModd$residuals)
qqnorm(RegModd$residuals)


#Multiple Regression
MulRegmod=lm(Sales.in.thousands~Price.in.thousands+Fuel.efficiency,A)
summary(MulRegmod)
attributes(MulRegmod)
plot(MulRegmod$residuals)
qqnorm(MulRegmod$residuals)
