rm(list=ls())
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\Lab 2")
Palmoil=read.csv("Palmoil.csv")
#install.packages('forecast')
#install.packages('tseries')
class(Palmoil)
palmoil_time=ts(Palmoil$Dollar,start=1,end=128,frequency = 4)
class(palmoil_time)
plot(palmoil_time)
acf(palmoil_time)
pacf(palmoil_time)
library('forecast')
library('tseries')
adf.test(palmoil_time)
#myfc=forecast(palmoil_time,level=c(95),h=10*4)
#myfc
#plot(myfc)
mypo=auto.arima(palmoil_time,ic="aic",trace=TRUE)
mypo
acf(ts(mypo$residuals))
myfc=forecast(mypo,level=c(95),h=10*4)
myfc
plot(myfc)
  





#Tractor sales(Non stationary data)
rm(list=ls())
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\Lab 2")
tractorSales=read.csv("Tractor_Sales.csv")
class(tractorSales)
Ts=ts(tractorSales$Number.of.Tractor.Sold,start=1,frequency = 12)
class(Ts)
dTs=diff(Ts)
plot(dTs)
lTs=log10(Ts)
plot(lTs)
dlTs=diff(lTs)
plot(dlTs)
ddlTs=diff(dlTs) #incase,the variance dsnt seem almost constant, we go for another time differentiating the data
plot(ddlTs)
par(mfrow=c(1,2))
acf(ddlTs,main='AutoCorrelationFactor')
pacf(ddlTs,main="PartialACfactor")
library(forecast)
library(tseries)
ar=auto.arima(ddlTs,ic="aic",trace=TRUE)
attributes(ar)
par(mfrow=c(1,2))
myfc=forecast(ar,level=c(95),h=3*12)
plot(myfc)
#myfc=predict(ar,n.ahead=36)
#myfc
par(mfrow=c(1,2))
acf(ts(ar$residuals),main='ACF Residual')
pacf(ts(ar$residuals),main='PACF Residual')








#Homework_Set1

A=read.csv("Soyaoil.csv")
class(A)
A_time=ts(A$Dollar,start=1,end=127,frequency = 4)
class(A_time)
plot(A_time)
acf(A_time)
pacf(A_time)
library('forecast')
library('tseries')
adf.test(A_time)
#myfc=forecast(A_time,level=c(95),h=10*4)
#myfc
#plot(myfc)
mypo=auto.arima(A_time,ic="aic",trace=TRUE)
mypo
acf(ts(mypo$residuals))
myfc=forecast(mypo,level=c(95),h=10*4)
myfc
plot(myfc)


#Homework_Set2
rm(list=ls())
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\Lab 2")
crudeOil=read.csv("Crudeoil_NS.csv")
class(crudeOil)
co=ts(crudeOil$POILBREUSDQ,start=1,frequency = 12)
class(co)
library('forecast')
library('tseries')
adf.test(co)
dTs=diff(co)
plot(dTs)
lTs=log10(co)
plot(lTs)
dlTs=diff(lTs)
plot(dlTs)
ddlTs=diff(dlTs) #incase,the variance dsnt seem almost constant, we go for another time differentiating the data
plot(ddlTs)
par(mfrow=c(1,2))
acf(ddlTs,main='AutoCorrelationFactor')
pacf(ddlTs,main="PartialACfactor")
library(forecast)
library(tseries)
ar=auto.arima(ddlTs,ic="aic",trace=TRUE)
attributes(ar)
myfc=forecast(ar,level=c(95),h=3*12)
plot(myfc)
#myfc=predict(ar,n.ahead=36)
#myfc
par(mfrow=c(1,2))
acf(ts(ar$residuals),main='ACF Residual')
pacf(ts(ar$residuals),main='PACF Residual')

