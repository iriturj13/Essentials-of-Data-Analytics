rm(list = ls())
#install.packages('cluster')
#install.packages('ClusterR')
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\Lab 6")
dt=read.csv("autos.csv")
View(dt)
summary(dt)
dt$system=as.factor(dt$system)
str(dt)

#price distr based on fuel system
pdt=dt[,c(7,12)]
plot(pdt,main="Price based on fuel System")
km=kmeans(pdt,2)    #
plot(pdt,col=(km$cluster+1)) # when k(  ,3)
km

#checking for optimal 'k'
dt2=pdt
ss=(nrow(dt2)-1)*sum(apply(dt2,2,var))
for(i in 2:10) ss[i]=sum(kmeans(dt2,centers = i)$withinss)

plot(1:10,ss,type = 'b',xlab='k',ylab='distortion')

