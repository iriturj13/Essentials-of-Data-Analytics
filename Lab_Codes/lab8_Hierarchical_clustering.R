#For Euclidean Complete linkage
rm(list=ls())
library(cluster)
library(dplyr)

v1=c(1,3,9,3,7,9,4,8,1)
v2=c(1,2,1,7,2,7,8,3,4)


df=data.frame(v1,v2)

dist_mat=dist(df,method='euclidean')
dist_mat


Hierar_cl=hclust(dist_mat,method="complete")
Hierar_cl

plot(Hierar_cl)

abline(h=7.5,col="green")

fit=cutree(Hierar_cl,k=3)
fit
table(fit)
rect.hclust(Hierar_cl,k=3,border = "green")
(for single linkage)
#For Euclidean single linkage
rm(list=ls())
library(cluster)
library(dplyr)

v1=c(1,3,9,3,7,9,4,8,1)
v2=c(1,2,1,7,2,7,8,3,4)


df=data.frame(v1,v2)

dist_mat=dist(df,method='euclidean')
dist_mat


Hierar_cl=hclust(dist_mat,method="single")
Hierar_cl

plot(Hierar_cl)

abline(h=7.5,col="green")

fit=cutree(Hierar_cl,k=3)
fit
table(fit)
rect.hclust(Hierar_cl,k=3,border = "green")

Manhattan
(for complete linkage)
#For Manhattan Complete linkage
rm(list=ls())
library(cluster)
library(dplyr)

v1=c(1,3,9,3,7,9,4,8,1)
v2=c(1,2,1,7,2,7,8,3,4)


df=data.frame(v1,v2)

dist_mat=dist(df,method='manhattan')
dist_mat


Hierar_cl=hclust(dist_mat,method="complete")
Hierar_cl

plot(Hierar_cl)

abline(h=7.5,col="green")

fit=cutree(Hierar_cl,k=3)
fit
table(fit)
rect.hclust(Hierar_cl,k=3,border = "green")
(for single linkage)
#For manhattan single linkage
rm(list=ls())
library(cluster)
library(dplyr)

v1=c(1,3,9,3,7,9,4,8,1)
v2=c(1,2,1,7,2,7,8,3,4)


df=data.frame(v1,v2)

dist_mat=dist(df,method='manhattan')
dist_mat


Hierar_cl=hclust(dist_mat,method="single")
Hierar_cl

plot(Hierar_cl)

abline(h=7.5,col="green")

fit=cutree(Hierar_cl,k=3)
fit
table(fit)
rect.hclust(Hierar_cl,k=3,border = "green")