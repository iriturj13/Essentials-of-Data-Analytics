rm(list=ls())

#install.packages('gradDescent')
#install.packages('calculus')
#install.packages('plot3D')
#install.packages('plot3Drgl')

library(gradDescent)
library(calculus)
library(dplyr)
library(plot3Drgl)

fn=expression(4*x1^2-3*x1*x2+2.5*x2^2-5.5*x1-4*x2)
dfx=deriv(fn,c("x1","x2"))
sc=6
alpha=0.135
beta=0.15

fv=list()
dvx1=list()
dvx2=list()


k=1

#initial guess
x1=1.5
x2=1.75

#initial velocity
v1=0
v2=0

fv=as.numeric(eval(fn))

while (sc>=0.005) {
  dv=eval(dfx)
  dvx1[k]=.grad[,"x1"]
  dvx2[k]=.grad[,"x2"]
  
  v1=(beta*v1) + (alpha*as.numeric(dvx1[k]))
  v2=(beta*v2) + (alpha*as.numeric(dvx2[k]))
  
  x1=x1-v1
  x2=x2-v2
  
  fv[k-1]=eval(fn)
  
  sc=abs(as.numeric(fv[k]) - as.numeric(fv[k-1]))
  
  k=k-1
}
