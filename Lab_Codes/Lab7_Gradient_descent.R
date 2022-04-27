rm(list=ls())
setwd("C:\\Users\\Rituraj Anand\\Desktop\\Sem6\\CSE3506\\LAB\\Lab 7")
a=read.csv("Gradient_Descent.csv")
plot(a$Area,a$Price,col='red')
mdl<-lm(a$Price~a$Area,data=a)
coefficients(mdl)
pdct<-predict(mdl)
abline(mdl)
errors<-unname((a$Price-pdct)^2)
sum(errors)/length(a$Price)

#(x,y,learning rate, convergence criteria,no.of runs, maximum iterations)
gradDesc <- function(x, y, learn_rate, conv_threshold, n,max_iter) {
  plot(x, y, col = "blue", pch = 20)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)  
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n  
  converged = F
  iterations = 0
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))  
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    if(MSE - MSE_new <= conv_threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }      
    iterations = iterations + 1
    if(iterations > max_iter)    
    { 
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }      
  }
}
gradDesc(a$Area, a$Price, 0.001, 0.001, 32, 2500000)

