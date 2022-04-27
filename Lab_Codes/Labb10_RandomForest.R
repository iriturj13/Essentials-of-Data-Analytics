rm(list=ls())
#install.packages("randomForest")
library(stats)
library(dplyr)
library(randomForest)

df=iris

str(df)

set.seed(234)
seg_data=sample(2,nrow(df),replace = T,prob = c(0.8,0.2))
train=df[seg_data==1,]
test=df[seg_data==2,]

ran_for=randomForest(Species~.,data=train)

pred=predict(ran_for,test)

test$Species_pred=pred

# calculating accuracy

tbl=table(test$Species,test$Species_pred)
tbl

accuracy=sum(diag(tbl))/sum(tbl)
accuracy

