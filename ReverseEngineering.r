# Reverse engineering

k<-read.csv(file="xgboost_v6.csv",header=TRUE)
l<-read.csv(file="xgboost_v7.csv",header=TRUE)

table(k$Active_Customer,l$Active_Customer)