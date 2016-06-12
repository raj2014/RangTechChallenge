
rm(list=ls())
library(xgboost)
library(caret)
setwd("E:/Kaggle/RangTech_Challenge")

# custom metric
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  k<-which(preds>0.5)
  preds[k]<-1
  preds[-k]<-0
  err <- as.numeric(sum(labels == preds)/length(labels))
  return(list(metric = "error", value = err))
}

train<-read.csv(file="Train.csv",header=TRUE)
test<-read.csv(file="Test.csv",header=TRUE)




# train<-train[which(train$Cust_Tenure>=0),]
train[is.na(train)] <- -10
test[is.na(test)]<--10

#length(which(complete.cases(test)==TRUE))

# Getting response variable
active<-train$Active_Customer
trainID<-train$Cust_id
which(sapply(train,function(x) (class(x)))=="factor")

# Merge process train and test
train$Active_Customer<-NULL

testID<-test$Cust_id
test$Cust_id<-NULL
train$Cust_id<-NULL


trainrowCount<-nrow(train)
df<-rbind(train,test)


# function Definitions BNP
getOnes<-function(x)
{
  
  #print(x)
  return (length(which(x==-10)))
  
}

getZeros<-function(x)
{
  
  #print(x)
  return (length(which(x==0)))
  
}

# get the index of the column names which have promotion as name
prom_index<-grep("Promotion",colnames(df),value = FALSE)

#getsum function
getsum<-function(x)
{
  
  x<-x[x!=-10]
  sum(x)
}

getthresholdLimits<-function(x)
{
  x<-x[x>0.5]
  length(x)
}





df<-cbind(df,zerocounts=apply(df[,prom_index],1,getZeros))
df<-cbind(df,sum_prom=apply(df[,prom_index],1,getsum))
# df<-cbind(df,threshold_prom=apply(df[,prom_index],1,getthresholdLimits))



#df_matrix<-model.matrix(~.-1,data=df)
dummies<-dummyVars(~.,data=df)
df_matrix<-predict(dummies,newdata=df)
df_matrix<-cbind(df_matrix,negones=apply(df_matrix,1,getOnes))
df_matrix<-cbind(df_matrix,negcounts=apply(df_matrix,1,getZeros))


param <- list("objective" = "binary:logistic",
              "eval_metric" = evalerror,
              "booster" = "gbtree",
              "eta"=0.01,
              "max.depth"=5,
              "nthread" = 3,
              "min_child_weight"=1,
              "colsample_bytree"=0.9,
              "subsample"=0.9
)



# 10 fold cross validation repeat for 3 different seeds
seeds<-c(1,2,3)
avg_cv<-c()
for (i in seeds)
  
{
  set.seed(i)
  k<-xgb.cv(params=param,nrounds=780,data=df_matrix[c(1:trainrowCount),],nfold=10,
            label=active,
            feval=evalerror,
            #metrics={'error'},
            print.every.n = 30,
            verbose=TRUE,
            showsd=FALSE,
            maximize=FALSE)
  
  avg_cv<-append(avg_cv,unlist(k[nrow(k),"test.error.mean",with=FALSE]))
}
paste("AVG CV: ",mean(avg_cv),"SD CV : ",sd(avg_cv),"Median CV: ",median(avg_cv))





# Training and prediction
set.seed(1)

dtrain<-xgb.DMatrix(data=data.matrix(df_matrix[c(1:trainrowCount),]),label=active)
watchlist<-list(train=dtrain)

xgb<-xgb.train(   params              = param, 
                  data                = dtrain, 
                  nrounds             = 780, #1500, 
                  verbose             = TRUE,  #1
                  #early.stop.round    = 20,
                  #feval=evalerror,
                  print.every.n = 20,
                  watchlist           = watchlist,
                  maximize            = FALSE
)

xgbimp<-xgb.importance(colnames(df_matrix),model=xgb)

y_pred <- predict(xgb, data.matrix(df_matrix[-c(1:trainrowCount),]))


# Keeping  threshold as 0.5
k<-which(y_pred>=0.5)
y_pred[k]<-1
y_pred[-k]<-0


submission<-data.frame(Cust_id=testID,Active_Customer=y_pred)
write.csv(file="xgboost_v7.csv",submission,row.names = FALSE)