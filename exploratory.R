setwd("E:/Kaggle/RangTech_Challenge")

train<-read.csv(file="Train.csv",header=TRUE)
test<-read.csv(file="Test.csv",header=TRUE)


# Exploratory

str(train)
colnames(train)

prop.table(table(train$Active_Customer))
table(train$Active_Customer)


# Check NA columns
names(which(sapply(train, function(x)(any(is.na(x))))==TRUE))


naframe<-data.frame(colnames(train),sapply(train, function(x) (length(which(is.na(x)==TRUE)))))
natestframe<-data.frame(colnames(test),sapply(test, function(x) (length(which(is.na(x)==TRUE)))))


# plots to understand the distribution


train[is.na(train)] <- -10
test[is.na(test)]<--10


# Getting response variable
active<-train$Active_Customer
which(sapply(df,function(x) (class(x)))=="factor")

# Merge process train and test
train$Active_Customer<-NULL

testID<-test$Cust_id
test$Cust_id<-NULL
train$Cust_id<-NULL


colnames(df)
summary(df[,c(3:43)])

k<-as.integer(df$Cust_status)

factColumns<-which(sapply(df,function(x) (class(x)))=="factor")

for ( i in factColumns)
{
  df[,i]<-as.integer(df[,i])
}



# Important consideration for two way

df$sub1<-df$Trans10-df$Trans11

df$sub2<-df$Trans8-df$Trans11

df$sub3<-df$Trans10-df$Trans8



# twoway features
m<-combn(c(3,4,5,6),2)

twoway_train<-matrix(0,nrow=nrow(df))

for (i in 1:ncol(m))
{
 
  pair<- m[,i]
  k<-unlist(df[,pair[1]])-unlist(df[,pair[2]])
  twoway_train<-cbind(twoway_train,k)
}
colnames(twoway_train)<-paste("k_",c(1:ncol(twoway_train)),sep="")

df<-cbind(df,twoway_train[,-c(1)])



# Variable importances

# Feature engineering on top of it(Cluster likewise features)

xgbimp<-xgb.importance(colnames(df_matrix),model=xgb)




# Extracting the ID information
library(stringr)

trainID<-strtoi(str_sub(trainID,start=2,end=-1))
trainID<-ifelse(trainID%%2==0,0,1)
testinfo<-ifelse(strtoi(str_sub(testID,start=2,end=-1))%%2==0,0,1)
#table(trainID,as.factor(train$Active_Customer))



df_matrix<-cbind(df_matrix,idinfo=c(trainID,testinfo))



# identifying the duplicate columns
# Removing duplicate columns
correlatedColumns<-c()
for (i in 1:(ncol(df_matrix)-1))
{
  for (j in (i+1):(ncol(df_matrix)))
  {
    
    if(abs(cor(df_matrix[,i],df_matrix[,j]))>0.99)
    {
      
      correlatedColumns<-c(correlatedColumns,i)
      print(paste(colnames(df_matrix)[c(i,j)]))
      
    }
    
  }
  
}

correlatedColumns<-unique(correlatedColumns)
correlatedColumns


#scaling for df_matrix

for (i in 1:ncol(df_matrix))
{
  df_matrix[,i]<-scale(df_matrix[,i],scale=TRUE,center=TRUE)
}


# get the index of the column names which have promotion as name
prom_index<-grep("Promotion",colnames(df),value = FALSE)

for (i in prom_index)
{
  if(min(df[,i])==0 & max(df[,i]<=1))
  {
    print(colnames(df)[i])
    print (summary(df[,i]))
    
  }
  else
  {
    #print(colnames[i])
    #summary(df[,i])
  }
}

df<-cbind(df,zerocounts=apply(df[,prom_index],1,getZeros))
df<-cbind(df,sum_prom=apply(df[,prom_index],1,sum))


# doing the same for Food columns
food_index<-grep("Food",colnames(df),value = FALSE)
df<-cbind(df,zerofoodcounts=apply(df[,food_index],1,getZeros))
food_log<-scale(df[,food_index],scale=TRUE,center=TRUE)

df<-cbind(df,sum_food=apply(food_log,1,sum))


# Clustering for food columns
library(lsa)
food_index<-grep("Food",colnames(df),value = FALSE)
food_log<-scale(df[,food_index],scale=TRUE,center=TRUE)
food_cor<-cosine(as.matrix(food_log))
rownames(food_cor)<-food_index
food_cor<-1-food_cor
set.seed(1)
foodCluster<-kmeans(food_cor,centers=20, iter.max = 1000, nstart = 100,algorithm = c("Lloyd"), trace=TRUE)
clusterLink<-data.frame(col_index=strtoi(rownames(food_cor)),foodCluster$cluster)

food_matrix=matrix(0,nrow=nrow(df),ncol=20)

getsum<-function(x)
{
  
   x<-x[x!=-10]
   sum(x)
}

for (i in 1:20)
{
  v<-clusterLink[clusterLink$foodCluster.cluster==i,"col_index"]
  if(length(v)>0)
  {
    food_matrix[,i]<-apply(df[,v],1,getsum)
  }
}

df_matrix<-cbind(df_matrix,food_matrix)
