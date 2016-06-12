rm(list=ls())
library(ggplot2)
library(ggrepel)
setwd("E:/Kaggle/RangTech_Challenge")

# Total number of records in the test set
num_records<-11042


# Reading public leaderboard dataset
data_public<-read.csv(file="Visualizations/publiclb.csv",header=FALSE)
# Total number of properly classified records in public lb
data_public$correct_records<-data_public$V3*0.3*num_records
colnames(data_public)<-c("Rank_public","TeamName","Accuracy","Timestamp","University","NumberOfSubmissions","correct_records")


# Reading private leaderboard dataset
data_private<-read.csv(file="Visualizations/privatelb.csv",header=FALSE)
# Total number of properly classified records in private lb
data_private$correct_records<-data_private$V4*num_records
colnames(data_private)<-c("Rank_private","TeamName","University","Accuracy","Timestamp","correct_records")


# Merging public and private LB
merge_df<-merge(data_private,data_public,by="TeamName")
merge_df$Rank_Climb<-merge_df$Rank_public-merge_df$Rank_private



# Private LeaderBoard Visualization
# data_private[data_private$Rank_private<=21,]
ggplot(data=data_private[data_private$Rank_private<=21,], aes(x= Rank_private, y= correct_records, colour=TeamName, label=TeamName))+
  geom_point()+
  ylim(c(7500,8000))+
  xlab("Rank_PrivateLeaderBoard")+
  ylab("Number of Correctly Classified Records")+
  geom_label_repel()+
  ggtitle("PrivateLeaderBoard Visualization- Top 20%")+
  theme(plot.title=element_text(face="bold",colour="deepskyblue",size=20))




# Rank shift from public to private LB
ggplot(data=merge_df[merge_df$Rank_private<=21,], aes(x= Rank_private, y= correct_records.x,
                                                      size=Rank_Climb,label=TeamName,color=TeamName))+
  geom_point()+
  ylim(c(7500,8000))+
  xlab("Rank_PrivateLeaderBoard")+
  ylab("Number of Correctly Classified Records")+
  geom_label_repel()+
  ggtitle("PrivateLeaderBoard Shift Visualization- Top 20%")+
  theme(plot.title=element_text(face="bold",colour="deepskyblue",size=20))


# Impact of submissions on public LB
ggplot(data=merge_df[merge_df$Rank_private<=21,], aes(x= Rank_private, y= correct_records.x,colour=TeamName,
                                                      size=NumberOfSubmissions,label=TeamName))+
  geom_point()+
  ylim(c(7500,8000))+
  xlab("Rank_PrivateLeaderBoard")+
  ylab("Number of Correctly Classified Records")+
  geom_label_repel()+
  ggtitle("Impact of Submissions on PrivateLeaderBoard Rank- Top 20%")+
  theme(plot.title=element_text(face="bold",colour="deepskyblue",size=20))

rm(data_top21)


data_top10<-merge_df[merge_df$Rank_private<=10,]
data_top10<-data_top10[order(data_top10$Rank_private),]
data_top10$TeamMembers<-c(3,2,1,2,2,1,1,1,3,1)

ggplot(data=data_top10, aes(x= Rank_private, y= correct_records.x,colour=TeamName,
                                                      size=TeamMembers,label=TeamName))+
  geom_point()+
  ylim(c(7500,8000))+
  xlab("Rank_PrivateLeaderBoard")+
  ylab("Number of Correctly Classified Records")+
  geom_label_repel()+
  ggtitle("Impact of TeamSize on PrivateLeaderBoard Rank- Top 10")+
  theme(plot.title=element_text(face="bold",colour="deepskyblue",size=20))


# Toppers Composition : University Representation in Top20%
winners<-read.csv(file="Visualizations/winners",header=FALSE)
winners<-as.data.frame(table(winners$V3))

library(plotrix)
Frequency <- winners$Freq
University <- winners$Var1
pie3D(Frequency,labels=University,explode=0.05,
      main="Composition of Universities in Top 20% ")



winners<-read.csv(file="Visualizations/winners.csv",header=FALSE)
ggplot(winners, aes(x = factor(1), fill = V3,label=V3)) +
  xlab("")+
  ylab("Count")+
  geom_bar(width = 5)+coord_polar(theta = "y")+
  ggtitle("University Composition in Top 20% ")+
  theme(plot.title=element_text(face="bold",colour="deepskyblue",size=20))

