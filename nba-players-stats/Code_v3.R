season_stat<- read.csv("Seasons_Stats.csv")
mvpdata<- read.csv("MVP_ESPN_Data.csv")

#Removing Serial number

season_stat<-season_stat[,-c(1)]

season_stat$PPG<-season_stat$PTS/season_stat$G
season_stat$BPG<- season_stat$BLK/season_stat$G
season_stat$APG<- season_stat$AST/season_stat$G
season_stat$RPG<- season_stat$TRB/season_stat$G
season_stat$ThreepointersPG<- season_stat$X3P/season_stat$G
season_stat$TwopintersPG<-season_stat$X2P/season_stat$G
season_stat$FGPG<- season_stat$FG/season_stat$G
season_stat$SteelPG<-season_stat$STL/season_stat$G
Season_MVP<- season_stat[,-c(3:23)]
Season_MVP<- Season_MVP[,-c(4:31)]

names(Season_MVP)[1]<-paste("YEAR")
names(Season_MVP)[2]<- paste("PLAYER")


nrow<- nrow(organics_data)
#install libraries first
#Checking data type of Variables
Season_MVP<- Season_MVP %>% mutate(WS = replace(WS,is.na(WS),0),
                                   PPG = replace(PPG,is.na(PPG),mean(PPG,na.rm=T)),
                                   BPG = replace(BPG,is.na(BPG),0),
                                   APG = replace(APG,is.na(APG),0),
                                   RPG=  replace(RPG,is.na(RPG),0),
                                   ThreepointersPG = replace(ThreepointersPG,is.na(ThreepointersPG),0),
                                   TwopintersPG = replace(TwopintersPG,is.na(TwopintersPG),mean(TwopintersPG, na.rm = T)),
                                   FGPG = replace(FGPG,is.na(FGPG),mean(FGPG, na.rm = T)),
                                   SteelPG = replace(SteelPG,is.na(SteelPG),0))

Season_MVP<- Season_MVP[!(is.na(Season_MVP$YEAR)),]

install.packages("stringr")               # Install stringr package
library("stringr")  
Season_MVP$PLAYER<-str_remove_all(Season_MVP$PLAYER,"[*]")
mvpdata<-mvpdata[,-c(3:9)]
names(mvpdata)[1]<-paste("YEAR")
names(mvpdata)[2]<-paste("PLAYER")


##########################################
#Install a lot of libraries:tidyr,tidyselect,stringi,stringr,scales,plyr,lattice,ggplot2,ggally,dplyr,e1071,caret
#starting  code: By Abhishek
#merging data frames to create one merged data set.

stats_merged<-merge(Season_MVP,mvpdata,by.x = "YEAR", by.y="YEAR", incomparables = NA)
#############
#changing df name so dont have to load the data again and again
stats_merged_1<-stats_merged
#######
#to add a MVP (yes/no) column in the merged data set 
stats_merged_1 <- stats_merged_1 %>%
  mutate(MVP = ifelse(as.character(stats_merged_1$PLAYER.x)==
                        as.character(stats_merged_1$PLAYER.y),
                      "yes", "no"))

table(stats_merged_1$MVP)
summary(stats_merged_1)
###########################################
#now removing the final values like names and year to create prediction df 
df<-stats_merged_1[,-c(1,2,12)]
################
#now creating training Data set
tran.index_1<-sample(c(1:dim(df)[1]), dim(df)[1]*0.5)
t.d<-df[tran.index_1,]
v.d<-df[-tran.index_1,]
######################################
#Creating model to predict

#using glm

set.seed(1)
summary(df)
prime_model<-train(MVP ~WS+PPG+BPG+APG+RPG+ThreepointersPG+TwopintersPG+FGPG+SteelPG,
                   data = t.d,method="glm",preProcess=c('scale','center'))
pred<-predict(prime_model,t.d)
table(pred)
# confusionMatrix(pred_12,as.factor(train.dataset_1$MVP))
pred_valid<-predict(prime_model,v.d)
table(pred_valid)
# confusionMatrix(pred12_validate,as.factor(validation.dataset_1$MVP))
str(t.d)

############################

#now creating a Function to Input parameters for the Model and Predict the MVP or not using the model glm
summary(df)

#applying model prediction to complete data set after training and validation to predict
pred_complete<-predict(prime_model,df)

#summary to check the prediction of yes and no on complete dataset
summary(pred_complete)

#now creating function
prediction_function<-function(m,WS,PPG,BPG,APG,RPG,ThreepointersPG,TwopintersPG,FGPG,SteelPG)
{
  new1<-predict(m,data.frame(WS=WS,PPG=PPG,BPG=BPG,APG=APG,RPG=RPG,ThreepointersPG=ThreepointersPG,TwopintersPG=TwopintersPG,FGPG=FGPG,SteelPG=SteelPG))
  msg<-paste("WS=",WS,"PPG=",PPG,"BPG=",BPG,"APG=",APG,"RPG=",RPG,"ThreepointersPG=",ThreepointersPG,"TwopintersPG=",
             TwopintersPG,"FGPG=",FGPG,"SteelPG=",SteelPG,"MVP==>",format(new1),sep ="")
  print(msg)
}
prediction_function(prime_model,8,	10,1.5,	0,	0,
                    0.3,	2.3,	0.2,	0)
str(df)
