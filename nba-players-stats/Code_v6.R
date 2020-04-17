
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


#install libraries first
#Checking data type of Variables
Season_MVP<- Season_MVP %>% mutate(WS = replace(WS,is.na(WS),0),
                                   PPG = replace(PPG,is.na(PPG),mean(PPG,na.rm=T)),
                                   BPG = replace(BPG,is.na(BPG),mean(BPG,na.rm=T)),
                                   APG = replace(APG,is.na(APG),mean(APG,na.rm=T)),
                                   RPG=  replace(RPG,is.na(RPG),mean(RPG,na.rm=T)),
                                   ThreepointersPG = replace(ThreepointersPG,is.na(ThreepointersPG),mean(ThreepointersPG,na.rm=T)),
                                   TwopintersPG = replace(TwopintersPG,is.na(TwopintersPG),mean(TwopintersPG, na.rm = T)),
                                   FGPG = replace(FGPG,is.na(FGPG),mean(FGPG, na.rm = T)),
                                   SteelPG = replace(SteelPG,is.na(SteelPG),mean(SteelPG,na.rm=T)))

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
                      1, 0))

table(stats_merged_1$MVP)

df<-stats_merged_1[,-c(1,2,12)]
df$MVP<-as.factor(df$MVP)
summary(df)

#########################Alogrithm applied on merged data set which is higly imbalanced.

library(ROCR)
library(caTools)
library(DMwR)
install.packages("DMwR")


#############################################
set.seed(1123)

split <- sample.split(df$MVP, SplitRatio = 0.75)

dresstrain <- subset(df, split == TRUE)
dresstest <- subset(df, split == FALSE)


## Let's check the count of unique value in the target variable
as.data.frame(table(dresstrain$MVP))

##########################################################

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(MVP ~., dresstrain, perc.over = 2500, k = 5, perc.under = 800)

as.data.frame(table(balanced.data$MVP))
############################
model <- glm (MVP~., data=balanced.data, family = binomial)
summary(model)

################NOW testing in validation data set
## Predict the Values
predict <- predict(model, dresstest, type = 'response')

## Create Confusion Matrix
table(dresstest$MVP, predict > 0.8)
#####################################
#ROC Curve install ROCR package manually for this to RUN

ROCRpred <- prediction(predict, dresstest$MVP)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

###################################
#Predicting values for the User.

prediction_function<-function(m,WS,PPG,BPG,APG,RPG,ThreepointersPG,
                              TwopintersPG,FGPG,SteelPG)
{
  call_param<-predict(m,data.frame(WS=WS,PPG=PPG,BPG=BPG,APG=APG,RPG=RPG,
                             ThreepointersPG=ThreepointersPG,
                             TwopintersPG=TwopintersPG,
                             FGPG=FGPG,SteelPG=SteelPG))
  mvp_value<-ifelse(call_param>0.5,1,0)
  msg<-paste("MVP ==>",format(mvp_value),sep = "")
  print(msg)
}
prediction_function(model,20.6	,23	,0.0000000	,0	,9.9113924	,0.00000000	,10.6329114	,10.6329114	,0.0000000)

#########################################################
#NOW APPLYING RANDOM FOREST MODEL to the same data set

#tocheck if it works better
install.packages("randomForest")
library(randomForest)  
library(e1071)  
#############################
###model named rf is created below
rf = randomForest(MVP~.,  
                  ntree = 100,
                  data = balanced.data)

plot(rf) 

#to check the importance of each attribute
varImp(rf)

#plot the importance
varImpPlot(rf,  
           sort = T,
           n.var=25,
           main="Variable Importance")
##############################################

#now using the rf model to predict
predicted.response <- predict(rf, dresstest)

#use this to compare modesl.
confusionMatrix(data=predicted.response,  
                reference=dresstest$MVP)
##############

####now prediction using Random forest

Mvp_pred_func<-function(m,WS,PPG,BPG,APG,RPG,ThreepointersPG,
                              TwopintersPG,FGPG,SteelPG)
{
  call_param_2<-predict(m,data.frame(WS=WS,PPG=PPG,BPG=BPG,APG=APG,RPG=RPG,
                                   ThreepointersPG=ThreepointersPG,
                                   TwopintersPG=TwopintersPG,
                                   FGPG=FGPG,SteelPG=SteelPG))

  msg_2<-paste("MVP ==>",format(call_param_2),sep = "")
  print(msg_2)
}
Mvp_pred_func(rf,21.2,	34.975610,	1.5975610,	5.9146341,
              5.4756098,	0.08536585	,12.9512195,	13.0365854,	3.1585366)

########################################################################

#hence from the confusion matrix we can say that Logic regression glm is 
#more suitable for our data set.