#Code for the Business Proposal regarding the Prediction of the MVP in NBA based on the various parameters

##Libraries required
library("stringr")
library("caret")
library("caTools")
library("DMwR")
library("doParallel")
library("dplyr")
library("e1071")
library("foreach")
library("GGally")
library("ggplot2")
library("gplots")
library("iterators")
library("lattice")
library("mlr")
library("ParamHelpers")
library("plyr")
library("pROC")
library("randomForest")
library("ROCR")
library("rpart.plot")
library("scales")
library("stringi")
library("tidyr")
library("tidyselect")
library("tibble")
library("unbalanced")
##################################################
#Importing the data based on 2 different sources
#1.This contains the historical NBA data for each player
season_stat<- read.csv("Seasons_Stats.csv")

#2.This contains the data of MVP players each year since the award originated.
mvpdata<- read.csv("MVP_ESPN_Data.csv")


################Cleaning and updating the data as per our requirements,
#so that it can be used to train the model.
#Removing Serial number

season_stat<-season_stat[,-c(1)]
#changing the data to per game basis
season_stat$PPG<-season_stat$PTS/season_stat$G
season_stat$BPG<- season_stat$BLK/season_stat$G
season_stat$APG<- season_stat$AST/season_stat$G
season_stat$RPG<- season_stat$TRB/season_stat$G
season_stat$ThreepointersPG<- season_stat$X3P/season_stat$G
season_stat$TwopintersPG<-season_stat$X2P/season_stat$G
season_stat$FGPG<- season_stat$FG/season_stat$G
season_stat$SteelPG<-season_stat$STL/season_stat$G

#analyzing data to know the right features for our model

hist(season_stat$Age,col = "BLUE",xlab = "Player Age",main = "Histogram")
hist(season_stat$G,col = "green",xlab = "Games played",main = "Histogram")
hist(season_stat$TOV,col = "Red",xlab = "TurnOver points",main = "Histogram")
hist(season_stat$PF,col = "yellow",xlab = "Personal fouls",main = "Histogram")


#correlation table
ggcorr(season_stat)


#keeping the important attributes only
Season_MVP<- season_stat[,-c(3:23)]
Season_MVP<- Season_MVP[,-c(4:31)]

#changing Column names in the MVP tables
names(Season_MVP)[1]<-paste("YEAR")
names(Season_MVP)[2]<- paste("PLAYER")



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


Season_MVP$PLAYER<-str_remove_all(Season_MVP$PLAYER,"[*]")
mvpdata<-mvpdata[,-c(3:9)]
names(mvpdata)[1]<-paste("YEAR")
names(mvpdata)[2]<-paste("PLAYER")


##########################################
#merging data frames to create one merged data set.
stats_merged<-merge(Season_MVP,mvpdata,by.x = "YEAR", by.y="YEAR", incomparables = NA)
#changing df name so dont have to load the data again and again
stats_merged_1<-stats_merged
#to add a MVP (1/0) column in the merged data set 
stats_merged_1 <- stats_merged_1 %>%
  mutate(MVP = ifelse(as.character(stats_merged_1$PLAYER.x)==
                        as.character(stats_merged_1$PLAYER.y),
                      1, 0))

table(stats_merged_1$MVP)

df<-stats_merged_1[,-c(1,2,12)]
df$MVP<-as.factor(df$MVP)
summary(df)

###few charts for df comparisions of attributes
#to give the idea about the data set
#Correlation using ggplots
ggcorr(df)

# Box plots for all Attributes

boxplot(df, horizontal=TRUE, main="Data frame attributes comparision")



# Density plot
par(mfrow=c(3, 3))
colnames <- dimnames(df)[[2]]
for (i in 1:10) {
  d <- density(df[,i])
  plot(d, type="n", main=colnames[i])
  polygon(d, col="green", border="gray")
}

#df is the final data set on which we will work data set which is higly imbalanced.

#############################################
#dividing data in test and train for the the model and prediction.

#setting seed
set.seed(1123)
#splitting data in 75% into training set and 25% into Test Data set 
split <- sample.split(df$MVP, SplitRatio = 0.75)
#train data
TrainMVP_Data <- subset(df, split == TRUE)
#test data
TestMVP_Data <- subset(df, split == FALSE)


## check the count of unique value in the MVP column
as.data.frame(table(TrainMVP_Data$MVP))

##########################################################
#BALANCING DATA USING Synthetic Minority Oversampling Technique

set.seed(75252)
balanced.data <- SMOTE(MVP ~., TrainMVP_Data, perc.over = 1000, k = 5, perc.under = 2600)

#This gives us the frequency of MVP data in the balanced data set.
as.data.frame(table(balanced.data$MVP))


#________________________________________________________________________________________________
#model Generation 
#now using random forest to implement model (we use comparision techniques to find the best model)

rf = randomForest(MVP~.,  
                  ntree = 100,
                  data = balanced.data)

#####
#to plot the importance of each variable use
plot(rf) 

#to check the importance of each attribute
varImp(rf)

#plot the importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Important Variables  By Order")

##############################################

#now using the rf model to predict
predicted.response <- predict(rf, TestMVP_Data)

#use this to compare models.
confusionMatrix(data=predicted.response,  
                reference=TestMVP_Data$MVP)

######prediction on Training data to check confusion matrix.

pred.resp<-predict(rf,TrainMVP_Data)
confusionMatrix(data = pred.resp,reference = TrainMVP_Data$MVP)

#################################


#Last Step to create a Function to use the above model to predict value based on paramenters.

Predict_MVP_func<-function(m,WS,PPG,BPG,APG,RPG,ThreepointersPG,
                        TwopintersPG,FGPG,SteelPG)
{
  call_param_2<-predict(m,data.frame(WS=WS,PPG=PPG,BPG=BPG,APG=APG,RPG=RPG,
                                     ThreepointersPG=ThreepointersPG,
                                     TwopintersPG=TwopintersPG,
                                     FGPG=FGPG,SteelPG=SteelPG))
  #print the message
  message<-paste(ifelse(call_param_2=='1',"Player has the very high Chances of becoming an MVP",
                      "Player has High chances of NOT becoming an MVP"))
  print(message)
}

#Passing the values in fucntion to check the player's chances.
#Prediction 1
Predict_MVP_func(rf,21.2,	0,	1.59,	0,
              0,	0.08	,12.9,	13.03,	3.1)

#Prediction 2
Predict_MVP_func(rf,11.3,16.5,	0.4,	2.9,	
                 22.67,	0.37,	6.6,	6.6,	0.67)
#prediction 3
Predict_MVP_func(rf,10,12,	0.40,	1.8,	
                 23,	0.3,	6.608,	8.3,	0.78)
#prediction 4
Predict_MVP_func(rf,18,	0,	0,	0,
                 0,	0.08	,12.9,	13.0,	3.15)


###############End of Project
