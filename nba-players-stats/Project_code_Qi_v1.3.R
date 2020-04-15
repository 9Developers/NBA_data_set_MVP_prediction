#BA WITH R
#GROUP 8 PROJECT WORK


#ACTION PLAN
#VIEW and summarise data
#Clean data
#Merge Data(try to create MVP column based on MVP table where if player is MVP then 1 else 0)
#Find correlation between data
#create a model to predict MVP of the Season based on stats.

season_data<-read.csv("Seasons_Stats.csv")
MVPTable<-read.csv("MVP_ESPN_Data.csv")

#Removing columns that are not useful for prediction

season_data<-season_data[,-1]
str(season_data)
summary(season_data)

str(MVPTable)
summary(MVPTable)

#######
#clean data

##############################################

#we will replace every value of NA with Mean except Year as we will not use the data where Year is NA.
####we are using mutate function so we dont need to seperate numeric data and everything

#player,pos,Tm,
table(season_data$Player)
table(season_data$Pos)
(max(table(season_data$Tm)))
table(is.na(season_data$Player))##no missing values in player
table(is.na(season_data$Pos))#no missing values in pos
table(is.na(season_data$Tm))#no missing values in Tm

#so just need to mutate numerical data other than year
table(is.na(season_data$Year))
#67 missing values in year need to be removed from the data

season_data_clean<-season_data %>% mutate(GS = replace(GS,is.na(GS),mean(GS, na.rm = T)),
                                      G = replace(G,is.na(G),mean(G, na.rm = T)),
                                      MP = replace(MP,is.na(MP),mean(MP, na.rm = T)),
                                      PER = replace(PER,is.na(PER),mean(PER, na.rm = T)),
                                      TS. = replace(TS.,is.na(TS.),mean(TS., na.rm = T)),
                                      X3PAr = replace(X3PAr,is.na(X3PAr),mean(X3PAr, na.rm = T)),
                                      FTr = replace(FTr,is.na(FTr),mean(FTr, na.rm = T)),
                                      ORB. = replace(ORB.,is.na(ORB.),mean(ORB., na.rm = T)),
                                      DRB. = replace(DRB.,is.na(DRB.),mean(DRB., na.rm = T)),
                                      TRB. = replace(TRB.,is.na(TRB.),mean(TRB., na.rm = T)),
                                      AST. = replace(AST.,is.na(AST.),mean(AST., na.rm = T)),
                                      STL. = replace(STL.,is.na(STL.),mean(STL., na.rm = T)),
                                      BLK. = replace(BLK.,is.na(BLK.),mean(BLK., na.rm = T)),
                                      TOV. = replace(TOV.,is.na(TOV.),mean(TOV., na.rm = T)),
                                      USG. = replace(USG.,is.na(USG.),mean(USG., na.rm = T)),
                                      OWS = replace(OWS,is.na(OWS),mean(OWS, na.rm = T)),
                                      DWS = replace(DWS,is.na(DWS),mean(DWS, na.rm = T)),
                                      WS = replace(WS,is.na(WS),mean(WS, na.rm = T)),
                                      WS.48 = replace(WS.48,is.na(WS.48),mean(WS.48, na.rm = T)),
                                      OBPM = replace(OBPM,is.na(OBPM),mean(OBPM, na.rm = T)),
                                      DBPM = replace(DBPM,is.na(DBPM),mean(DBPM, na.rm = T)),
                                      BPM = replace(BPM,is.na(BPM),mean(BPM, na.rm = T)),
                                      VORP = replace(VORP,is.na(VORP),mean(VORP, na.rm = T)),
                                      FG = replace(FG,is.na(FG),mean(FG, na.rm = T)),
                                      FGA = replace(FGA,is.na(FGA),mean(FGA, na.rm = T)),
                                      FG. = replace(FG.,is.na(FG.),mean(FG., na.rm = T)),
                                      X3P = replace(X3P,is.na(X3P),mean(X3P, na.rm = T)),
                                      X3PA = replace(X3PA,is.na(X3PA),mean(X3PA, na.rm = T)),
                                      X3P. = replace(X3P.,is.na(X3P.),mean(X3P., na.rm = T)),
                                      X2P = replace(X2P,is.na(X2P),mean(X2P, na.rm = T)),
                                      X2PA = replace(X2PA,is.na(X2PA),mean(X2PA, na.rm = T)),
                                      X2P. = replace(X2P.,is.na(X2P.),mean(X2P., na.rm = T)),
                                      eFG. = replace(eFG.,is.na(eFG.),mean(eFG., na.rm = T)),
                                      FT = replace(FT,is.na(FT),mean(FT, na.rm = T)),
                                      FTA = replace(FTA,is.na(FTA),mean(FTA, na.rm = T)),
                                      FT. = replace(FT.,is.na(FT.),mean(FT., na.rm = T)),
                                      ORB = replace(ORB,is.na(ORB),mean(ORB, na.rm = T)),
                                      DRB = replace(DRB,is.na(DRB),mean(DRB, na.rm = T)),
                                      TRB = replace(TRB,is.na(TRB),mean(TRB, na.rm = T)),
                                      AST = replace(AST,is.na(AST),mean(AST, na.rm = T)),
                                      STL = replace(STL,is.na(STL),mean(STL, na.rm = T)),
                                      BLK = replace(BLK,is.na(BLK),mean(BLK, na.rm = T)),
                                      TOV = replace(TOV,is.na(TOV),mean(TOV, na.rm = T)),
                                      PF = replace(PF,is.na(PF),mean(PF, na.rm = T)),
                                      PTS = replace(PTS,is.na(PTS),mean(PTS, na.rm = T)))


#removing blank columns from the data set
season_data_clean<-season_data_clean[,-c(21,26)]

#removing rows data for missing year
season_data_clean<-season_data_clean[!(is.na(season_data_clean$Year)),]

table(is.na(season_data_clean$Year))
#################################################
#data cleaning is done here.
######################################
#final data frame is:
#season_data_clean

#second step of the project is to merge the data set with MVP values
#######Begin
season_stats<-season_data_clean

####################################################################
####Player position popularity in history/decades-Visulization (Qi)
####################################################################

##library(ggplot2)

##Pos_summary <- as.data.frame(table(season_stats$Pos))
##colnames(Pos_summary) <- c("Position","Count")
##ggplot(data = Pos_summary, aes(x=Position,y=Count)) + 
##  geom_histogram(bin=15,stat='identity',color='grey',size = 3)

##season_stats$Decade <- cut(season_stats$Year, breaks = c(1949,1959,1969,1979,1989,1999,2009,2019),labels = c("50s","60s","70s","80s","90s","00s","10s"))

##Pos_decade_summary <- as.data.frame(table(season_stats$Decade,season_stats$Pos))
##colnames(Pos_decade_summary) <- c("Decade","Position","Count")

##ggplot(data = Pos_decade_summary, aes(x=Position,y=Count)) + geom_histogram(bin=15,stat='identity',color='grey',size = 3)+ facet_wrap(~Decade)
###result: no apparent tendecy of single position on court over decades

######################################################
###Visualization for average 3PT attempt made per player over decades
######################################################

##X3PA_50s <- mean(season_data$X3PA[(season_data$Year-1959 < 0) & season_data$Year-1950 >=0 ])  
##X3PA_60s <- mean(season_data$X3PA[(season_stats$Year-1969 < 0) & season_stats$Year-1959 >=0 ])
##X3PA_70s <- mean(season_stats$X3PA[(season_stats$Year-1979 < 0) & season_stats$Year-1969 >=0 ])
##X3PA_80s <- mean(season_stats$X3PA[(season_stats$Year-1989 < 0) & season_stats$Year-1979 >=0 ])
##X3PA_90s <- mean(season_stats$X3PA[(season_stats$Year-1999 < 0) & season_stats$Year-1989 >=0 ])
##X3PA_00s <- mean(season_stats$X3PA[(season_stats$Year-2009 < 0) & season_stats$Year-1999 >=0 ])
##X3PA_10s <- mean(season_stats$X3PA[(season_stats$Year-2017 < 0) & season_stats$Year-2009 >=0 ])

##X3PA_decade_summary <- c(X3PA_50s,X3PA_60s, X3PA_70s, X3PA_80s, X3PA_90s, X3PA_00s,X3PA_10s)
##names(X3PA_decade_summary) <- c("X3PA_50s","X3PA_60s", "X3PA_70s", "X3PA_80s", "X3PA_90s", "X3PA_00s","X3PA_10s")
##barplot(X3PA_decade_summary, xlab= "Decade", ylab = "Count")

####################################################################
####Visulization code end
####################################################################

#if no merger than we can add a column with MVP values matching from MVP table
########################################################################
#trial code begins
##
# 
# season_stats_1<-season_stats
# season_stats_1$MVP<-ifelse(as.character(MVPTable$PLAYER)==as.character(season_stats_1$Player) &
#                              MVPTable$ï..YEAR==season_stats_1$Year,"Yes","No")
# table(ss2$MVP)
# ss2 <- season_stats_2 %>%
#   mutate(MVP = ifelse(as.character(MVPTable$PLAYER)==as.character(season_stats_2$Player) &
#                         MVPTable$ï..YEAR==season_stats_2$Year, 'Yes', 'No'))
# 
# table(ifelse(MVPTable$PLAYER==season_stats_2$Player,"Yes","No"))
# table(ifelse(MVPTable$ï..YEAR==season_stats_2$Year,"Yes","No"))
# levels(MVPTable$PLAYER)
# levels(season_stats_1$Player)

#trial code ends
########################################
#so here we can see we are not able to match players based on name as season stats contains star ahead
#of players name .we need to remove those stars from names.
###########
#to remove * from the name
season_stats_2<-season_stats
season_stats_2$Player<-str_remove_all(season_stats_2$Player,"[*]")
MVPTable$PLAYER<-as.character(MVPTable$PLAYER)
#jsut checking values and kept changing names of data set.
# str(MVPTable$PLAYER)
# str(season_stats_2$Player)
table(MVPTable$PLAYER==season_stats_2$Player)
#############################
#removing columns from MVP table and keeping name and year only.
##(Qi)
MVPTable_s<-MVPTable
MVPTable_s<-MVPTable_s[,-c(3:9)]
colnames(MVPTable_s)[1:2] <- c("MVP_Year","MVP_Player")

#remove year records beyond NBA_stats's coverage
MVPTable_s<-MVPTable_s[-c(1:(max(MVPTable_s$MVP_Year)-max(season_stats_2$Year))),]

#MVPTable_s$MVP_PLAYER<-MVPTable_s$PLAYER

###########
#merging data frames to create one merged data set.

stats_merged<-merge(season_stats_2,MVPTable_s,by.x = "Year", by.y="MVP_Year", incomparables = NA)

#check the MVP status of corresponding year
MVP_test <- stats_merged[,c(1:2,51)]

summary(season_stats_2)
stats_merged_1<-stats_merged
#######
#to add a MVP (yes/no) column in the merged data set 
stats_merged_1 <- stats_merged_1 %>%
  mutate(MVP = ifelse(as.character(stats_merged_1$Player)==
                        as.character(stats_merged_1$MVP_Player),
                      "yes", "no"))

table(stats_merged_1$MVP)
summary(stats_merged_1)
#we can see Age left above in the data cleaning process and is still showing NA
#so we change that
df<-stats_merged_1 %>% mutate(Age = replace(Age,is.na(Age),mean(Age, na.rm = T)))
summary(df)

########################################Merging and clenaing ends
###############################
#now we need to create a numeric data frame to check correlation
#install.packages("GGally")
library(GGally)
library(ggplot2)
?ggcorr
ggcorr(df)
#BAsed on the above correlation graph we can say below parameters are good.
#PER,MP,GS,G
#DRB-9,OWS-16,DWS-17,WS-18,Ws48-19
#BPM-22,FG-24,X2P-30,FTA-35,AST-40,TOV-43,PTS-45,PF-44

df$MVP<-as.factor(df$MVP)
summary(df)
freq<-count(df$MVP)
freq
######
barplot(height = freq$freq,name=freq$x,ylab="frequency")
#######

df.set<-df[,-c(1:5)]
df.set<-df.set[,-c(5:8,10:15,20,21,23,25:29,31:34,36:39,41,42,46)]

##########
#modeling
tran.index<-sample(c(1:dim(df.set)[1]), dim(df.set)[1]*0.5) #pick 50% of the data to train
train.dataset<-df.set[tran.index,]
validation.dataset<-df.set[-tran.index,]#

######################################
#Trial on different decision tree and logistic regression models (Qi)
######################################
library(rpart)
library(rpart.plot)
library(caret)
##decision tree
defalut.ct <- rpart(MVP ~ ., data = train.dataset, method = "class")
prp(defalut.ct)

default.ct.pred.train <- predict(default.ct,train.dataset,type = "class")
confusionMatrix(default.ct.pred.train,as.factor(train.dataset$MVP))

# custome tree
cust.ct <- rpart(MVP ~ ., data = train.dataset, method = "class", maxdepth = 3)
prp(cust.ct)

cust.ct.pred.train<-predict(cust.ct,train.dataset,type = "class")
confusionMatrix(cust.ct.pred.train,as.factor(train.dataset$MVP)) ##accuracy = 99.7%

cust.ct.pred.valid<-predict(cust.ct,validation.dataset,type = "class")
confusionMatrix(cust.ct.pred.valid,as.factor(validation.dataset$MVP)) ## accuracy = 99.6%

#deeper tree (redudant rule) model
deeper.ct <- rpart(MVP ~ ., data = train.dataset, method = "class", minsplit = 1, cp = 0)
prp(deeper.ct)

deeper.ct.pred.train<-predict(deeper.ct,train.dataset,type = "class")
confusionMatrix(deeper.ct.pred.train,as.factor(train.dataset$MVP)) ##accuracy = 1 (sure thing!)

deeper.ct.pred.valid<-predict(deeper.ct,validation.dataset,type = "class")
confusionMatrix(deeper.ct.pred.valid,as.factor(validation.dataset$MVP)) ## accuracy = 99.7% (good but too complex rules)

#logit model 
####convert MVP result "yes" to "1" and "no" to "0"
train.dataset$MVP <- ifelse(train.dataset$MVP=='yes', 1,0)
validation.dataset$MVP <- ifelse(validation.dataset$MVP=='yes', 1,0)

sum(is.na(train.dataset))

logit.reg <- glm(MVP ~., data = train.dataset, family = "binomial")
options(scipen=999)
summary(logit.reg)

### generate confusion matrix for training data
logit.reg.pred.train<-predict(logit.reg,train.dataset,type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred.train>0.7,1,0)),as.factor(train.dataset$MVP))

### repeat the code for the validation set
logit.reg.pred.valid<-predict(logit.reg,validation.dataset,type = "response")
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid>0.5,1,0)),as.factor(validation.dataset$MVP)) #Balanced Accuracy : 0.7162
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid>0.7,1,0)),as.factor(validation.dataset$MVP)) #Balanced Accuracy : 0.6332
confusionMatrix(as.factor(ifelse(logit.reg.pred.valid>0.9,1,0)),as.factor(validation.dataset$MVP)) #Balanced Accuracy : 0.5832

library(pROC)

r<-roc(train.dataset$MVP,logit.reg.pred.train)#it gives u probability based on different thresholds
plot.roc(r)
#######################
##Trial code end (Qi)
#######################


model_set<-train(MVP ~ .,data = train.dataset,method="glm")
model_set
model_set_2<-train(MVP ~ .,data = train.dataset,method="glm",preProcess=c('scale','center'))
model_set_2
prediction<-predict(model_set_2,validation.dataset)
table(prediction)

####working till here
##############
######################
########trying to print the prediction based on var
summary(df.set)
predict_func<-function(m,G,WS,PTS,TOV,PER,MP){new1<-predict(m,data.frame(
  G=G,WS=WS,PTS=PTS,TOV=TOV,PER=PER,MP=MP))
msg<-paste( "G=",G,"WS=",WS,"PTS=",PTS,"TOV=",TOV,"PER=",PER,"MP=",MP, " MVP==>",format(new1),sep ="")
print(msg)
}
predict_func(model_set_2,69,0,11.3,1142,73.93,22.8)
model_test<-lm(formula = MVP ~ .,data = validation.dataset )
######################
