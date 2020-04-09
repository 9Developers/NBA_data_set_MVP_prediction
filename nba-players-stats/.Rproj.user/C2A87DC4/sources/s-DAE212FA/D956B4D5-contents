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
MVPTable_s<-MVPTable_s[,-c(1:9)]
MVPTable_s$MVP_PLAYER<-MVPTable_s$PLAYER
MVPTable_s<-MVPTable
MVPTable_s$Year<-MVPTable_s$ï..YEAR
###########
#merging data frames to create one merged data set.
?merge
stats_merged<-merge(season_stats_2,MVPTable_s,incomparables = NA)
summary(season_stats_2)
stats_merged_1<-stats_merged
#######
#to add a MVP column in the merged data set 
stats_merged_1 <- stats_merged_1 %>%
  mutate(MVP = ifelse(as.character(stats_merged_1$Player)==
                        as.character(stats_merged_1$MVP_PLAYER),
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
ti<-sample(c(1:20000),12000)#pick 12000 out of 20000 to train
train.dataset<-df.set[ti,]
validation.dataset<-df.set[-ti,]#

model_set<-train(MVP ~ .,data = train.dataset,method="glm")
model_set
model_set_2<-train(MVP ~ .,data = train.dataset,method="glm",preProcess=c('scale','center'))
model_set_2
prediction<-predict(model_set_2,validation.dataset)
table(prediction)

####working till here
##############
# salary_prediction <- function(m, point, minutes, turn_over){
#   pre_new <- predict(m, data.frame(PPG = point, MPG = minutes, TOPG = turn_over))
#   msg <- paste("PPG:", point, ",MPG:", minutes, ",TOPG:", turn_over, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
#   print(msg)
# }
# 
# model <- lm(formula = salary17_18 ~ PPG + MPG + TOPG, data = stats_salary_regression)
# # Prediction on Salary of J.J. Redick
# salary_prediction(model,18.3,30,1.4)
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