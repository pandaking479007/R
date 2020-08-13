install.packages("caret")
install.packages("dplyr")
install.packages("data.table")
install.packages("xgboost")

library(caret)
library(dplyr)
library(data.table)
library(xgboost)


rm(list = ls())
getwd()
setwd("/Users/alex/downloads/pubg-finish-placement-prediction/")

train<- fread("train_v2.csv",stringsAsFactors = F)
train<- train[complete.cases(train),]
test<- fread("test_v2.csv",stringsAsFactors = F)

write.csv(x=solo,file="PUBG_solo_train.csv")
write.csv(x=team,file="PUBG_team_train.csv")
df.solo<-fread("PUBG_solo_train.csv")
df.team<-fread("PUBG_team_train.csv")

#### feature extraction####
#clean solo, team#
train$matchType<-gsub("fpp|tpp|normal|-","",train$matchType)
test$matchType<-gsub("fpp|tpp|normal|-","",test$matchType)

#find negative rankpoint 
train$rankPoints <- ifelse(train$rankPoints == -1, 0, train$rankPoints)
test$rankPoints <- ifelse(test$rankPoints == -1, 0, test$rankPoints)

#build a new feature "totaldist"
train$totaldist <- train$walkDistance + train$rideDistance + train$swimDistance
test$totaldist <- test$walkDistance + test$rideDistance + test$swimDistance

#build a new feature "damage.kill"
train$damage.kill<-train$damageDealt/100-train$kills
test$damage.kill<-test$damageDealt/100-test$kills

#build a new feature "head shot rate"
train$head.shot.rate<-train$headshotKills/train$kills
test$head.shot.rate<-test$headshotKills/test$kills
train$head.shot.rate<-gsub("NaN",0,train$head.shot.rate)
test$head.shot.rate<-gsub("NaN",0,test$head.shot.rate)
train$head.shot.rate<-as.numeric(train$head.shot.rate)
test$head.shot.rate<-as.numeric(test$head.shot.rate)

#### splite the train and test into solo and team ####
solo.train<- train[train$matchType=="solo",]
team.train<- train[train$matchType!="solo",]
solo.test<-test[test$matchType=="solo",]
team.test<- test[test$matchType!="solo",]

team.train<-team.train %>%
  group_by(groupId)%>%
  mutate(maxkill=max(kills),
         maxDistance=max(totaldist),
         maxDamage=max(damageDealt),
         meanKill=mean(kills),
         meanDistance=mean(totaldist),
         meanDamage=mean(damageDealt)
        )
team.test<-team.test%>%
  group_by(groupId)%>%
  mutate(maxkill=max(kills),
         maxDistance=max(totaldist),
         maxDamage=max(damageDealt),
         meanKill=mean(kills),
         meanDistance=mean(totaldist),
         meanDamage=mean(damageDealt)
  )
   

set.seed(1234)
intrain<- createDataPartition(y=solo.train$winPlacePerc,p=0.15,list=FALSE)
training.solo<-solo.train[intrain,]
testing.solo <- solo.train[-intrain,]
rm(intrain)
summary(training.solo)

training.solo<-subset(training.solo,select=-c(Id,groupId,matchId,matchType,teamKills,assists,revives,DBNOs))
testing.solo<-subset(testing.solo,select=-c(Id,groupId,matchId,matchType,teamKills,assists,revives,DBNOs))

intrain<- createDataPartition(y=team.train$winPlacePerc,p=0.15,list=FALSE)
training.team<-team.train[intrain,]
testing.team <- team.train[-intrain,]
rm(intrain)

training.team<-subset(training.team,select=-c(Id,groupId,matchId,matchType))
testing.team<-subset(testing.team,select=-c(Id,groupId,matchId,matchType))


#### lm mode ####

#use 5-fold cross validation
train_control<-trainControl(method="cv",number=5,savePredictions = TRUE)
#solo
modelfit.lm.solo <- train(winPlacePerc~.,
                          data = training.solo,
                          trControl = train_control,
                          method = "lm",
                          importance = TRUE)

lm.solo.pred <- predict(modelfit.lm.solo, newdata = testing.solo)
mean(abs(lm.solo.pred - testing.solo$winPlacePerc))

#team
modelfit.lm.team <- train(winPlacePerc~.,
                          data = training.team,
                          trControl = train_control,
                          method = "lm")

lm.team.pred <- predict(modelfit.lm.team, newdata = testing.team)
mean(abs(lm.team.pred - testing.team$winPlacePerc))

#solo result
solo.test.duplicate <- subset(solo.test, select = -c(Id, groupId, matchType,matchId,teamKills,assists,revives,DBNOs))
solo.pred.lm  <- predict(modelfit.lm.solo, newdata = solo.test.duplicate)
solo.pred.lm <- data.frame(solo.pred.lm)
solo.lm <- cbind(solo.test$Id,solo.pred.lm)
solo.lm$Id <- solo.lm$`solo.test$Id`
solo.lm$`solo.test$Id` <- NULL
solo.lm$winPlacePerc <- solo.lm$solo.pred
solo.lm$solo.pred.lm <- NULL

#team result
team.test.duplicate <- subset(team.test, select = -c(Id, groupId, matchType,matchId))
team.pred.lm  <- predict(modelfit.lm.team, newdata = team.test.duplicate)
team.pred.lm <- data.frame(team.pred.lm)
team.lm <- cbind(team.test$Id, team.pred.lm)
team.lm$Id <- team.lm$`team.test$Id`
team.lm$`team.test$Id` <- NULL
team.lm$winPlacePerc <- team.lm$team.pred
team.lm$team.pred.lm <- NULL

#write csv
final.lm <- rbind(solo.lm,team.lm)
final.lm$winPlacePerc <- ifelse(final.lm$winPlacePerc > 1, 1, final.lm$winPlacePerc)
final.lm$winPlacePerc <- ifelse(final.lm$winPlacePerc < 0, 0, final.lm$winPlacePerc)
write.csv(final.lm,"/Users/alex/downloads/pubg-finish-placement-prediction/lm_model.csv",row.names = FALSE)
summary(final.lm)


#### xgboost ####
#predict the train
xbg.label<-training.solo$winPlacePerc
training.solo.duplicate<-training.solo
training.solo.duplicate$winPlacePerc<-NULL
training.solo.xbg<-xgb.DMatrix(data=as.matrix(training.solo.duplicate),label=as.matrix(xbg.label))

testing.solo.duplicate<-testing.solo
testing.solo.duplicate$winPlacePerc<-NULL
testing.solo.xbg<-xgb.DMatrix(data=as.matrix(testing.solo.duplicate))

set.seed(123)
xbg.model<-xgboost(data=training.solo.xbg,nround=200,booster="gbtree",objective="reg:linear")

xbg.predict<-predict(xbg.model,testing.solo.xbg)
mean(abs(xbg.predict-testing.solo$winPlacePerc))

#0.04253265

#predict the test
#solo
solo.test.dm <- xgb.DMatrix(data = as.matrix(solo.test.duplicate))
solo.pred <- predict(xbg.model,solo.test.dm)
solo.pred <- data.frame(solo.pred)
solo <- cbind(solo.test$Id, solo.pred)
solo$Id <- solo$`solo.test$Id`
solo$`solo.test$Id` <- NULL
solo$winPlacePerc <- solo$solo.pred
solo$solo.pred <- NULL
#team
team.xgb.label <- training.team$winPlacePerc
training.team.duplicate <- training.team
training.team.duplicate$winPlacePerc <- NULL
training.team.xgb.dm <- xgb.DMatrix(data = as.matrix(training.team.duplicate),
                                    label = as.matrix(team.xgb.label))
testing.team.duplicate <- testing.team
testing.team.duplicate$winPlacePerc <- NULL
testing.team.xgb.dm <- xgb.DMatrix(data = as.matrix(testing.team.duplicate))

set.seed(1234)
team.xgb.model <- xgboost(data = training.team.xgb.dm,nround = 200,booster = "gbtree",objective = "reg:linear")
team.xgb.pred <- predict(team.xgb.model,testing.team.xgb.dm)
mean(abs(team.xgb.pred-testing.team$winPlacePerc))

#0.05447553

#predict for the test team
test.team.dm <- xgb.DMatrix(data = as.matrix(team.test.duplicate))
team.pred <- predict(team.xgb.model,test.team.dm)
team.pred <- data.frame(team.pred)
team <- cbind(team.test$Id,team.pred)
team$Id <- team$`team.test$Id`
team$`team.test$Id` <- NULL
team$winPlacePerc <- team$team.pred
team$team.pred <- NULL

# write csv for xgb model
final.xgb <- rbind(solo,team)
final.xgb$winPlacePerc <- ifelse(final.xgb$winPlacePerc > 1, 1, final.xgb$winPlacePerc)
final.xgb$winPlacePerc <- ifelse(final.xgb$winPlacePerc < 0, 0, final.xgb$winPlacePerc)
write.csv(final.xgb,"/Users/alex/downloads/pubg-finish-placement-prediction/xgb_model.csv",row.names = FALSE)
summary(final.xgb)









