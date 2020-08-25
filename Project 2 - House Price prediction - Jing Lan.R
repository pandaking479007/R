install.packages("caret")
install.packages("dplyr")
install.packages("tidyr")
install.packages("corrplot")
install.packages("xgboost")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("pcr")

library(caret)
library(dplyr)
library(tidyr)
library(corrplot)
library(xgboost)
library(ggplot2)
library(randomForest)
library(pcr)


rm(list = ls())
getwd()
setwd("/Users/alex/downloads/house-prices-advanced-regression-techniques/")

train<- read.csv("train.csv",stringsAsFactors = F)
test<- read.csv("test.csv",stringsAsFactors = F)

#### Data Exploring ####
summary(train)
str(train)
sort(colSums(is.na(train)),decreasing = TRUE)
#examine all columns in train data set, train set has 81 variables and 1460 obs, of which "PoolQC", "MiscFeature", "Alley",
#"Fence", "FireplaceQu" and "LotFrontage" have most of NAs.

summary(test)
str(test)
sort(colSums(is.na(test)),decreasing = TRUE)
#examine all columns in test data set, test set has 80 variables and 1459 obs. Similar to train set, variables in test set 
#hava a lot of NAs.


#### Data cleansing & imputing####
#PoolQC column
table(train$PoolQC)
sum(is.na(train$PoolQC))
#We now can observe that PoolQC column has "Ex", "Fa","Gd" and NAs. Accordingly, we can assign different values 
#to different levels and assign "None" to NAs.
train$PoolQC[is.na(train$PoolQC)] <- 'None'
recode(train$PoolQC,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
train$PoolQC<-as.factor(train$PoolQC)
table(train$PoolQC)

test$PoolQC[is.na(test$PoolQC)] <- 'None'
recode(test$PoolQC,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$PoolQC<-as.factor(test$PoolQC)
table(test$PoolQC)
#Now all NAs in train and test sets are fixed.

#MiscFeature column
#there are 1406 and 1408 NA's in train and test MiscFeature column, respectively.
train$MiscFeature[is.na(train$MiscFeature)] <- 'None'
#assign"None"to NA's
table(train$MiscFeature)
test$MiscFeature[is.na(test$MiscFeature)] <- 'None'
table(test$MiscFeature)


#Alley column
#there are 1369 and 1352 NA's in train and test Alley column, respectively.
train$Alley[is.na(train$Alley)] <- 'None'
#assign"None"to NA's
train$Alley <- as.factor(train$Alley)
#since the values are not ordinal, so I convert it into a factor.
table(train$Alley)

test$Alley[is.na(test$Alley)] <- 'None'
test$Alley <- as.factor(test$Alley)
table(test$Alley)


#Fence Column
#there are 1179,1169 NA's in train and test Fence column, respectively.
train$Fence[is.na(train$Fence)] <- 'None'
table(train$Fence)
#now we have "GdPrv","GdWo","MnPrv","MnWw" and "None" five categories, but are they ordinal?
train[!is.na(train$SalePrice),] %>%
  group_by(Fence) %>% 
  summarise(median = median(SalePrice), counts=n())
#as we can see, housing price without fence appears to have the highest value. 
recode(train$Fence, 'MnWw' = 1, 'MnPrv' = 2, 'GdWo' = 3, 'GdPrv' = 4, 'None' = 5)
train$Fence<-as.factor(train$Fence)

test$Fence[is.na(test$Fence)] <- 'None'
recode(test$Fence, 'MnWw' = 1, 'MnPrv' = 2, 'GdWo' = 3, 'GdPrv' = 4, 'None' = 5)
test$Fence<-as.factor(test$Fence)


#FireplaceQu column
#there are 690 and 730 NA's in train and test FireplaceQu column, respectively.
train$FireplaceQu[is.na(train$FireplaceQu)] <- 'None'
recode(train$FireplaceQu,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
train$FireplaceQu<-as.factor(train$FireplaceQu)
table(train$FireplaceQu)

test$FireplaceQu[is.na(test$FireplaceQu)] <- 'None'
recode(test$FireplaceQu,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$FireplaceQu<-as.factor(test$FireplaceQu)
table(test$FireplaceQu)
#assign 'None" to NA's first and then assign 'None' to 0 and other characters accordingly.

#LotFrontage column
#Linear feet of street connected to property, there are 259 and 227 NA's in train and test LotFrontage column, respectively.
LotFrontage<-train[!is.na(train$LotFrontage),] %>%
  group_by(Neighborhood)%>%
  summarise(median=median(LotFrontage))%>%
  arrange(desc(median))

LotFrontage
#it is rational to impute the NA by the median LotFrontage of each neighborhood.
train<-train %>% 
  group_by(Neighborhood) %>% 
  mutate(LotFrontage= replace(LotFrontage, is.na(LotFrontage), mean(LotFrontage, na.rm=TRUE)))
         
train$LotFrontage<-as.integer(train$LotFrontage)

test<-test %>% 
  group_by(Neighborhood) %>% 
  mutate(LotFrontage= replace(LotFrontage, is.na(LotFrontage), mean(LotFrontage, na.rm=TRUE)))

test$LotFrontage<-as.integer(test$LotFrontage)
#all LotFrontage NA's have been replaced by its neighborhood mean.

#GarageYrBlt  
#there are 81 and 78 NA's in train and test GarageYrBlt column, respectively.
#normolly, the garage and the house were build at the same time, so I use YearBuild to replace NA in GarageYrBlt
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- train$YearBuilt[is.na(train$GarageYrBlt)]
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- test$YearBuilt[is.na(test$GarageYrBlt)]

#GarageType 
#there are 81 and 78 NA's in train and test GarageType column, respectively.
train$GarageType[is.na(train$GarageType)] <- 'No Garage'
table(train$GarageType)
#the GarageType is not ordinal, so I converted them into factors.
train$GarageType <- as.factor(train$GarageType)

test$GarageType[is.na(test$GarageType)] <- 'No Garage'
test$GarageType <- as.factor(test$GarageType)

#GarageFinish 
#there are 81 and 78 NA's in train and test GarageFinish column, respectively.
table(train$GarageFinish)
#we can assign different value to them and they are ordinal.
train$GarageFinish[is.na(train$GarageFinish)] <- 'None'
recode(train$GarageFinish,'None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)
train$GarageFinish<-as.factor(train$GarageFinish)
table(train$GarageFinish)

test$GarageFinish[is.na(test$GarageFinish)] <- 'None'
recode(test$GarageFinish,'None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)
test$GarageFinish<-as.factor(test$GarageFinish)

#GarageQual
#there are 81 and 78 NA's in train and test GarageQual column, respectively.
table(train$GarageQual)
#apprently, it is ordinal and can be assigned.
train$GarageQual[is.na(train$GarageQual)] <- 'None'
recode(train$GarageQual,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
train$GarageQual<-as.factor(train$GarageQual)
table(train$GarageQual)

test$GarageQual[is.na(test$GarageQual)] <- 'None'
recode(test$GarageQual,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$GarageQual<-as.factor(test$GarageQual)

#GarageCond
#there are 81 and 78 NA's in train and test GarageCond column, respectively.
table(train$GarageCond)
#it is ordinal and can be assigned.
train$GarageCond[is.na(train$GarageCond)] <- 'None'
recode(train$GarageCond,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
train$GarageCond<-as.factor(train$GarageCond)
table(train$GarageCond)

test$GarageCond[is.na(test$GarageCond)] <- 'None'
recode(test$GarageCond,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
test$GarageCond<-as.factor(test$GarageCond)

#GarageCars and GarageArea
#both have 1 NA in the test
table(test$GarageCars)
test$GarageCars[is.na(test$GarageCars)] <-names(sort(-table(test$GarageCars)))[1]
table(train$GarageCars)
recode(train$GarageCars,'0' = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5)
recode(test$GarageCars,'0' = 0, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 5)
train$GarageCars<-as.integer(train$GarageCars)
test$GarageCars<-as.integer(test$GarageCars)

which(is.na(test$GarageArea))
test$GarageArea[is.na(test$GarageArea)]<-as.integer(0)
#based on similar garage type, assign the NA to 0.




#BsmtExposure
#there are 38 and 45 NA's in train and test BsmtExposure column, respectively.
table(train$BsmtExposure)
#the rates are ordinal and can be re assigned
train$BsmtExposure[is.na(train$BsmtExposure)] <- 'None'
recode(train$BsmtExposure,'None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
train$BsmtExposure<-as.factor(train$BsmtExposure)
table(train$BsmtExposure)

test$BsmtExposure[is.na(test$BsmtExposure)] <- 'None'
recode(test$BsmtExposure,'None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)
test$BsmtExposure<-as.factor(test$BsmtExposure)

#BsmtCond
#there are 37 and 45 NA's in train and test BsmtCond column, respectively.
table(train$BsmtCond)
#it is ordinal
train$BsmtCond[is.na(train$BsmtCond)] <- 'None'
recode(train$BsmtCond,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4)
train$BsmtCond<-as.factor(train$BsmtCond)
table(train$BsmtCond)

test$BsmtCond[is.na(test$BsmtCond)] <- 'None'
recode(test$BsmtCond,'None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4)
test$BsmtCond<-as.factor(test$BsmtCond)

#BsmtQual
#there are 37 and 44 NA's in train and test BsmtQual column, respectively.
table(train$BsmtQual)
#it is ordinal and can be assigned
train$BsmtQual[is.na(train$BsmtQual)] <- 'None'
recode(train$BsmtQual,'None' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3,'Ex'=4)
train$BsmtQual<-as.factor(train$BsmtQual)
table(train$BsmtQual)

test$BsmtQual[is.na(test$BsmtQual)] <- 'None'
recode(test$BsmtQual,'None' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3,'Ex'=4)
test$BsmtQual<-as.factor(test$BsmtQual)

#BsmtFinType1
#there are 37 and 42 NA's in train and test BsmtQual column, respectively.
table(train$BsmtFinType1)
#stil, all the ranks can be ordinal
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 'None'
recode(train$BsmtFinType1,'None' = 0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
train$BsmtFinType1<-as.factor(train$BsmtFinType1)
table(train$BsmtFinType1)

test$BsmtFinType1[is.na(test$BsmtFinType1)] <- 'None'
recode(test$BsmtFinType1,'None' = 0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
test$BsmtFinType1<-as.factor(test$BsmtFinType1)

#BsmtFinType2
#there are 38 and 42 NA's in train and test BsmtQual column, respectively.
table(train$BsmtFinType2)
#stil, all the ranks can be ordinal
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- 'None'
recode(train$BsmtFinType2,'None' = 0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
train$BsmtFinType2<-as.factor(train$BsmtFinType2)
table(train$BsmtFinType2)

test$BsmtFinType2[is.na(test$BsmtFinType2)] <- 'None'
recode(test$BsmtFinType2,'None' = 0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
test$BsmtFinType2<-as.factor(test$BsmtFinType2)

#BsmtFullBath
#only 2 NA's in the test
table(test$BsmtFullBath)
#it is ordinal and majorities are 0
test$BsmtFullBath[is.na(test$BsmtFullBath)] <-names(sort(-table(test$BsmtFullBath)))[1]


train$BsmtFullBath<-as.integer(train$BsmtFullBath)
test$BsmtFullBath<-as.integer(test$BsmtFullBath)


#BsmtHalfBath
#only 2 NA's in the test
table(test$BsmtHalfBath)
#it is ordinal and majorities are 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <-names(sort(-table(test$BsmtHalfBath)))[1]

train$BsmtHalfBath<-as.integer(train$BsmtHalfBath)
test$BsmtHalfBath<-as.integer(test$BsmtHalfBath)


#BsmtUnfSF,BsmtFinSF1,BsmtFinSF2,TotalBsmtSF
#those 4 columns all have 1 NA in the test
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- names(sort(-table(test$BsmtUnfSF)))[1]
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- names(sort(-table(test$BsmtFinSF1)))[1]
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- names(sort(-table(test$BsmtFinSF2)))[1]
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- names(sort(-table(test$TotalBsmtSF)))[1]

train$BsmtUnfSF<-as.integer(train$BsmtUnfSF)
test$BsmtUnfSF<-as.integer(test$BsmtUnfSF)

train$BsmtFinSF1<-as.integer(train$BsmtFinSF1)
test$BsmtFinSF1<-as.integer(test$BsmtFinSF1)

train$BsmtFinSF2<-as.integer(train$BsmtFinSF2)
test$BsmtFinSF2<-as.integer(test$BsmtFinSF2)

train$TotalBsmtSF<-as.integer(train$TotalBsmtSF)
test$TotalBsmtSF<-as.integer(test$TotalBsmtSF)

#assign the 1 NA to the group which has the most values.






#MasVnrType
#there are 8 and 16 NA's in train and test BsmtQual column, respectively.
table(train$MasVnrType)
#I have no clue which type will be favorabele to sales price. So I decide to see the median price for each one
MasVnrType<-train[!is.na(train$MasVnrType),] %>%
  group_by(MasVnrType)%>%
  summarise(median=median(SalePrice))%>%
  arrange(desc(median))

MasVnrType
#obviously, stone>brkface>None>Brkcmn
train$MasVnrType[is.na(train$MasVnrType)] <- 'None'
recode(train$MasVnrType,'BrkCmn' = 0, 'None'=1, 'BrkFace'=2, 'Stone'=3)
train$MasVnrType<-as.factor(train$MasVnrType)
table(train$MasVnrType)

test$MasVnrType[is.na(test$MasVnrType)] <- 'None'
recode(test$MasVnrType,'BrkCmn' = 0, 'None'=1, 'BrkFace'=2, 'Stone'=3)
test$MasVnrType<-as.factor(test$MasVnrType)

#MasVnrArea
#there are 8 and 15 NA's in train and test BsmtQual column, respectively.
train$MasVnrArea<-ifelse(is.na(train$MasVnrArea)&train$MasVnrType=='None',0,train$MasVnrArea)
test$MasVnrArea<-ifelse(is.na(test$MasVnrArea)&test$MasVnrType=='None',0,test$MasVnrArea)
train$MasVnrArea<-as.integer(train$MasVnrArea)
test$MasVnrArea<-as.integer(test$MasVnrArea)
#I found that if the MasVnrType is none, the MasVnrArea should be 0.

#Electrical
#only 1 NA in the train.
table(train$Electrical)
#it is categorical.
train$Electrical[is.na(train$Electrical)] <- names(sort(-table(train$Electrical)))[1]
train$Electrical <- as.factor(train$Electrical)
test$Electrical <- as.factor(test$Electrical)
#because among all 5 categories, SBrkr has the highest numbers, so we assign the NA to SBrkr.

#MSZoning
#only 1 NA in the test
table(test$MSZoning)
test$MSZoning[is.na(test$MSZoning)] <- names(sort(-table(test$MSZoning)))[1]
train$MSZoning <- as.factor(train$MSZoning)
test$MSZoning <- as.factor(test$MSZoning)
#because among all 5 categories, RL has the highest numbers, so we assign the NA to RL.

#Utilities
#only 2 NA's in the test
table(test$Utilities)
#1457 out of 1459 are AllPub
test$Utilities[is.na(test$Utilities)] <-names(sort(-table(test$Utilities)))[1]
train$Utilities<-as.factor(train$Utilities)
test$Utilities<-as.factor(test$Utilities)

#Functional
#only 2 NA's in the test
table(test$Functional)
#it is ordinal and majorities are Typ
test$Functional[is.na(test$Functional)] <-names(sort(-table(test$Functional)))[1]
recode(test$Functional,'Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)
train$Functional<-as.factor(train$Functional)
test$Functional<-as.factor(test$Functional)


#Exterior1st
#only 1 NA in the test
table(test$Exterior1st)
#seems categorical.
test$Exterior1st[is.na(test$Exterior1st)] <- names(sort(-table(test$Exterior1st)))[1]
train$Exterior1st <- as.factor(train$Exterior1st)
test$Exterior1st <- as.factor(test$Exterior1st)
table(test$Exterior1st)

#Exterior2nd
#only 1 NA in the test
table(test$Exterior2nd)
#seems categorical.
test$Exterior2nd[is.na(test$Exterior2nd)] <- names(sort(-table(test$Exterior2nd)))[1]
train$Exterior2nd <- as.factor(train$Exterior2nd)
test$Exterior2nd <- as.factor(test$Exterior2nd)
table(test$Exterior2nd)

#KitchenQual
#only 1 NA in the test
table(test$KitchenQual)
#it is ordinal
test$KitchenQual[is.na(test$KitchenQual)] <-names(sort(-table(test$KitchenQual)))[1]
recode(test$KitchenQual,'Fa'=0, 'TA'=1, 'Gd'=2, 'Ex'=3)
train$KitchenQual <- as.factor(train$KitchenQual)
test$KitchenQual <- as.factor(test$KitchenQual)


#SaleType
#only 1 NA in the test
table(test$SaleType)
#it is categorical
test$SaleType[is.na(test$SaleType)] <-names(sort(-table(test$SaleType)))[1]
train$SaleType <- as.factor(train$SaleType)
test$SaleType <- as.factor(test$SaleType)
#assign the NA to the biggest group and make all category factor

####other character variables####
table(train$HeatingQC)
recode(train$HeatingQC,'Po'=0, 'Fa'=1, 'TA'=2, 'Gd'=3, 'Ex'=4)
train$HeatingQC <- as.factor(train$HeatingQC)
test$HeatingQC <- as.factor(test$HeatingQC)

#all those character variables need to be converted into factor, so I wrote a for loop.
v<-c("Street","Heating","Foundation","LotShape","LandContour","LotConfig","LandSlope",
             "Neighborhood","Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl",
             "ExterQual","ExterCond","BsmtFinSF1","CentralAir","PavedDrive","MiscFeature","SaleCondition")

for(i in v) {
  train[[i]] <- as.factor(train[[i]]) 
  test[[i]] <- as.factor(test[[i]]) 
  }

str(train)
str(test)
#all chr variales have been converted into factor,now the train and test only have int or factor variables.




####Feature Selection####
#let's take a look at numetric columns in the train and test data set.
train_numeric_loc<-which(sapply(train,is.numeric))
length(train_numeric_loc)
test_numeric_loc<-which(sapply(test,is.numeric))
length(test_numeric_loc)
#now we can observe that train has 36 numeric columns and test has 35 numeric columns
train_numeric<-select_if(train, is.numeric)
train_numeric<-train_numeric[ , -which(names(train_numeric) %in% c("Neighborhood","Id"))]
#id and Neighborhood are useless to the this correlation here.
test_numeric<-select_if(test,is.numeric)
test_numeric<-test_numeric[ , -which(names(test_numeric) %in% c("Neighborhood","Id"))]

cor_train<-cor(train_numeric)
corrplot(cor_train, tl.col="black",tl.cex = 0.8,cl.cex = .8, number.cex=1,type = "upper",method="number")
#Now, we would be able to see that the most influencial numeric variables to sale price are "OverQual","GrLiveArea"
#"GarageArea","TotalBsmtSF","X1stFlrSF","FullBath",etc. 

####Dimenstionality Reduction #### 
#PCA
#turn off scientfic notation
options(scipen = 999)
#locate whihc columns have variances of 0, which will causing the scaling to fail.
which(apply(train, 2, var)==0)
which(apply(test, 2, var)==0)

#initiate PCA and scaling on all numeric variables.
train.pca=prcomp(train_numeric, scale= TRUE)
names(train.pca)

#use rotation to get principal component loadings;
train.pca$rotation
#variance explained by each principal component
train.pca.var=train.pca$sdev^2
#compute the proportion of variance explained by each principal component
pve<-train.pca.var/sum(train.pca.var)
pve

#visualize the PC
biplot(train.pca)
plot(pve,ylim=c(0,1),type='b')
ggplot(data.frame(train.pca$x),aes(x=PC1,y=PC2,color=PC1))+geom_point()

#examine weights in PC1 and PC2 for all columns
train.pca$rotation[,1:2]
PC1<-as.data.frame(sort(abs(train.pca$rotation[,1]),decreasing= TRUE))
PC1
PC2<-as.data.frame(sort(abs(train.pca$rotation[,2]),decreasing =TRUE))
PC2
#how to explain this? More variables that have more explaining ablity have been selected?


set.seed (1234)
pcr_model<-pcr(SalePrice~., data = train, scale = TRUE, validation = "CV")
pcr_pre<-predict(pcr_model,test)










####modeling####
#Ramdon Forest 
set.seed(123)
train$BsmtFinSF1<-as.integer(train$BsmtFinSF1)
test$BsmtFinSF1<-as.integer(test$BsmtFinSF1)
#I don't know what is goping on, everytime when I run the RF below, R tells me my BsmtFinSF1 variables 
#still have 53 factors, which I have set them interger long before. So I have to put them here.
train.rf<- randomForest(SalePrice~.,data=train,mtry=100,importantce = TRUE)
train.pre<-predict(train.rf,newdata = test)
#??????because the test doesn't have SalePrice Column??






train.rf.imp<-importance(train.rf)
train.df.imp<-data.frame(Variables = row.names(train.rf.imp), MSE = train.rf.imp[,1])
train.df.imp <- train.df.imp[order(train.df.imp$MSE, decreasing = TRUE),]

ggplot(train.df.imp[1:10,],
       aes(x=reorder(Variables,MSE),
           y=MSE/100000000000,
          fill=MSE))+geom_bar(stat = 'identity') +ylim(0,50)+labs(x = 'Variables', y= "MSE/100000000000")
#Apparently, thanks to ramdon forest works well both on numeric and categorical variables, hence, it is 
#observable that "QverallQual","Neiborhood',"GrliveAare","TotalBsmtSF" ,etc are quite important to sale price.


set.seed(1234)
myControl<- trainControl(method="cv",number=5)

#lasso regression
myGrid.ridge<- expand.grid(alpha=1,lambda=seq(0,0.5,length=5))
#alpha = 0 refers to ridge regression and 1 lasso regression

model.ridge<-train(SalePrice~.,
                   data=train,
                   method="glmnet",
                   tuneGrid=myGrid.ridge,
                   trControl=myControl)

model.ridge$results
model.pred.ridge<- predict(model.ridge, newdata = test)
#???
mean((test$SalePrice-model.pred.ridge)^2)
#???

#linear model
model.lm<-train(SalePrice~.,
                data=train,
                method="lm",
                trControl=myControl)

model.lm$results
model.pred.lm <- predict(model.lm, newdata = test)

#XGboost model








