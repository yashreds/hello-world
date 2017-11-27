
# Introduction to R
Name = c("Yash", "Yash2")
Mark = c(90,91)
Data = data.frame(Name,Mark)
 
#To convert into different datatypes
a=85/23
#a=3.69
b=as.integer(a)
#a=3
#to round off the numbers, use
ceiling()
floor()
round(a,digits=1)
 
#To repeat numbers or vectors
x <- rep(c(1,2,3),c(2,1,4)
        
#To add rows to this dataframe, create another dataframe
Name= c("Yash3","Yash4")
Mark= c(50,60)
NewData= data.frame(Name,Mark)
Combine= rbind(Data,NewData)
#########################
summary(WHO)
tapply(WHO$ChildMortality, WHO$Region, mean)
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)
ls()
which.min(WHO$Under15)
plot(WHO$GNI, WHO$FertilityRate)
Outliers= subset(WHO, GNI > 10000 & FertilityRate > 2.5)
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region)
table(WHO$Region)
tapply(WHO$Over60,WHO$Region,mean)
match("Candy", USDA$Description)
x <- matrix(c(2,3,4,5), nrow=2,ncol=2,byrow="TRUE")
seq(0,100,2) #0 to 100 with diff 2
#One variable linear Regression
model1 = lm(Price ~ AGST, data=wine)
#lm() is the function for linear regression and data gives idea from where the data is to be fetched
summary(model1)
#Gives the intercept value
model1$residuals
#model1 has residual values for computing SSE
SSE = sum(model1$residuals^2)
#lower the value higher the efficiency, and higher the residual value higher the efficiency
model3 = lm(Price ~ AGST+HarvestRain+WinterRain+Age+FrancePop, data=wine)
summary(model3)
SSE = sum(model3$residuals^2)
#has the highest efficiency and lowest residual value
#MONEYBALL
moneyball = subset(baseball, Year<2002)
moneyball$RD = moneyball$RS - moneyball$RA
 
#Linear Regression Equation
Runs Scored/Allowed= intercept estimate + OBP/OOBP + SLG/OSLG
Wins= intercept estimate + (W~RD)(Runs scored-Runs Allowed)
#NBA points prediction for current season
NBA= read.csv("NBA_train.csv")
str(NBA)
PointsReg= lm(PTS ~ X2PA + X3PA + STL, data=NBA)
SSE= sum(PointsReg$residuals^2) #if SSE is too big, then go for RMSE
RMSE= sqrt(SSE/nrow(NBA))
#Predict points for upcoming seasons using the test.csv file
PointsPrediction = predict(PointsReg4, newdata=NBA_test)
SSE= sum(PointsPrediction - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE/SST
RMSE = sqrt(SSE/nrow(NBA_test))
 
 
 
 
 
 
 
#Logistic Regression
#to split the training and testing set, we need to install a new package to R
library(caTools)
 
split=sample.split(quality$PoorCare,SplitRatio=0.75)
qualityTrain=subset(quality,split==TRUE)
qualityTest=subset(quality,split==FALSE)
nrow(qualityTrain)
nrow(qualityTest)
 
LogRegr=glm(PoorCare ~ OfficeVisits+Narcotics, data=qualityTrain,family=binomial)
 
summary(LogRegr)
 
 
#Predicting the Quality of the logisitic regression model
 
#Model
 
set.seed(1000)
framingham=read.csv("framingham.csv")
split=sample.split(framingham$TenYearCHD, SplitRatio= 0.65)
Train=subset(framingham, split==TRUE)
Test=subset(framingham, split==FALSE)
#taking all the other fields for logistic regression
framinghamlog=glm(TenYearCHD ~ ., data=Train, family=binomial)
summary(framinghamlog)
 
#check out for significant coefficients
 
#Designing a Prediction model
predictTest=predict(framinghamlog, type="response", newdata=Test)
table(Test$TenYearCHD, predictTest> 0.5)
 
#Checking for Accuracy
 
table(Test$TenYearCHD, predictTest > 0.5)
 
FALSE TRUE
0  1069    6
1   187   11
 
Accuracy= (1069+11)/(1069+11+6+187)= 0.84= 84% accuracy
 
--> # 0.5 is the threshold value which is to be manually picked
 
#Picking threshold is better explained using an ROC Curve 
install.packages("ROCR")
ROCRPred=predict(predictTrain,qualityTrain$PoorCare)
ROCRPerf=performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf,colorize=TRUE)
 
#TREE USING RPART AND RPART.PLOT
spl=sample.split(stevens$Reverse, SplitRatio=0.7)
Train=subset(stevens,spl==TRUE)
Test=subset(stevens,spl=FALSE)
StevensTree = rpart(Reverse ~ Circuit + Issue, data=Train, method="class", minbucket=25)
prp(StevensTree)
PredictCART=predict(StevensTree, type="class", newdata=Test)
table(Test$Reverse, PredictCART)
#use of method=class is used to show that the tree is a classification tree and minbucket denotes that the tree fits in properly. The lower the value, the more it fits. The higher the value, the more generalised the tree is
 
 
#Using Random Forest method(to build a regression tree)
spl=sample.split(stevens$reverse, SplitRatio=0.6)
Train=subset(stevens,spl==TRUE)
Test=subset(stevens,spl==FALSE)
StevensTree=randomForest(Reverse~Circuit+Issue, data=Train, nodesize=25,ntree=200)
Train$Reverse=as.factor(Train$Reverse)
Test$Reverse=as.factor(Test$Reverse)
StevensTree=randomForest(Reverse~Circuit+Issue, data=Train, nodesize=25,ntree=200)
#To check the accuracy of the model:
PredictForest=predict(StevensTree,newdata=Test)
table(Test$Reverse, PredictForest)
 
 
#Using Baseline method
spl=sample.split(Claims$bucket2009, SplitRatio=0.6)
Train=subset(Claims, spl=TRUE)
Test=subset(Claims, spl=FALSE)
table(Claims$bucket2009, Claims$bucket2008)
#now upon calculating the sum of the elements in the diagonal and dividing by nrow(Claims) will give the accuracy of the model
 
 
 
#BOSTON DATA
plot(boston$LON, boston$LAT)
#CHAS refers to proximity to the river, if 1, city is close to river and viceversa
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue",pch=19)
#Here 3531 is the TRACT value that represents MIT(according to data)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red",pch=19)
#Now to detect the area with the most polluted area, we have the NOX variable, so
points(boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],col="green",pch=0.19)
# Now to detect the area where housing is costly, we have the MEDV variable, so
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red",pch=19)
 
 
# Draw a vertical or horizontal line in the plot
abline(v=-71.07)
abline(h=42.17)