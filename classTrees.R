#Reviewing ERIC
#Cleaning the memory
rm(list=ls())

#Loading libraries
library(rpart)
library(adabag)
library(randomForest)

#Importing data
setwd("~/R/bestRegressionModel/")
dDFrame <- read.table("Data.txt",sep="\t",dec=".",header=TRUE)
print(summary(dDFrame))

#Creating trainingData and testingData partition
trainingData  <- dDFrame[dDFrame$sample=="train",1:20]
testingData   <- dDFrame[dDFrame$sample=="test",1:20]

print(summary(trainingData$REGION_TYPE))
print(summary(testingData$REGION_TYPE))

#Evaluation function
error_rate <- function(yobs,ypred){
        mc <- table(yobs,ypred)#Matrix of confusion
		err <- 1.0 - sum(diag(mc))/sum(mc)#rate of error
		return(err)
}#End-function

#Decision Tree
treeOne <- rpart(REGION_TYPE ~ ., data = trainingData)
print(treeOne)

#Prediction
predOne <- predict(treeOne,newdata=testingData,type="class")
#rate of error
print(error_rate(testingData$REGION_TYPE,predOne))

#Decision Stump
paramStump = rpart.control(cp=0,maxdepth=1,minsplit=2,minbucket=1)
treeTwo <- rpart(REGION_TYPE ~ ., data = trainingData,control=paramStump)
print(treeTwo)
#Prediction and Error
predTwo <- predict(treeTwo,newdata=testingData,type="class")
print(error_rate(testingData$REGION_TYPE,predTwo))

#Deep Tree
paramDeep = rpart.control(cp=0,maxdepth=30,minsplit=2,minbucket=1)
treeThree <- rpart(REGION_TYPE ~ ., data = trainingData,control=paramDeep)
print(treeThree)
##Prediction and Error
predThree <- predict(treeThree,newdata=testingData,type="class")
print(error_rate(testingData$REGION_TYPE,predThree))

#Bagging
bagOne <- bagging(REGION_TYPE ~ ., data = trainingData,mfinal=20)
#Prediction and Error
predbagOne <- predict(bagOne,newdata = testingData)
print(error_rate(testingData$REGION_TYPE,predbagOne$class))

#First Tree
print(bagOne$trees[[1]])
print(sort(bagOne$importance,decreasing=TRUE))#Importance 
importanceplot(bagOne,cex.names=0.5,horiz=TRUE) #Importance's plot

#Bagging with one Tree
param      <- rpart.control(maxsurrogate=20)
bagOneTree <- bagging(REGION_TYPE ~ ., data = trainingData,mfinal=1)

print(bagOneTree$trees[[1]])#Printing the Tree
print(bagOneTree$importance)#Importance

#Bagging Decision Stump
bagStump <- bagging(REGION_TYPE ~ ., data = trainingData,mfinal=20, control=paramStump)

#Prediction and Error
predbagStump <- predict(bagStump,newdata = testingData)
print(error_rate(testingData$REGION_TYPE,predbagStump$class))

#Bagging Deep Tree
bagDeep <- bagging(REGION_TYPE ~ ., data = trainingData,mfinal=20, control=paramDeep)

#Prediction and Error
predbagDeep <- predict(bagDeep,newdata = testingData)
print(error_rate(testingData$REGION_TYPE,predbagDeep$class))
numtest <- c(1,5,10,20,50,100,200) #Number of Trees to test

#Learning test
trainTestBag <- function(m){
        bag     <- bagging(REGION_TYPE ~ .,data=trainingData,mfinal=m,control=paramDeep)
		predbag <- predict(bag,newdata = testingData)
		return(error_rate(testingData$REGION_TYPE,predbag$class))
}#End-function

#Evaluating twenty (20) times each m value
result <- replicate(20,sapply(numtest,trainTestBag))

#Plotting system
plot(numtest,apply(result,1,mean),xlab="m",ylab="Err. rate",type="b")
randForOne <- randomForest(REGION_TYPE ~ ., data = trainingData, ntree = 20)

#Prediction and Error
predrandForOne <- predict(randForOne,newdata=testingData,type="class")
print(error_rate(testingData$REGION_TYPE,predrandForOne))

print(randForOne$confusion)#Matrix confusion out-of-bag
print(1-sum(diag(randForOne$confusion))/sum(randForOne$confusion))#Error out-of-bag

#Printing all the variables
print(data.frame(cbind(colnames(trainingData)[2:20],varUsed(randForOne))))
varImpPlot(randForOne) #Importance

print(getTree(randForOne,1))#Access in the 1st Tree
numtest <- c(1,5,10,20,50,100,200)#Number of m Tree to test

#Learning test
trainTestRandFor <- function(m){
            rf     <- randomForest(REGION_TYPE ~ .,data=trainingData,ntree=m)
			predrf <- predict(rf,newdata = testingData)
			return(error_rate(testingData$REGION_TYPE,predrf))
}#End-function

#Evaluating twenty (20) times each m value
result <- replicate(20,sapply(numtest,trainTestRandFor))

#Plotting system
plot(numtest,apply(result,1,mean),xlab="m",ylab="Err. rate",type="b")

#Boosting
boostOne <- boosting(REGION_TYPE ~ ., data = trainingData,mfinal = 20, boos= FALSE)

#Prediction and Error
predboostOne <- predict(boostOne,newdata = testingData)
print(error_rate(testingData$REGION_TYPE,predboostOne$class))

#Boosting Decision Stump
boostModel <- boosting(REGION_TYPE ~ ., data = trainingData,mfinal=20, boos=FALSE, coeflearn= 'Zhu', control=paramStump)

#Prediction and Error
predboostModel <- predict(boostModel,newdata = testingData)
print(error_rate(testingData$REGION_TYPE,predboostModel$class))

#Boosting the tree very deep
boostDeep <- boosting(REGION_TYPE ~ ., data = trainingData,mfinal=20, boos=FALSE, coeflearn= 'Zhu', control=paramDeep)

#Prediction and Error
predboostDeep <- predict(boostDeep,newdata = testingData)
print(error_rate(testingData$REGION_TYPE,predboostDeep$class))

numtest <- c(1,5,10,20,50,100,200)#Number of m Tree to test

#Learning test
trainTestBoost <- function(m){
            bo     <- boosting(REGION_TYPE ~ .,data=trainingData,mfinal=m,coeflearn='Zhu')
			predbo <- predict(bo,newdata = testingData)
			return(error_rate(testingData$REGION_TYPE,predbo$class))
}#End-function

#Evaluating twenty (20) times each m value
result <- replicate(20,sapply(numtest,trainTestBoost))
#Plotting system
plot(numtest,apply(result,1,mean),xlab="m",ylab="Err. rate",type="b")
