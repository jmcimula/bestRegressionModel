rm(list=ls())

#function for error evaluation
err_rate <- function(D,prediction){
  #confusion matrix
  mc <- table(D$chiffre,prediction)
  #error rate
  err <- 1 - sum(diag(mc))/sum(mc)
  print(paste("Error rate :",round(100*err,2),"%"))
}

#Loading libraries
library(rpart)
library(adabag)
library(gbm)
library(xgboost)


setwd("~/R/bestRegressionModel/")

#import the training and test sets
dtrain <- read.table("opt_digits_train.txt",header=T,sep="\t")
dtest <- read.table("opt_digits_test.txt",header=T,sep="\t")

#type of the variables
print(sapply(dtrain,class))


###############
# decision tree
###############


m.tree <- rpart(chiffre ~ ., data = dtrain)
print(m.tree)

#variable importance
print(m.tree$variable.importance)

#prediction
y.tree <- predict(m.tree, newdata = dtest, type = "class")

#error rate
err_rate(dtest,y.tree)

#modify the settings - deeper tree
param.tree.2 <- rpart.control(minsplit=5,minbucket=2,cp=0)

#tree with the new settings
m.tree.2 <- rpart(chiffre ~ ., data = dtrain, control = param.tree.2)
print(m.tree.2)

#prediction
y.tree.2 <- predict(m.tree.2, newdata = dtest, type = "class")

#error rate
err_rate(dtest,y.tree.2)

##########
# boosting
##########

#boosting
m.boosting <- boosting(chiffre ~ ., data = dtrain, boos = FALSE, mfinal = 100, coeflearn = 'Zhu')

#prediction
y.boosting <- predict(m.boosting, newdata = dtest)

#error rate
err_rate(dtest,y.boosting$class)

###################
# gradient boosting
###################

#learning process - default settings
m.gbm.default <- gbm(chiffre ~ ., data = dtrain, distribution="multinomial")

#print
print(m.gbm.default)

#summary -> variable importance
print(head(summary(m.gbm.default),10))

#predict ==> score for each class
p.gbm.default <- predict(m.gbm.default,newdata=dtest,n.trees=m.gbm.default$n.trees)
print(head(p.gbm.default[,,1],6))

#transform score into prediction
y.gbm.default <- factor(levels(dtrain$chiffre)[apply(p.gbm.default[,,1],1,which.max)])

#error rate
err_rate(dtest,y.gbm.default)

#print the properties of the object
print(attributes(m.gbm.default))

#100 trees
print(m.gbm.default$n.trees)

#1 : decision stumps are used 
print(m.gbm.default$interaction.depth)

#0.001 : very low convergence
print(m.gbm.default$shrinkage)

#0.5 : fraction of the train set used at each step
print(m.gbm.default$bag.fraction)

#modify the settings for deeper trees and stronger correctionat each step
m.gbm.2 <- gbm(chiffre ~ ., data = dtrain, distribution="multinomial",interaction.depth=6,shrinkage=0.1)

#print
print(m.gbm.2)

#prediction
p.gbm.2 <- predict(m.gbm.2,newdata=dtest,n.trees=m.gbm.2$n.trees)
y.gbm.2 <- factor(levels(dtrain$chiffre)[apply(p.gbm.2[,,1],1,which.max)])

#error rate
err_rate(dtest,y.gbm.2)

##################
# package "mboost"
##################

#package mboost
library(mboost)

#learning ==> ERROR
m.mb.def <- blackboost(chiffre ~ ., data = dtrain, family=Multinomial())


###################
# package "xgboost"
###################

#convert the type of the descriptors - learning set
XTrain <- data.frame(lapply(dtrain[,-1],as.numeric))
XTrain <- as.matrix(XTrain)

#recode the target attribute
yTrain <- unclass(dtrain$chiffre)-1

#learn the classifier with the default settings (eta=0.3, max.depth=6)
m.xg.def <- xgboost(data=XTrain,label=yTrain,objective="multi:softmax",num_class=10,nrounds=100)

#convert the type of the descriptors - test set
XTest <- data.frame(lapply(dtest[,-1],as.numeric))
XTest <- as.matrix(XTest)

#prediction
y.xg.def <- predict(m.xg.def,newdata=XTest)+1

#test error rate
err_rate(dtest,y.xg.def)

#variable importance
print(head(xgb.importance(colnames(XTrain),model=m.xg.def),15))

#variable importance - graphical representation
library(Ckmeans.1d.dp)
xgb.plot.importance(xgb.importance(colnames(XTrain),model=m.xg.def))

#learn the classifer with modified settings
m.xg.2 <- xgboost(data=XTrain,label=yTrain,objective="multi:softmax",num_class=10,nrounds=100,eta=0.1,colsample_bytree=0.125)

#prediction
y.xg.2 <- predict(m.xg.2,newdata=XTest)+1

#test error rate
err_rate(dtest,y.xg.2)

#variable importance
print(head(xgb.importance(colnames(XTrain),model=m.xg.2),15))
