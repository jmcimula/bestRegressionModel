#Naive Bayes classification is based on Bayes Theorem. Naive Bayes assumes that the features contribute independently to determine the classification of the final outcome.
#Naive Bayes is a particularly handy tool you can use to address classification based problems. It can be implemented using the naiveBayes() function in e1071 package or naive.bayes() in bnlearn package.

#Loading libraries
library(e1071)
#library(class)

data("diamonds") #Loading economics dataset

summary(diamonds) #Descriptive statistics

#Variables :
#5: Depth
#6: Table
#7: Price
#2 Quality


dataDiamonds <- diamonds [,c(2,5,6,7)] #Subsetting the dattaset

nv <- naiveBayes(cut ~ ., data = dataDiamonds) #Model

predict(nv,dataDiamonds[1:4,-1])

predict(nv,dataDiamonds[1:4,-1],type = "raw")

pred <- predict(nv,dataDiamonds[,-1]) #Predicting the model

table(pred, dataDiamonds$cut) #Result