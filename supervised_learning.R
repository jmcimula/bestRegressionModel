#Reviewing ERIC
#clear the memory
rm(list=ls())

#launch the rpart package
library(rpart)

#*****************************************************************************
#***************** Procedures and functions **********************************
#*****************************************************************************

#classification based on misclassification cost matrix
#modele is a decision tree produced by rpart
#mat_cout is the misclassification cost matrix
#donnees is the test sample
affectation <- function(modele,mat_cout,donnees){

  #multiplication between the vector of pesterior probabilities
  #and one column of the misclassification cost matrix
  produit <- function(x,p){
    #sum of the product
    return(sum(p*x))
  }

  #classification of one example 
  #based on a misclassification cost
  #and the posterior probabilities
  prediction <- function(p,mc){
    #compution the expected misclassification cost for each class
    caff <- apply(mc,2,produit,p)
    #search the one which minimizes the cost
    return(which.min(caff))
  }
    
  #predict gives the posterior probabilities with this "type" parameter
  pred <- predict(modele,type="prob", newdata = donnees)  
  
  #for each example, use the prediction based on the minimization of the expected cost
  ychapeau <- apply(pred,1,prediction,mc=mat_cout)
  
  #send the prediction
  return(ychapeau)
}

#bagging on trees
bagging_rpart <- function(y, formule, donnees, mat_cout, repetition){

  #set the seed for the random value generation
  set.seed(15)
 
  #the models are stored in a list
  tous <- NULL
  
  #vector of costs
  cout <- NULL
  
  #number of examples
  n <- nrow(donnees)
  
  #uniform weights
  poids <- rep(1/n,n)
  
  #parameters of rpart
  parametres <- rpart.control(xval=0,cp=0,maxcompete=0,maxsurrogate=0)

  #repeat the learning process
  for (k in 1:repetition){
  
    #get a sample of the dataset (with replacement)
    echantillon <- sample(1:n,n,T,poids) 
    subsample <- donnees[echantillon,]
    
    #learning of the tree
    modele <- rpart(formula = formule, data = subsample, method = "class", control = parametres)
    
    #store the model into a list
    tous[[k]] <- modele
    
    #classification of the examples of the learning set
    ychapeau <- affectation(modele,mat_cout,donnees)
       
    #computation of costs
    mc <- table(y,ychapeau)
    cout[k] <- sum(mc*mat_cout)
    
  }  
  #return the costs and the models for each iteration
  return(list(couts=cout,modeles=tous))
}

#classification using a simple vote process
#from a list of classifiers
#the misclassification cost matrix is used for each model
bagging_affectation <- function(liste,mat_cout,donnees){

  #compute the distribution of the classification for each example
  #return the index of the most occured one
  pred_on_vote <- function(x){
    frequence <- table(x)
    return(as.integer(names(which.max(frequence))))
  }

  #get a matrix with the prediction of each classifier
  allpred <- sapply(liste,affectation,mat_cout,donnees)
  
  #use a simple vote for the prediction
  ychapeau <- apply(allpred,1,pred_on_vote)
  
  #return the prediction
  return(ychapeau)
  
}

#*********************************************************************
#***************** MAIN PROGRAM **************************************
#*********************************************************************

#set the misclassification cost matrix
cost.matrix <- matrix(c(-3.0,1.0,1.0,1.0,-6.0,1.0,0.0,0.0,0.0),nrow=3,ncol=3)
print(cost.matrix)

#launch the whole data file
#Importing data
setwd("~/R/bestRegressionModel/")
cup <- read.table(file="Data_Supervised_Learning.txt",header=T,sep="\t",dec=".")

#subdivide the dataset into learning set and test set
cup.train <- cup[cup$ID=="train",2:length(cup)]
cup.test <- cup[cup$ID=="test",2:length(cup)]           

#******************************
#Compute and test a single tree
#******************************

#settings of rpart
parametres <- rpart.control(xval=0,cp=0,maxcompete=0,maxsurrogate=0)

#learn a tree
std.modele <- rpart(COUPON ~ ., data = cup.train, method="class", control = parametres)

#classification without the misclassification cost matrix
std.ypred <- predict(std.modele,newdata = cup.test,type="class")
std.m <- table(cup.test$COUPON,std.ypred)
print(std.m) #print the confusion matrix
print(-sum(std.m*cost.matrix)) #compute the profit (i.e. -1.0 * cost)

#classification with the misclassification cost matrix
cost.std.ypred <- affectation(std.modele,mat_cout=cost.matrix,donnees=cup.test)
cost.std.m <- table(cup.test$COUPON,cost.std.ypred)
print(cost.std.m)
print(-sum(cost.std.m*cost.matrix))

#********************************
#Compute and test a bag of trees
#********************************

#bagging on rpart - repetition is the number of replications
classifieurs <- bagging_rpart(cup.train$COUPON, COUPON ~ ., cup.train, cost.matrix, repetition = 25)

#display the resubstitution profit for each tree
print(-classifieurs$couts)

#classification using a simple voting scheme
bag.ypred <- bagging_affectation(classifieurs$modeles, cost.matrix, cup.test)
bag.m <- table(cup.test$COUPON,bag.ypred)
print(bag.m)
print(-sum(bag.m*cost.matrix))

