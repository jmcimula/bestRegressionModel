#Loading libraries
library(DAAG)
library(MASS)
library(dplyr)
library(stringr)
library(glmnet)# (Lasso + Ridge + Elastic Net) Regression

getDFResponse <- function (dataframe, response){

inputUnit <- dataframe
response  <- response
#attach(inputUnit)

#(1) Number of colums of the data frame
getLen <- dim(inputUnit)[2]

for (i in 1:getLen){
        #Removing the response variable in the temporary data frame
        if (names(inputUnit)[i] == response){
		        expLayer <- inputUnit [,-i]
				break;
        }#if-condition
}#Loop for

#(2)Number of colums of the temporary data frame of explanatory variables
getLenExpVar <- dim(expLayer)[2]

#(3)Initializing a Matrix which will contain all combinations of predictor model
ExpVarMatrix <- matrix( ncol = getLenExpVar)

#Creating combination
for (i in 1:getLenExpVar){
        #Combination function
		comb    <- t(combn(names(expLayer),i))
		numbRow <- nrow(comb)
		numbCol <- length(names(expLayer))
		numbRowNA  <- numbRow
		numbColNA  <- numbCol-ncol(comb)
		naMatr   <- matrix(rep(NA, numbRowNA*numbColNA), nrow = numbRowNA, ncol = numbColNA)
		result   <- cbind(comb, naMatr)
		ExpVarMatrix <- rbind(ExpVarMatrix, result)
}#Loop for
#Removing all NA
ExpVarMatrix <- ExpVarMatrix[-1,]

#Final result of combination between response and explanatory variables
#Setting an empty data frame
dynamicRegression <- data.frame()
for (i in 1:nrow(ExpVarMatrix)){

        getVal <- na.omit (ExpVarMatrix[i, ])
		mdRegComb <- paste (response, " ~ ", paste (getVal, collapse = " + "), sep = "")
		#print(mdRegComb)
        mdLM <- lm(as.formula(mdRegComb),data=inputUnit)
		
		#Evaluating based on goodness a fit	
		#Diagnostic parameters
        SMry <- summary(mdLM)		
        RSqrt  <- SMry[8]   #R-Squared
        AdjRSqrt <- SMry[9] #adj R-Squared
        AIC  <- AIC(mdLM)#AIC
        BIC  <- BIC(mdLM)#BIC
		
		#Evaluating based on prediction
		PL  <- DAAG::press(mdLM)
		
		#Ridge Regression
		if (str_detect(mdRegComb,'[+]') == TRUE){
		
		trainingData  <- sample_frac(inputUnit, 0.75) #Original dataframe
        testingData   <- setdiff(inputUnit, trainingData)
	    #ridge     <- lm.ridge (as.formula(mdRegComb),data=trainingData) #Ridge Linear
		#print(testingData)
		#predicted <- predict(ridge,testingData)  #Predict on test data
		#comp      <- cbind (actual=testingData$response, predicted) #Combine	
		#RidgeRegression <- mean (apply(comp, 1, min)/apply(comp, 1, max))
		print(mdRegComb)
		
		    #RidgeRegression <- 1
		    getRidgeValue (inputUnit,mdRegComb)
		}else{
		
		    RidgeRegression <- 0
		  
		}

		#RidgeRegression = mean (apply(compare, 1, min)/apply(compare, 1, max))
		
		#Assembling diagnostic parameters per model predictor in Matrix of all combinations
		dFrame <- data.frame(modelReg = mdRegComb,RSquared = RSqrt,AdjustedRSquared = AdjRSqrt,AIC = AIC,BIC = BIC,PRESS = PL)#, Accuracy = RidgeRegression)
		
		#Loading data frame
		dynamicRegression <- rbind(dynamicRegression,dFrame)
}
   dynamicRegression <- as.data.frame(dynamicRegression) 
   #getBestRModel(dynamicRegression)  #Call the function getBestRModel
   return (View(dynamicRegression))#Return

}#End function


getRidgeValue <- function(dataframe, model){

   delim <- unlist(gregexpr(pattern ='[~]',model)) - 1 #Checking the tilde in the model   
   
   stPart <- substr(model,1,delim) #Retrieving the part before the tilde with the response variable
   
   stPart <- str_trim(stPart) #Trim the result retrieved   
   
   sndPart <- substr(model,delim + 2, str_length(model)) #Retrieving the second part with the exploratory variables
   
   ChPlusLength <- length(unlist(gregexpr(pattern = "[+]",sndPart))) + 1 #The Length of the character + in the string augmented of 1   
   chPlus <- str_split_fixed(sndPart,"[+]",n = ChPlusLength) #Creating a table of string 
   
   getColData <- NULL
   #Retrieving the number of columns for all explanatory variables
   for (i in 1 : length(chPlus)){
   
        H <- str_trim(chPlus[i]) #Decomposing the exploratory variable one by one from Linear Model presentation
   
        for (j in 1:dim(dataframe)[2]){
                   #Take one by one chosen exploratory variables
                   if (names(dataframe)[j] == H){
                       colData <- j
                     break;
                   }
        }#End-for
        getColData <- paste(getColData,colData,sep=",") #Concat the number of exp variable column in the dataframe
   }#End-for
   
   expVarColumns <- getColData
   
   #Retrieving the number of the column of the response variable
   for (j in 1:dim(dataframe)[2]){
                   #Take one by one chosen exploratory variables
                   if (names(dataframe)[j] == stPart){
                       colData <- j
                     break;
                   }
		    colData <- colData
    }#End-for
	respVarColumn <- colData

	

}#End-function 

getLassoValue <- function(){

}

getElasticNetValue <- function(){

}

getBestRModel <- function (AIC, BIC, PRESS){  

#Assignement
inputUnit <- dataframe

nbCol <- ncol(inputUnit)
nbRow <- nrow(inputUnit)

for (i in 1: nbCol){
	for (j in 1: nbRow){
	
	###print(inputUnit[i][j])
	
	}
}

}#End function

#CP Criterio
CPCriterio <- function (dataframe){

inputUnit <- dataframe
n <- inputUnit[1]
p <- inputUnit[2]


}
