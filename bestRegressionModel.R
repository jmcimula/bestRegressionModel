#Loading libraries
library(DAAG)
library(MASS)
library(dplyr)
library(stringr)
library(glmnet)#( Ridge + Elastic Net) Regression
library(lars)#Lasso Regression


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
		nbResp <- 0
		nbExp  <- 0
		Ridge  <- 0
		Lasso  <- 0 
		ElasticNet <- 0
		if (str_detect(mdRegComb,'[+]') == TRUE){

			delim   <- unlist(gregexpr(pattern ='[~]',mdRegComb)) - 1 #Checking the tilde in the model 
			stPart  <- substr(mdRegComb,1,delim) #Retrieving the part before the tilde with the response variable
			stPart  <- str_trim(stPart) #Trim the result retrieved  			
			sndPart <- substr(mdRegComb,delim + 2, str_length(mdRegComb)) #Retrieving the second part with the exploratory variables
			
			nbResp <- getNumberResponse (inputUnit,stPart) #Column of Response variable
			nbExp  <- getNumberExploratory (inputUnit,sndPart)#Columns of Exploratory variables
			Ridge  <- getRidgeValue(inputUnit,nbResp,nbExp)
			Lasso  <- getLassoValue(inputUnit,nbResp,nbExp)
            ElasticNet <- getElasticNetValue(inputUnit,nbResp,nbExp)
		}
		#Assembling diagnostic parameters per model predictor in Matrix of all combinations
		dFrame <- data.frame(modelReg = mdRegComb,RSquared = RSqrt,AdjustedRSquared = AdjRSqrt,AIC = AIC,BIC = BIC,PRESS = PL,Ridge = Ridge,Lasso = Lasso, ElasticNet = ElasticNet)#, nbResp = nbResp, nbExp = nbExp)#, Accuracy = RidgeRegression)
		
		#Loading data frame
		dynamicRegression <- rbind(dynamicRegression,dFrame)
}
   dynamicRegression <- as.data.frame(dynamicRegression) 
   #getBestRModel(dynamicRegression)  #Call the function getBestRModel
   return (View(dynamicRegression))#Return

}#End function

getNumberResponse <- function (dataframe, model){
#Retrieving the number of the column of the response variable
   colData <- 0
   for (j in 1:dim(dataframe)[2]){
                   #Take one by one chosen exploratory variables
                   if (names(dataframe)[j] == str_trim(model)){
                       colData <- j
                     break;
                   }
		    colData <- colData
    }#End-for
	respVarColumn <- colData #Response variable number in the dataframe
	#print(respVarColumn)
	return(respVarColumn)
}#End function

getNumberExploratory <- function (dataframe, model){

   ChPlusLength <- length(unlist(gregexpr(pattern = "[+]",model))) + 1 #The Length of the character + in the string augmented of 1   
   chPlus <- str_split_fixed(model,"[+]",n = ChPlusLength) #Creating a table of string 
   
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
   return(expVarColumns)#Return
}#End function

getRidgeValue <- function(dataframe,nbResp,nbExp){
	    #Putting the exploratory in the vector
        nbExp <- substr(nbExp,2,str_length(nbExp))
		#Taking the element of the pair one by one
		nbExp <- str_split_fixed (nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )
		
		dFrame <- data.frame(row.names=1:nrow(dataframe))
        n <- 0		
		for (i in 1 : length(nbExp)){
		    
			n <- as.integer(nbExp[i])
			dataCol <- dataframe[n]
		    dFrame  <- cbind(dFrame,dataCol)
		}#End-for
		dFrame <- as.data.frame(dFrame)
        x <- as.matrix(dFrame)
	    y <- as.matrix(dataframe[nbResp])
	    #fit model
        fit <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
        #summarize the fit
	    summary(fit)
		# make predictions
		predictions <- predict(fit, x, type="link")
		# summarize accuracy
		rmse <- mean((y - predictions)^2)

		#return
		return (rmse)		
}#End-function 

getLassoValue <- function(dataframe,nbResp,nbExp){

        #Putting the exploratory in the vector
        nbExp <- substr(nbExp,2,str_length(nbExp))
		#Taking the element of the pair one by one
		nbExp <- str_split_fixed (nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )
		
		dFrame <- data.frame(row.names=1:nrow(dataframe))
        n <- 0		
		for (i in 1 : length(nbExp)){
		    
			n <- as.integer(nbExp[i])
			dataCol <- dataframe[n]
		    dFrame  <- cbind(dFrame,dataCol)
		}#End-for
		dFrame <- as.data.frame(dFrame)
        x <- as.matrix(dFrame)
	    y <- as.matrix(dataframe[nbResp])
	    #fit model
        fit <- lars(x, y, type="lasso")
        #summarize the fit
		summary(fit)
		# select a step with a minimum error
		best_step <- fit$df[which.min(fit$RSS)]
		# make predictions
		predictions <- predict(fit, x, s=best_step, type="fit")$fit
		# summarize accuracy
		rmse <- mean((y - predictions)^2)
		
		#return
		return (rmse)
}#End-function 

getElasticNetValue <- function(dataframe,nbResp,nbExp){
        #Putting the exploratory in the vector
        nbExp <- substr(nbExp,2,str_length(nbExp))
		#Taking the element of the pair one by one
		nbExp <- str_split_fixed (nbExp,",", n = length(unlist(gregexpr(pattern = ",",nbExp))) + 1 )
		
		dFrame <- data.frame(row.names=1:nrow(dataframe))
        n <- 0		
		for (i in 1 : length(nbExp)){
		    
			n <- as.integer(nbExp[i])
			dataCol <- dataframe[n]
		    dFrame  <- cbind(dFrame,dataCol)
		}#End-for
		dFrame <- as.data.frame(dFrame)
        x <- as.matrix(dFrame)
	    y <- as.matrix(dataframe[nbResp])
	    #fit model
        fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
        #summarize the fit
	    summary(fit)
	    #make predictions
	    predictions <- predict(fit, x, type = "link")
	    #summarize accuracy
	    rmse <- mean((y - predictions)^2)
		
		#return
		return (rmse)
}#End-function 

