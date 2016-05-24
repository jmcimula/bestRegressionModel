library(DAAG)
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
			
		#Assembling diagnostic parameters per model predictor in Matrix of all combinations
		dFrame <- data.frame(modelReg = mdRegComb,RSquared = RSqrt,AdjustedRSquared = AdjRSqrt,AIC = AIC,BIC = BIC,PRESS = PL)
		
		#Loading data frame
		dynamicRegression <- rbind(dynamicRegression,dFrame)
}
   dynamicRegression <- as.data.frame(dynamicRegression) 
   #getBestRModel(dynamicRegression)  #Call the function getBestRModel
   return (View(dynamicRegression))#Return

}#End function

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
