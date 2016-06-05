#Bootstrapping is a nonparametric approach to statistical inference that substitutes computation for more traditional distributional assumptions and asymptotic results.
#Bootstrapping offers a number of advantages:

#(1)The bootstrap is quite general, although there are some cases in which it fails.
#(2) Because it does not require distributional assumptions (such as normally distributed errors),the bootstrap can provide more accurate inferences when the data are not well behaved or when the sample size is small.
#(3) It is possible to apply the bootstrap to statistics with sampling distributions that are difficult to derive, even asymptotically.
#(4) It is relatively simple to apply the bootstrap to complex data-collection plans (such as stratified and clustered samples).

library(boot)

data("diamonds") #Loading economics dataset

summary(diamonds) #Descriptive statistics

#Variables :
#5: Depth
#6: Table
#7: Price
#2 Quality

dataDiamonds <- diamonds [,c(2,5,6,7)] #Subsetting the dattaset

colnames(dataDiamonds)[1] <- "Quality"

# Bootstrap 95% CI for R-Squared

# function to obtain R-Squared from the data 
rSquared  <- function(formula, data, indices) {
  samp <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data = samp)
  return(summary(fit)$r.square)
}

# bootstrapping with 1000 replications 
bootResult <- boot(data = dataDiamonds, statistic = rSquared, R = 5000, formula = price ~ depth + table)

#The result
bootResult

plot (bootResult) #General plot
plot(bootResult, index = 1) # intercept 
plot(bootResult, index = 2) # depth 
plot(bootResult, index = 3) # table 

#Get 95% confidence interval 
boot.ci(bootResult, type = "bca")#General

boot.ci(bootResult, type = "bca", index = 1) # intercept 
boot.ci(bootResult, type = "bca", index = 2) # depth
boot.ci(bootResult, type = "bca", index = 3) # table
