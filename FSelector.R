#clear the memory
rm (list=ls())

#load the dataset
setwd("~/R/bestRegressionModel/")
vote.data <- read.table(file="vote_for_feature_selection.txt",header=T, sep="\t")

#loading the package
library(FSelector)

#*********************************
#ranking - Symmetrical uncertainty
#get the weight for each predictors
vote.ranking <- symmetrical.uncertainty(group ~ ., data = vote.data)
#sorting the result according the weight
index <- order(vote.ranking[[1]],decreasing=T)
vote.sorted <- vote.ranking[index,]
names(vote.sorted) <- rownames(vote.ranking)[index]
print(head(vote.sorted,10))

#*************
#cfs selection
vote.cfs <- cfs(group ~ ., data = vote.data)
print(vote.cfs)
