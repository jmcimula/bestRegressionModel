#Multidimensional scaling (MDS) is a means of visualizing the level of similarity of individual cases of a dataset. It refers to a set of related ordination techniques used in information visualization, in particular to display the information contained in a distance matrix. An MDS algorithm aims to place each object in N-dimensional space such that the between-object distances are preserved as well as possible. Each object is then assigned coordinates in each of the N dimensions. The number of dimensions of an MDS plot N can exceed 2 and is specified a priori. Choosing N=2 optimizes the object locations for a two-dimensional scatterplot.

#Loading data

setwd('~/R/DynamicRegressionModel/')
msleep <- read.table(file = "DataAuto.txt",sep="\t",header=TRUE,dec=".",row.names=1)

#Renaming the columns
colnames(msleep) <- c("Price","Cylinder","Power","Weight","Consumption")


#Euclidean distances between the rows
euDist <- dist(msleep)

#Multidimensional Scaling from base package
MDS <- cmdscale( euDist,eig = TRUE, k = 2 ) #k is the number of dim


#Getting each dimension in its variable 
dimOne  <- MDS$points[,1]
dimTwo  <- MDS$points[,2]


par(mar=c(1,1,1,1))

plot(dimOne, dimTwo, xlab = "Coordinate 1", ylab = "Coordinate 2", main = "Metric	MDS", type = "n")
text(dimOne, dimTwo, labels = row.names(mydata), cex = .7)


#In the plot, we can see that Mercedes and Ferrari are probably outliers in this dataset
