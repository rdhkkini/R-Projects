# CANCER DETECTION USING K- means Cluster

healthyCellData   <- read.csv("healthy.csv", header = FALSE) # Read the file

# Convert into a Matrix file
healthyCellMatrix <- as.matrix(healthyCellData)
str(healthyCellMatrix) # LARGE DATA SET

# Display the image in grey-scale
image(healthyCellMatrix, 
      axes = FALSE, 
      col = grey(seq(0,1,length = 256)
                 )
      )

# Convert into a vector to use for the cluster 
healthyCellVector <- as.vector(healthyCellMatrix)

# Distance method to compute the distance between the data points
distanceVector <- dist(healthyCellVector, method = 'euclidian')
# Error displayed due to a lage data set being processed
str(healthyCellVector)

# Error reason Computation
n <- length(healthyCellVector) # n = 365636
distNumber <- n* (n-1)/2
distNumber                     # distNumber = 66844659430
# distNumber is too large value to compute a hclust

# Method used : K means clustering 
set.seed(1)
k <- 10

kclust <- kmeans(healthyCellVector, centers = k, iter.max = 10000)
str(kclust)

healthyCellCluster <- kclust$cluster
kclust$centers[2]

dim(healthyCellCluster) <- c(nrow(healthyCellMatrix), ncol(healthyCellMatrix))
image(healthyCellCluster, axes = FALSE, col = grey(seq(0,1, length = 256)))

image(healthyCellCluster, axes = FALSE, col = rainbow(k))


#========================================================================
# Tumor data Set
#========================================================================

tumorDataSet <- read.csv('tumor.csv', header = FALSE)
tumorMatrix  <- as.matrix(tumorDataSet)
tumorVector  <- as.vector(tumorMatrix)


install.packages('flexclust')

library(flexclust) # kcc centroid

kclust.kcc <- as.kcca(kclust, healthyVector)

tumorCluster <- predict(kclust.kcc, newdata = tumorVector)

dim(tumorCluster) <- c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorCluster, axes = FALSE, col = rainbow(k))
image(tumorCluster, axes = FALSE, col = grey(seq(0,1, length = 256)))
