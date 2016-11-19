# MOVIE RECOMMENDATION SYSTEM
# DataSet : http://files.grouplens.org/datasets/movielens/ml-100k/u.item

movieDataFiles <- read.table("movieList.txt", header = FALSE, sep = "|", quote = "\"")
str(movieDataFiles)

colnames(movieDataFiles) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", 
                              "IMDB", "Unknown", "Action", "Adventure", 
                              "Animation", "Childrens", "Comedy", "Crime", 
                              "Documentary", "Drama", "Fantasy", "FilmNoir", 
                              "Horror", "Musical", "Mystery", "Romance", "SciFi", 
                              "Thriller", "War", "Western")

str(movieDataFiles)

# Remove the unnecessary columns
movieDataFiles$ID = NULL
movieDataFiles$ReleaseDate = NULL
movieDataFiles$VideoReleaseDate = NULL
movieDataFiles$IMDB = NULL

movieDataFiles <- unique(movieDataFiles)

str(movieDataFiles)

# Heirarchical Clustering
# Distance between the data Points
distanceBetweenMovie <-  dist(movieDataFiles[2:20], method = "euclidean")

# Hierarchical clustering

movieDataFilesCluster = hclust(distanceBetweenMovie, method = "ward.D") 
plot(movieDataFilesCluster)

movieDataFilesClusterGroup = cutree(movieDataFilesCluster, k = 8)
moviesMatrix <- matrix(moviesMatrix)
movieDataFilesClusterGroup

moviesMatrix$Action         <- tapply(movieDataFiles$Action, movieDataFilesClusterGroup, mean)
moviesMatrix$Adventure      <- tapply(movieDataFiles$Adventure, movieDataFilesClusterGroup, mean)
moviesMatrix$Childrens      <- tapply(movieDataFiles$Childrens, movieDataFilesClusterGroup, mean)
moviesMatrix$Comedy         <- tapply(movieDataFiles$Comedy, movieDataFilesClusterGroup, mean)
moviesMatrix$Crime          <- tapply(movieDataFiles$Crime, movieDataFilesClusterGroup, mean)
moviesMatrix$Documentary    <- tapply(movieDataFiles$Documentary, movieDataFilesClusterGroup, mean)
moviesMatrix$Drama          <- tapply(movieDataFiles$Drama, movieDataFilesClusterGroup, mean)
moviesMatrix$Fantacy        <- tapply(movieDataFiles$Fantasy, movieDataFilesClusterGroup, mean)
moviesMatrix$FilmNoir       <- tapply(movieDataFiles$FilmNoir, movieDataFilesClusterGroup, mean)
moviesMatrix$Horror         <- tapply(movieDataFiles$Horror, movieDataFilesClusterGroup, mean)
moviesMatrix$Musical        <- tapply(movieDataFiles$Musical, movieDataFilesClusterGroup, mean)
moviesMatrix$Mystery        <- tapply(movieDataFiles$Mystery, movieDataFilesClusterGroup, mean)
moviesMatrix$ROmance        <- tapply(movieDataFiles$Romance, movieDataFilesClusterGroup, mean)
moviesMatrix$SciFi          <- tapply(movieDataFiles$SciFi, movieDataFilesClusterGroup, mean)
moviesMatrix$Thriller       <- tapply(movieDataFiles$Thriller, movieDataFilesClusterGroup, mean)
moviesMatrix$War            <- tapply(movieDataFiles$War, movieDataFilesClusterGroup, mean)
moviesMatrix$Western        <- tapply(movieDataFiles$Western, movieDataFilesClusterGroup, mean)

moviesMatrix

subset(movieDataFiles, Title == "Men in Black (1997)")
movieDataFilesClusterGroup[257]
cluster2 <- subset(movieDataFiles, movieDataFilesClusterGroup ==8)
cluster2$Title

library(ggplot2)
scatterplot <- ggplot(movieDataFiles, aes(x= movieDataFiles$Title, y = movieDataFilesClusterGroup ))+
               geom_smooth()                      
scatterplot
