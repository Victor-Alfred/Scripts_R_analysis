# load these libraries, or install them if not already installed

library(NbClust)
library(factoextra)
library(cluster)
library(fpc)

# Replace tracks_summary with your dataset
df <- scale(tracks_summary[2:6])
df[is.na(df)] <- 0

pca_res <- prcomp(as.matrix(df), center = TRUE, scale. = TRUE)
summary(pca_res)

# Determine optimum number of clusters, and visualise using the NbCLust package

set.seed(123)
nb <- NbClust(df, distance = "euclidean", min.nc = 2, 
              max.nc = 10, method = "complete", index ="all")

fviz_nbclust(nb) + theme_minimal()

barplot(table(nb$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Parameters",
        main="Optimisation of Cluster Number")


##########

# 0R the elbow-method to decide cluster number
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(df)

#######################################################################

# DO k-means clustering

set.seed(123)
km.res <- kmeans(df, 3, nstart = 25) # decide number of clusters after running NbClust
# k-means group number of each observation

km.res$size  # visualise cluster size

km.res$centers  # view center values of clusters

# Visualize k-means clusters
fviz_cluster(km.res, data = df, geom = "point", stand = FALSE, 
             ellipse.type = "norm") + theme_bw() + theme(panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank())
# retrieve dataframe from ecah cluster

data_clus_1 <- tracks_summary[km.res$cluster == 1,]
data_clus_2 <- tracks_summary[km.res$cluster == 2,]
data_clus_3 <- tracks_summary[km.res$cluster == 3,]
