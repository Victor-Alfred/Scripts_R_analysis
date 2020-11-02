# select criteria/columns that should be used for clustering
df <- scale(tracks_summary[2:6])   #TIME, DISP_X, DISP_Y, DISP, ANGLE
#df <- scale(tracks_summary[c(2,5,6)])
df[is.na(df)] <- 0

pca_res <- prcomp(as.matrix(df), center = TRUE, scale. = TRUE)
summary(pca_res)


# To determine optimum number of clusters
library(NbClust)
library(factoextra)
library(cluster)
library(fpc)

set.seed(123)
nb <- NbClust(df, distance = "euclidean", min.nc = 2, 
              max.nc = 10, method = "complete", index ="all")

# nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

fviz_nbclust(nb) + theme_minimal()

barplot(table(nb$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Parameters",
        main="Optimisation of Cluster Number")

# nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

########################################################

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
km.res$cluster

km.res$size

km.res$centers

# Visualize k-means clusters
fviz_cluster(km.res, data = df, geom = "point", stand = FALSE, 
             ellipse.type = "norm") + theme_bw() + theme(panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank())

data_clus_1 <- tracks_summary[km.res$cluster == 1,]
data_clus_2 <- tracks_summary[km.res$cluster == 2,]
data_clus_3 <- tracks_summary[km.res$cluster == 3,]

mean(data_clus_1$ANGLE)
mean(data_clus_2$ANGLE)
mean(data_clus_3$ANGLE)

# to get more information about these clusters
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
# [1] 1468  596  945

table(control$GeneSymbol=="LP", fit.km$cluster)

fit.km$centers

############################

library(fpc)
library(ggplot2)
kclust=kmeans(df,centers=3)
kclust$cluster <- as.factor(kclust$cluster)
d=dist(df, method = "euclidean") 
fit=cmdscale(d,eig=TRUE, k=2) # k is the number of dim


p = ggplot(data.frame(df), aes(fit$points[,1], fit$points[,2], color =  factor(kclust$cluster))) 
p <- p + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
p <- p + theme(axis.title.x = element_text(size = rel(1.5), angle = 00))
p= p + theme(axis.text=element_text(size=16,angle=90),axis.title=element_text(size=20,face="bold")) + geom_point(size=4)
p= p + theme(legend.text = element_text(size = 14, colour = "black"))
p= p + theme(legend.title = element_text(size = 18, colour = "black"))
p



