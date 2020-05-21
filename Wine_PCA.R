##loading Library
library(factoextra)
library(cluster)
mydata<-wine
View(mydata)

## loasing data without column 1
View(mydata[-1])
data <- mydata[,-1]
attach(data)

##corelation betweeen the data
cor(data)

### finding the PCA
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
str(pcaObj)
summary(pcaObj)
loadings(pcaObj)

##plotting PCA
plot(pcaObj)

plot(pcaObj$scores[,1:2], col= " Blue", pch = 20, cex= 0.3, lwd = 3)
biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")

####Top 3 PCA Scores which represents the whole data
pcaObj$scores[,1:3]


# Considering top 3 principal component scores and binding them with mydata
mydata<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata)

# Hierarchial Clustering
# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,8:10]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") ## method for finding distance in centres

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1)            
rect.hclust(fit1, k=7, border="red")


groups<-cutree(fit1,7) # Cutting the dendrogram for 7 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)


### binding the membership with the original data
final1<-cbind(membership_1,mydata)
View(final1)

View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
write.csv(final1,file="wine_cluster.csv",row.names = F,col.names = F)

### getting working directory
getwd()


# K-Means Clustering :
library(plyr)

mydata <- wine_cluster
str(mydata)

View(mydata)

### nomralise the comp 1, comp2 and 3
normalized_data<-scale(mydata[,15:17])

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

### creating a cluster plot###
fit <- eclust(normalized_data, "kmeans", k = 4, nstart = 25, graph = FALSE) # 7 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")


final2<- data.frame(fit$cluster,mydata) # append cluster membership
View(final2)
aggregate(mydata[,2:17], by=list(fit$cluster), FUN=mean)


table(fit$cluster)
