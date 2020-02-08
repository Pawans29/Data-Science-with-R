University_new_data<- Universities
University_new<- scale(University_new_data[,2:7])
University_new
d<- dist(University_new, method = "euclidean")
d
fit<- hclust(d, method = "average")
plot(fit)
groups<- cutree(fit, k=4)
rect.hclust(fit,k = 4,  border = "Red")
clusters= data.frame("University_new_data"= University_new_data[,1], 'cluster'=groups)
