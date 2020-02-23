iris
acc<- c()
for (i in 1:1000)
{
  print(i)
intraininglocal <- createDataPartition(iris$Species, p = 0.70, list = F)
training1 <- iris[intraininglocal,]
testing<- iris[-intraininglocal, ]
fitree<- C5.0(training1$Species~., data = training1)
summary(fitree)
pred<- predict.C5.0(fitree, testing[,-5])
a<- table (testing$Species, pred)
a
acc <- c(acc,sum(diag(a))/sum(a))
}
acc
summary(acc)

