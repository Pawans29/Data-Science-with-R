#boosting#
iris
##data partition##
intraininglocal_1<- createDataPartition(iris$Species, p = .75, list = F)
training_1<- iris[intraininglocal_1,]
testing_1<- iris[-intraininglocal_1,]
##model##
model_1<- C5.0(training_1$Species~., data = training_1, trials= 10)##number of models is trails##
##genrate summary
summary (model_1)
#predict test data##
pred_1<- predict.C5.0(model_1, testing_1[,-5])
a<- table (testing$Species, pred)
sum(diag(a))/sum(a)

##bagging and boosting
iris
acc<- c()
for (i in 1:200)
{
  print(i)
  intraininglocal <- createDataPartition(iris$Species, p = 0.80, list = F)
  training1 <- iris[intraininglocal,]
  testing<- iris[-intraininglocal, ]
  fitree<- C5.0(training1$Species~., data = training1, trials = 5)
  summary(fitree)
  pred<- predict.C5.0(fitree, testing[,-5])
  a<- table (testing$Species, pred)
  a
  acc <- c(acc,sum(diag(a))/sum(a))
}
acc
summary(acc)

  