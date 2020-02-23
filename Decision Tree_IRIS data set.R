data("iris")
iris
install.packages("caret")
library(caret)
library(C50)
intraining<-createDataPartition(iris$Species,p=.70, list= FALSE)
trainig<- iris[intraining,]
testing<- iris[-intraining,]
model<- C5.0(trainig$Species~., data = trainig)
summary(model)
pred<- predict.C5.0(model,testing[,-5])
a<- table(testing$Species,pred)
sum(diag(a))/sum(a)
plot(model)
a
