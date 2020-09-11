## loading the packages
library(kernlab)
library(caret)
library(plyr)
library(e1071)
library(ggplot2)
library(caTools)

### loading the data train data
train_sal<- read.csv(file.choose())
str(train_sal)


### loading the test data
test_sal<- read.csv(file.choose())
str(test_sal)

##EDA

sum(is.na(train_sal))
sum(is.na(test_sal))

## adding one more feature
salary <- ifelse(train_sal$Salary== ' >50K','high','low')
salary_1 <- ifelse(test_sal$Salary== ' >50K','high','low')

train_sal <- cbind(train_sal,salary)
test_sal <- cbind(test_sal,salary_1)


## converting factor variable in train dataset into numeric

train_sal$workclass<- as.numeric(train_sal$workclass)
train_sal$education<- as.numeric(train_sal$education)

## converting factor varibale in test dataset into numeric

test_sal$workclass<- as.numeric(test_sal$workclass)
test_sal$education<- as.numeric(test_sal$education)

### dividing the data

trian_svm <- (train_sal[,c(-(5:9),-(12:14))])
test_svm <- (test_sal[,c(-(5:9),-(12:14))])

### creating a model

model <- ksvm(salary~.,data=trian_svm,kernal="vanilladot")
model

pred <- predict(model,test_svm)
confusionMatrix(table(pred,test_svm$salary_1))  ### accuracy is 81.79

## kernel = rbfdot 
model1 <- ksvm(salary~.,data=trian_svm,kernal="rbfdot")
model1

pred_1 <- predict(model1,test_svm)
confusionMatrix(table(pred_1,test_svm$salary_1))  ### accuracy 81.83


# kernal = besseldot
model2 <- ksvm(salary~.,data=trian_svm,kernal="besseldot")
model2

pred2 <- predict(model2,test_svm)
confusionMatrix(table(pred2,test_svm$salary_1)) ### accuracy 81.84

# kernel = polydot
model3 <- ksvm(salary~.,data=trian_svm,kernal="polydot")
model3

pred3 <- predict(model3,test_svm)
confusionMatrix(table(pred3,test_svm$salary_1)) #  Accuracy : 81.84

