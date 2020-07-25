## loading libraries

library(randomForest)
library(caret)

### importing the data
data<- `Company_Data.(1)`
summary(data)
str(data)

## getting a histogram for sales

hist(data$Sales, main = "sales of company_data", col = c("blue","red","pink"), xlim = c(0,20))

### getting sales as an categorical data
highsales= ifelse(data$Sales<9, "No","Yes")
CD= data.frame(data[2:11], highsales)
str(CD)

table(CD$highsales)

## data partition

set.seed(123)
ind<- sample(2, nrow(CD), replace = TRUE, prob = c(0.7, 0.3))
train<- CD[ind==1,]
test<- CD[ind==2,]

### creating the model

set.seed(222)
?set.seed

rf<- randomForest(highsales~., data = CD) ## OOB error rate is 14.75%, ntree= 500
rf
plot(rf)
rf$importance ### the impact of Price and Shelf life seems to be a more than any other variable

## predicting with the train data
p1<- predict(rf, train)
print(p1)
summary(p1)

## cofusion matrix
confusionMatrix(p1, train$highsales) ### the accuracy seems to be 1

### prediction on test data

p2<- predict(rf, test)
print(p2)
summary(p2)
plot(p2)

## confusion matrix on the test data

confusionMatrix(p2, test$highsales) ### the accuracy rate is really high at 1


### tuning the random forest
?tuneRF
train_2<- train[,-1]
train_2
train<- as.data.frame(train)
train_2
t <-tuneRF(train[,-1], train[,1],stepFactor = 0.50, plot = TRUE, ntreeTry = 300, improve = 0.10, trace = TRUE, doBest = TRUE)   ### this thows that the data is using all rows and we remove 22nd column
### this shows as the Mtry increases from 1 to 6 the OOB reduces 


#### Building another model with optimum Mtry
rf1<- randomForest(highsales~., data= train, ntree= 300,mtry= 6, importance= TRUE, proximity= TRUE)
rf1  ### the OOB error rate is 17.19 % which is quite high

pred_1<-predict(rf1,train)
confusionMatrix(pred_1, train$highsales) ### accuracy is 100 percent that is becuase the data is known to the model

###prediction with the test data
pred_2<- predict(rf1, test)
confusionMatrix(pred_2, test$highsales)  ### the prediction rate here is 83%


### no of nodes in each tree

summary(treesize(rf1))
hist(treesize(rf1), col = "blue", main = "no of nodes in a tree")
## majority of the tree have an average of have 45 to 55 nodes

### variable immportance
varImpPlot(rf1)

### mean decrease accuracy plot shows how bad would the model perform if the variables are removed
## this shows that the best performing variable is shelf life and the worst is urban 

### mean decrease gini plot shows how much will the gini decreases if one of the variables are removed
## price is the most important one

importance(rf1) ## same as the plot just the quantitative values

?partialPlot
partialPlot(rf1,train, Price)
### this shows the dependence of sales on price, if the price is above 150 the sales would average out

### multidimesional plot

MDSplot(rf1,CD$highsales)
