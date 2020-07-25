###libraraies
library(randomForest)
library(caret)
library(MASS)
## loading the data
Fraud_check<- read.csv(file.choose())
str(Fraud_check)

## making a histogram for taxable income
hist(Fraud_check$Taxable.Income)

hist(Fraud_check$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(20,40,60)), col = c("blue","red", "green","violet"))

Risky_Good = ifelse(Fraud_check$Taxable.Income<= 30000, "Risky", "Good")     
Fc_temp<- data.frame(Fraud_check, Risky_Good)
FC = Fc_temp[,c(1:7)]
str(FC)
table(FC$Risky_Good)

### partition the data
set.seed(123)
ind<- sample(2, nrow(FC), replace = TRUE, prob = c(0.7, 0.3))
train <- FC[ind==1,]             
test<- FC[ind==2,]

## creating a RF model

rf<- randomForest(Risky_Good~., data = train)
summary(rf)
rf### OOB error rate is about 0.23%
attributes(rf)
rf$importance

### using the predict on train data
pred<- predict(rf, train)
head(pred)
head(train$Risky_Good) 

confusionMatrix(pred, train$Risky_Good) ### the accurcay rate is 100%

### prediction with test data
pred_1<- predict(rf, test)
confusionMatrix(pred_1, test$Risky_Good) ### even here the accuracy is 100%

### plotting the random forest

plot(rf) ### after like 250 trees there seems to be a straight line

## tuning the random forest model
train[,-6]
tune<- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 250, trace = TRUE, improve = 0.05)
## it seems the OOB is the least when the mtry is 2

### creating a new model with new split, mtry
rf1<- randomForest(Risky_Good~., ntree= 250, mtry = 2, importance= TRUE, proximity = TRUE, data =train)
rf1


## new rf model on the train data
pred_2<- predict(rf1, train)
confusionMatrix(pred_2, train$Risky_Good) ### accuracy is 100 percent for the train data
### predicting on test data
pred_test_rf1<predict(rf1, test)
confusionMatrix(pred_test_rf1, test$Risky_Good) ## accuracy is 100 percent

### no of nodes of trees
hist(treesize(rf1),main = "Nodes on the trees", col = "green")
### it shows that majority of the trees have 80 nodes or more


### variance plot

varImpPlot(rf1) ### mean decrease plot shows how would the mean decraese in case of each variable is taken off, this shows that
## taxable income is the most important variable and martial status and city population are not that significant

### mean decrease gini plot shows how will the average gini decrease if one the variable is taken off, this shows that taxable income is the most implrtant variable
## and urban and undergard are not that significant

# sorting the variance plot for top 5

varImpPlot(rf1, n.var =5, sort = T, main= " Top 5 - Variable Importance" )

## quantitaive values
rf1$importance

## partial dependence plot 

partialPlot(x = rf1, train, Taxable.Income)
### th eplot shows that if the taxable income of a person is more than 30000 then he is a good customer or else he is a risky customer
