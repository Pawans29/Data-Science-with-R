### loading of packages
library(e1071)
library(caret)
library(ggplot2)
install.packages("kernlab")
library(plyr)
library(kernlab)


## loading the data
data<-`forestfires.(1)`
data
str(data)
data$area
summary(table(data$area))

#### the area has lots of zero's

## making a histogram for the feature area
hist(data$area)
rug(data$area) ### there is a lot of concentration of zero, the data is highly skewed , so to we will use log(area+1)

### transformining the area into y
mydata<- mutate(data, y = log(area+1))
hist(mydata$y)

summary(mydata)

### Prediction of Forest fires requires only prediction from 
### temperature, rain, relative humidity and wind speed
### we have to normalise the data

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}    ###### subtract the min value in x and divide by the range of values in x.

mydata$temp<- normalize(mydata$temp)
mydata$wind<- normalize(mydata$wind)
mydata$RH<- normalize(mydata$RH)
mydata$rain<- normalize(mydata$rain)

### presuming that we are intrested in knowing the weather condition that gives rise to small fire (<5 hectre, assuming 5 hectre
## for this problem

sum(mydata$area<5) #### 366 small fires
sum(mydata$area >= 5) ### 151 not so small fires


mydata$size <- NULL
mydata$size <- factor(ifelse(mydata$area >5, 1, 0),
                      labels = c("small", "large"))

table(mydata$size)

### splitting the data into test and training to check the various kernels

set.seed(123)
ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.7,0.3))
mydata_train <- mydata[ind==1,]
mydata_test  <- mydata[ind==2,]

### building a model using the polynomial
m.poly <- ksvm(size ~ temp + RH + wind + rain,
               data = mydata_train,
               kernel = "polydot", C = 1)
               

m.poly ### error rate is 25% number of suppoet vectors is 256

### predictting using this polynomial as kernel

Area_pred_1 <- predict(m.poly, mydata_test)


### making a table to check the accuracy of it
tab_1<-table(Area_pred_1,mydata_test$size)

agreement <- Area_pred_1 == mydata_test$size
table(agreement) ### 49 wrong out of the 146.

### get accuracy
sum(diag(tab_1))/sum(tab_1) ### accuracy is just 67 %

## misclasiification
1- sum(diag(tab_1))/sum(tab_1) ## misclassification is about 32 %

## getting the mean
mean(Area_pred_1==mydata_test$size) ## mean is 67 % 


### building model using vaniladot

model1<-ksvm(size ~ temp+rain+wind+RH, 
             data= mydata_train,kernel = "vanilladot")

model1 ### error rate is 27%, number of support vectors is 214


### predictting using this vanilladot as kernel

Area_pred <- predict(model1, mydata_test)


### making a table to check the accuracy of it
tab<-table(Area_pred,mydata_test$size)

agreement <- Area_pred == mydata_test$size
table(agreement) ### 49 wrong out of the 146.

### get accuracy
sum(diag(tab))/sum(tab) ### accuracy is just 66 %

## misclasiification
1- sum(diag(tab))/sum(tab) ## misclassification is about 33 %

###using the e1071 package to check radial kernel

mymodel<- svm(size~temp + RH + wind + rain, data= mydata_train)
summary(mymodel) ### 242 support vectors
plot(mymodel, data = mydata_train,
     temp ~ RH,
     slice = list(wind=10,rain = 6))
summary(mymodel)
pred<- predict(mymodel, mydata_test)
tab_2<-table(predicted= pred, actual = mydata_test$size)
tab_2

### get accuracy
sum(diag(tab_2))/sum(tab_2) ### 67 % accuracy

## misclasiification
1- sum(diag(tab_2))/sum(tab_2) ###  32 % error

### using the linear kernel
mymodel_linear<- svm(size~temp + RH + wind + rain, data= mydata_train, kernel= "linear")
summary(mymodel_linear) ### 221 support vectors
plot(mymodel_linear, data = mydata_train,
     temp ~ RH,
     slice = list(wind=10,rain = 6))

## predicting using the linear kernel
pred<- predict(mymodel_linear, mydata_test)
tab_3<-table(predicted= pred, actual = mydata_test$size)
tab_3

### get accuracy
sum(diag(tab_3))/sum(tab_3) ### 66 % accuracy

## misclasiification
1- sum(diag(tab_3))/sum(tab_3) ###  33 % error

### matrix for all the model
SVM_kernel <- list(kernel=NULL,accuracy=NULL,error_rate = NULL)
SVM_kernel[["kernel"]] <- c("m.poly","vailladot","radial")
SVM_kernel[["accuracy"]] <- c(67,66,67)
SVM_kernel[["error_rate"]]<-c(33,34,33)
SVM_kernel




