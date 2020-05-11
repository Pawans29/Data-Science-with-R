### install packages

install.packages("cars")
install.packages("ggplot")
library(CARS)


##loading the data##
del_data<-`delivery_time.(1)`

## plotting the data
plot(del_data$Sorting.Time, del_data$Delivery.Time, main= "delivery time vs sorting time")
dt<- del_data$Delivery.Time
st<- del_data$Sorting.Time

##corelation between variables
cor(dt,st)

## there is a co-rwlation of 0.82 betweeen the variables

###boxplot##
boxplot(dt, st)

###histogram to check if the delivery time is normally distributed
hist(del_data$Delivery.Time)

###histogram to check if the sorting time data is normally distributed
hist(del_data$Sorting.Time)

## summary
summary(del_data)

## simple model##
del_mod<-lm(dt~st)
summary(del_mod) 
abline(del_mod) ### best fit line

### confidence interval
confint(del_mod,level = 0.95)



## prediction for the model

predict_1<-predict(del_mod,del_data)
predict_1
difference_1 = predict_1- del_data$Delivery.Time
difference_1
RMSE<- sqrt(mean (difference^2))
RMSE
summary(del_mod)
### RMSE value is 2.71
## R square 0.68

### log model##
del_mod_log_1<- lm(log(dt)~log(st))
summary(del_mod_log_1)#### R^2 0.77

plot(del_mod_log_1)

### confidence interval
confint(del_mod_log_1,level = 0.95)

## prediction for the model

predict_2<-predict(del_mod_log_1,del_data)
predict_2
difference_2 = predict_2- del_data$Delivery.Time
difference_2
RMSE<- sqrt(mean (difference^2))
RMSE
summary(del_mod_log_1)
####RMSE is 14.79194
####R^2 is 0.7722

### col 21 seems to be a outlier
del_data_1<- del_data[-c(5),]
nrow(del_data_1)
### trying to remove col 21 to see if the R^2 improves
## using log of st to improve it

del_mod_log<- lm(dt~log(st), data = del_data_1)
summary(del_mod_log)### R^2 is 0.69 
plot(del_mod_log)

### 21 is still shown as an outlier

## prediction for the model

predict_3<-predict(del_mod_log,del_data_1)
predict_3
difference_3 = predict_3- del_data_1$Delivery.Time
difference_3
RMSE<- sqrt(mean (difference^2))
RMSE
summary(del_mod_log_1)
####RMSE is 5.467
####R^2 is 0.7722
### no difference in the R^2 value of this model and the last one lets use lof of DT to improve it and the outlier still eixst)
## using the orginal dataset
###using log of dt to improve it
del_mod_exp<-lm(log(dt)~st, data= del_data)
summary(del_mod_exp) ## R^2 is 0.69

predict_4<-predict(del_mod_exp,del_data)
predict_4
difference_4 = predict_4- del_data$Delivery.Time
difference_4
RMSE<- sqrt(mean (difference^2))
RMSE
summary(del_mod_exp)

### RMSE value of 5.467
##R^2 value is 0.71 
### matrix for all the model
model_R_Squared_values <- list(model=NULL,R_squared=NULL,RMSE= NULL)
model_R_Squared_values[["model"]] <- c("del_mod","del_mod_log_1","del_mod_log", "del_mod_exp")
model_R_Squared_values[["R_squared"]] <- c(0.68,0.77, 0.69, 0.69)
model_R_Squared_values[["RMSE"]]<- c(2.71,14.79 ,5.46,5.46)
model_R_Squared_values

#### the model "del_mod_log_1" has the best adjusted R^2 value###


                 