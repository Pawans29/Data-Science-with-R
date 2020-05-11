###loading data##
data_1<- calories_consumed
summary(data_1)

##plotting the data##
plot(data_1$Calories.Consumed, data_1$Weight.gained..grams., main = "Calrories Consumed vs weight gained")
cc<- data_1$Calories.Consumed
wg <- data_1$Weight.gained..grams.
cor(cc, wg)

###boxplot##
boxplot(cc,wg)

###histogram to check if the calories consumed data is normally distributed
hist(data_1$Calories.Consumed)

###histogram to check if the calories consumed data is normally distributed
hist(data_1$Weight.gained..grams.)

###summary##
summary(data_1)

## correlation  beterrn the variables
cc<- data_1$Calories.Consumed
wg <- data_1$Weight.gained..grams.
cor(cc, wg)

## corealation great than 0.85 indicates strong corelation so the variables are strongly corlated

##simple model without any transformation
mod<- lm(wg~cc)
summary(mod)##r^2 value is 0.89##
abline(mod)

### confidence interval
confint(mod,level = 0.95)

## prediction for the model

predict_1<-predict(mod,data_1)
predict_1
difference = predict_1- data_1$Weight.gained..grams.
difference
RMSE<- sqrt(mean (difference^2))
RMSE
summary(mod)
### RMSE value is 141

### R square for the model is 0.89 applying transformation for better Rsquare

# Logarthmic transformation
mod_log<-lm(wg~log(cc))  # Regression using logarthmic transformation
summary(mod_log)

## confidence interaval
confint(mod_log, level= 0.95)

###predict the values

predict_2<-predict(mod_log,data_1)
predict_2
difference_2 = predict_2- data_1$Weight.gained..grams.
difference_2
RMSE_2<- sqrt(mean (difference_2^2))
RMSE_2
summary(mod)

### RMSE value is 141
## R sqaured value for the model is 0.807 
## trying another transformation for better R^2

# Exponential model 
mod_exp<-lm(log(wg)~cc) # regression using Exponential model
summary(mod_exp)

### confidence interval
confint(mod_exp, level = 0.95)

## predict the values

predict_3<-predict(mod_exp,data_1)
predict_3
difference_3 = predict_3- data_1$Weight.gained..grams.
difference_3
RMSE_3<- sqrt(mean (difference_3^2))
RMSE_3
summary(mod)

### RMSE value is 476.4
### R square has incresed to 0.877
### trying another transformation##


### matrix for all the model
model_R_Squared_values <- list(model=NULL,R_squared=NULL, RMSE = NULL)
model_R_Squared_values[["model"]] <- c("mod","mod_log","mod_exp")
model_R_Squared_values[["R_squared"]] <- c(0.89,0.807,0.877)
model_R_Squared_values[["RMSE"]]<-c(141,141,476.4)
model_R_Squared_values

#### the model "mod" has the best R^2 value and a low RMSE value so it is the best fit model ###
