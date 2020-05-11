data<- `emp_data.(1)`
plot(data$Salary_hike, data$Churn_out_rate,pch= 16,cex= 1, col="blue",  main = "salary vs churn out")
sh<- data$Salary_hike
co<- data$Churn_out_rate
mod<- lm(co~sh, data = data)
summary(mod)
plot(data$Salary_hike, mod$fitted.values)
cor(sh,co)
attributes(mod)
mod$fitted.values
predict_data<-predict(mod, list(sh=c(1900,1760, 2000, 2222, 2450, 1000, 500 )))
predict_data
list(predict_data)        
