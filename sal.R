data<- `Salary_Data.(1)`
plot(data)
ye<-data$YearsExperience
sal<-data$Salary
cor(data$YearsExperience, data$Salary)
plot(ye,sal,cex= 1, col= "Blue", main = "salary hike vs year of experience")
mod<-lm(sal~ye) 
summary(mod)
abline(mod)
pred<- predict(mod, list(ye = c(7, 15,8,9 )))
pred
