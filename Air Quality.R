airqulaity<- datasets::airquality
airqulaity
head(airquality, 5)
tail(airquality, 4)
summary(airquality)
summary(airquality$Solar.R)
plot(airquality$Ozone, airquality$Temp)
plot(airquality)
plot(airquality$Ozone, xlab = "ozone concentration", ylab = "levels", main = "Ozone levels in Ny city", col =("purple"))
barplot(airquality$Ozone, horiz = TRUE)
hist(airquality$Ozone,xlab = "ozone levels", ylab = "levels",col = "pink",main = "levels of ozone in Ny city")
boxplot(airquality)
boxplot(airquality[,1:4], main = "mutiple box plots")
par(mfrow= c(3, 3), mar= c(2, 5,2, 1), las= 0, bty