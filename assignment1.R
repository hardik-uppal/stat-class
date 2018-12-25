# install the library and data
library(ISLR)
data(Auto)

## part(a)
attach(Auto)
str(Auto)
pairs(Auto[,1:7], pch='.')

## part(b)
cor(Auto[,1:7])

## part (c)
origin_cat = as.factor(origin)
reg_fit = lm(formula = mpg ~ cylinders + displacement +horsepower+weight+acceleration+year+ origin_cat)
summary(reg_fit)

## part (d)
contrasts(origin_cat)
new = data.frame(cylinders= 8, displacement = 300, horsepower = 150, weight = 3600, acceleration = 13, year = 77, origin_cat= "3")
pred_new_clim = predict.lm(reg_fit, new, se.fit=TRUE, interval = "confidence")
pred_new_clim

# part (e)
sfit = summary(reg_fit)
sfit$fstatistic[1]
qf(0.95,sfit$fstatistic[2],sfit$fstatistic[3])

## part (g)
par(mfrow=c(4,3))
plot(fitted(reg_fit), sfit$residuals,xlab = "fitted values", ylab="residuals")
plot(cylinders, sfit$residuals,xlab = "cylinder", ylab="residuals") 
plot(displacement, sfit$residuals,xlab = "displacement", ylab="residuals") 
plot(horsepower, sfit$residuals,xlab = "horsepower", ylab="residuals") 
plot(weight, sfit$residuals,xlab = "weight", ylab="residuals") 
plot(acceleration, sfit$residuals,xlab = "acceleration", ylab="residuals") 
plot(year, sfit$residuals,xlab = "year", ylab="residuals") 
plot(origin, sfit$residuals,xlab = "origin", ylab="residuals") 
plot(1:length(mpg), sfit$residuals,xlab = "Index",ylab = "residuals")
qqnorm(sfit$residuals)
qqline(sfit$residuals)



