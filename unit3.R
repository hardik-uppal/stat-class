## install data
install.packages("ISLR")
library(ISLR)
Advertising= read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
Advertising$X = NULL
Advertising
names(Advertising)
pairs(Advertising)


## model fitting
fit = lm(sales~TV+radio+newspaper, data = Advertising)
fit = lm(sales~., data = Advertising)
sfit = summary(fit)
str(sfit)


##  Is there a relationship between advertising sales and budget?
sfit$fstatistic[1]
1- pf(sfit$fstatistic[1],sfit$fstatistic[2],sfit$fstatistic[3])

##How strong is the relationship?
sfit$sigma
mean(Advertising$sales)
sfit$sigma/mean(Advertising$sales)
sfit$r.squared

##Which media contribute to sales?
sfit

## How large is the effect of each medium on sales?
sfit$coefficients
coef(sfit)
cbind(sfit$coefficients[,1]-qt(0.975,196)*sfit$coefficients[,2], sfit$coefficients[,1]+ qt(0.975,196)*sfit$coefficients[,2])
confint(fit)

## How accurately can we predict future sales?
fitted.values = fitted(fit)
pred_clim = predict.lm(fit,data = Advertising,se.fit=TRUE, interval =  "confidence")
pred_plim = predict.lm(fit,data = Advertising,se.fit=TRUE, interval =  "prediction")
new = data.frame(TV = 60, radio = 6, newspaper = 26 )
pred_new_clim = predict.lm(fit, new,se.fit=TRUE, interval =  "confidence")
pred_new_plim = predict.lm(fit, new,se.fit=TRUE, interval =  "prediction")

## model diagnostic
par(mfrow=c(2,3))
plot(fitted.values, sfit$residuals,xlab = "fitted values", ylab="residuals")
plot(Advertising$TV, sfit$residuals,xlab = "TV", ylab="residuals")
plot(Advertising$radio, sfit$residuals,xlab = "radio", ylab="residuals")
plot(Advertising$newspaper, sfit$residuals,xlab = "newspaper", ylab="residuals")
plot(1:200, sfit$residuals,xlab = "Index",ylab = "residuals")
qqnorm(sfit$residuals)
qqline(sfit$residuals)

plot(fit)

## compare models
fit2 = lm(sales~TV+radio, data = Advertising)
anova(fit2,fit)


## include interactiona 
fit3 = lm(sales~TV*radio, data = Advertising)
anova(fit2,fit3)
summary(fit3)


### data carset 
data( Carseats )
names(Carseats)

## polynomial model 
fit_p2  = lm(Sales ~ .^2, data = Carseats)
summary(fit_p2)
 
fit_pint = lm(Sales ~ . + I(CompPrice^2) + Income*Advertising+ Price:Age, data = Carseats)
summary(fit_pint)

