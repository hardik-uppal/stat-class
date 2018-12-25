###################Question 2##############################
install.packages("ISLR")
library(ISLR)
data(Auto)

##(a) 
attach(Auto)
median(mpg)
mpg01 =as.numeric( mpg>median(mpg))


##(b)
# include mpg01 to the dataset Auto
Auto = cbind(Auto, mpg01)
Auto = as.data.frame(Auto)
 
# boxplots
par(mfrow=c(2,3))
boxplot(cylinders  ~ mpg01, xlab = "mpg01", ylab="cylinders", names=c("below median","above median"))
boxplot(displacement  ~ mpg01, xlab = "mpg01", ylab="displacement", names=c("below median","above median"))
boxplot(horsepower  ~ mpg01, xlab = "mpg01", ylab="horsepower", names=c("below median","above median"))
boxplot(weight  ~ mpg01, xlab = "mpg01", ylab="weight", names=c("below median","above median"))
boxplot(acceleration  ~ mpg01, xlab = "mpg01", ylab="acceleration", names=c("below median","above median"))
boxplot(year  ~ mpg01, xlab = "mpg01", ylab="year", names=c("below median","above median"))

require(lattice)
origin = as.factor(origin)
levels(origin) = c("American", "European","Japanese")
Auto$origin=origin
Auto$mpg01 = as.factor(mpg01)
xyplot(mpg ~ mpg01|origin,data=Auto)


##(c) nt: number of training points
library(MASS)
n= dim(Auto)[1]
nt = 300
sp = sample (1:n,nt)
train = (1:n)%in% sp
Auto_training = Auto[sp,]
Auto_test = Auto[-sp,]

#(d)
# linear discriminant 
fit.lda <- lda(mpg01 ~ cylinders + weight + displacement + horsepower +year +origin, data = Auto, subset = train)
fit.lda 
model.matrix.lda = model.matrix(fit.lda)
cov.lda =  ((sum(as.numeric(Auto_training$mpg01)-1)-1)*cov( model.matrix.lda[(as.numeric(Auto_training$mpg01)-1)==1,2:8])+
(nt-sum(as.numeric(Auto_training$mpg01)-1)-1)*cov( model.matrix.lda[(as.numeric(Auto_training$mpg01)-1)==0,2:8]))/(nt-1)


pred.lda <- predict(fit.lda, Auto_test)
table(pred.lda$class, mpg01[-sp])
mean(pred.lda$class != mpg01[-sp])


#(e) quadratic discriminant 
fit.qda <- qda(mpg01 ~ cylinders + weight + displacement + horsepower+year+origin, data = Auto, subset = train)
fit.qda

pred.qda <- predict(fit.qda, Auto_test)
table(pred.qda$class, mpg01[-sp])
mean(pred.qda$class != mpg01[-sp])

#(f) logistic regression
fit.glm <- glm(mpg01 ~ cylinders + weight + displacement + horsepower+year+origin, data = Auto, family = binomial, subset = train)
summary(fit.glm)

prob_logit <- predict(fit.glm, Auto_test, type = "response")
pred.glm = as.numeric(prob_logit>=0.5)
table(pred.glm, mpg01[-sp])
mean(pred.glm != mpg01[-sp])

#(g) the prediction 
new = data.frame(cylinders = 6, displacement = 400,
horsepower = 110, weight = 3000, year = 75, origin = "American")
pred.lda <- predict(fit.lda,new)
pred.qda <- predict(fit.qda,new)
pred.glm <- predict(fit.glm,new, type = "response")

## (f) knn 
require(class)
mpg01.knn= rep(0,100)
Auto_test$origin = as.numeric(Auto_test[,8])
Auto_training$origin = as.numeric(Auto_training[,8])
for (k in 1:100){
 knn.fit = knn( Auto_training[,c(2:5,7,8)], Auto_test[,c(2:5,7,8)], Auto_training[,10], k )
 mpg01.knn[k] = mean( knn.fit == Auto_test[,10] )
 }
which.max(mpg01.knn)
max(mpg01.knn)
###################Question 3 ##############################
 
X = 1
rss_ridge = function(X,Y, lambda, beta)
{ return( (Y - X*beta)^2+lambda*beta^2 )}
rss_lasso = function(X,Y, lambda, beta)
{ return( (Y - X*beta)^2+lambda*abs(beta))}
par(mfrow = c(2,3))
Y = 1
lambda = 2
beta = seq(0,4,length = 100)
plot( beta, rss_ridge(X,Y, lambda,beta),xlab = expression(beta),type = 'l', ylab = "RSS ridge",main = "Y=1, lambda=2")
abline(v = Y/(1+lambda))
Y = 2
lambda = 2
beta = seq(0,4,length = 100)
plot( beta, rss_ridge(X,Y, lambda,beta),xlab = expression(beta), type = 'l', ylab = "RSS ridge",main = "Y=2, lambda=2")
abline(v = Y/(1+lambda))
Y = 5
lambda = 2
beta = seq(0,4,length = 100)
plot( beta, rss_ridge(X,Y, lambda,beta),xlab = expression(beta), type = 'l', ylab = "RSS ridge",main = "Y=5, lambda=2")
abline(v = Y/(1+lambda))
Y = 1
lambda = 0.5
beta = seq(0,4,length = 100)
plot( beta, rss_ridge(X,Y, lambda,beta),xlab = expression(beta),type = 'l', ylab = "RSS ridge",main = "Y=1, lambda=0.5")
abline(v = Y/(1+lambda))
Y = 2
lambda = 0.5
beta = seq(0,4,length = 100)
plot( beta, rss_ridge(X,Y, lambda,beta),xlab = expression(beta), type = 'l', ylab = "RSS ridge",main = "Y=2, lambda=0.5")
abline(v = Y/(1+lambda))
Y = 5
lambda = 0.5
beta = seq(0,4,length = 100)
plot( beta, rss_ridge(X,Y, lambda,beta),xlab = expression(beta), type = 'l', ylab = "RSS ridge",main = "Y=5, lambda=0.5")
abline(v = Y/(1+lambda))

## lasso
par(mfrow = c(2,3))

Y = 1
lambda = 2
beta = seq(0,6,length = 100)
plot( beta, rss_lasso(X,Y, lambda,beta),xlab = expression(beta),type = 'l', ylab = "RSS lasso",main = "Y=1, lambda=2")
if(Y > lambda/2)
 { 
   abline(v = Y -lambda/2)
   } else if(Y < -lambda/2)
      { 
	    abline(v = Y +lambda/2)
		} else {
	  abline(v =0)
	   }
 
 
Y = 2
lambda = 2
beta = seq(0,6,length = 100)
plot( beta, rss_lasso(X,Y, lambda,beta),xlab = expression(beta), type = 'l', ylab = "RSS lasso",main = "Y=2, lambda=2")
if(Y > lambda/2)
 { 
   abline(v = Y -lambda/2)
   } else if(Y < -lambda/2)
      { 
	    abline(v = Y +lambda/2)
		} else {
	  abline(v =0)
	   }
 
 
Y = 5
lambda = 2
beta = seq(0,6,length = 100)
plot( beta, rss_lasso(X,Y, lambda,beta),xlab = expression(beta), type = 'l', ylab = "RSS lasso",main = "Y=5, lambda=2")
if(Y > lambda/2)
 { 
   abline(v = Y -lambda/2)
   } else if(Y < -lambda/2)
      { 
	    abline(v = Y +lambda/2)
		} else {
	  abline(v =0)
	   }
 
Y = 1
lambda = 1
beta = seq(0,6,length = 100)
plot( beta, rss_lasso(X,Y, lambda,beta),xlab = expression(beta),type = 'l', ylab = "RSS lasso",main = "Y=1, lambda=1")
if(Y > lambda/2)
 { 
   abline(v = Y -lambda/2)
   } else if(Y < -lambda/2)
      { 
	    abline(v = Y +lambda/2)
		} else {
	  abline(v =0)
	   }
 
 
Y = 2
lambda = 1
beta = seq(0,6,length = 100)
plot( beta, rss_lasso(X,Y, lambda,beta),xlab = expression(beta), type = 'l', ylab = "RSS lasso",main = "Y=2, lambda=1")
if(Y > lambda/2)
 { 
   abline(v = Y -lambda/2)
   } else if(Y < -lambda/2)
      { 
	    abline(v = Y +lambda/2)
		} else {
	  abline(v =0)
	   }
 
Y = 5
lambda = 1
beta = seq(0,6,length = 100)
plot( beta, rss_lasso(X,Y, lambda,beta),xlab = expression(beta), type = 'l', ylab = "RSS lasso",main = "Y=5, lambda=1")
if(Y > lambda/2)
 { 
   abline(v = Y -lambda/2)
   } else if(Y < -lambda/2)
      { 
	    abline(v = Y +lambda/2)
		} else {
	  abline(v =0)
	   }
	 
 




### part (b)

est_lasso = function(Y, lambda)
{
v = vector("numeric",length(lambda))
for(i in 1:length(lambda))
{ if(Y > lambda[i]/2)
 { 
   v[i] = Y -lambda[i]/2
   } else if(Y < -lambda[i]/2)
      { 
	     v[i] = Y +lambda[i]/2
		} else {
	   v[i] = 0
	   }
}
return(v)
}
par(mfrow=c(2,3))
 Y = 0.5
 lambda = seq(0.1,10,length = 100)
 plot(lambda, Y/(1+ lambda), xlab = "lambda" ,ylab = "estimate of beta",lty=1,col= 'red',ylim = c(0,1),main = "Y=0.5")
 lines(lambda, est_lasso(Y, lambda), col='blue',lty=2)
 legend(4,0.8, legend=c("Ridge ", "Lasso"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
 Y = 1 
 lambda = seq(0.1,10,length = 100)
 plot(lambda, Y/(1+ lambda), xlab = "lambda" ,ylab = "estimate of beta",lty=1,col= 'red',ylim = c(0,1),main = "Y=1")
 lines(lambda, est_lasso(Y, lambda), col='blue',lty=2)
 legend(4,0.8, legend=c("Ridge ", "Lasso"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
 Y = 2 
 lambda = seq(0.1,10,length = 100)
 plot(lambda, Y/(1+ lambda), xlab = "lambda" ,ylab = "estimate of beta",lty=1,col= 'red',ylim = c(0,2),main = "Y=2")
 lines(lambda, est_lasso(Y, lambda), col='blue',lty=2)
 legend(4,1, legend=c("Ridge ", "Lasso"),
       col=c("red", "blue"), lty=1:2, cex=0.8)	   
 Y = 5
 lambda = seq(0.1,20,length = 100)
 plot(lambda, Y/(1+ lambda), xlab = "lambda" ,ylab = "estimate of beta",lty=1,col= 'red',ylim = c(0,10),main = "Y=5")
 lines(lambda, est_lasso(Y, lambda), col='blue',lty=2)
 legend(5,4, legend=c("Ridge ", "Lasso"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
 Y = 10
 lambda = seq(0.1,50,length = 1000)
 plot(lambda, Y/(1+ lambda), xlab = "lambda" ,ylab = "estimate of beta",lty=1,col= 'red',ylim = c(0,20),main = "Y=10")
 lines(lambda, est_lasso(Y, lambda), col='blue',lty=2)
 legend(20,8, legend=c("Ridge ", "Lasso"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
 Y = 20
 lambda = seq(0.1,100,length = 1000)
 plot(lambda, Y/(1+ lambda), xlab = "lambda" ,ylab = "estimate of beta",lty=1,col= 'red',ylim = c(0,20),main = "Y=20")
 lines(lambda, est_lasso(Y, lambda), col='blue',lty=2)
 legend(40,15, legend=c("Ridge ", "Lasso"),
       col=c("red", "blue"), lty=1:2, cex=0.8)	   
###################Question 4 ############################## 
n =200
X1 = runif(n, 0, 4)
X2 = runif(n, 3, 8)
X3 = runif(n,-1,5)

Y = 10+1.5*X1-0.3*X2 +10.7*X3 + rnorm(n, 0, sqrt(3))
Z1 = 1.5*X1*X2
Z2 = -3.6*X1*X3
Z3 = X2*X3
Z4 = rnorm(n,20,sqrt(40))
Z5 = rnorm(n,10,1)
X = cbind(X1,X2,X3,Z1,Z2,Z3,Z4,Z5)

#(a) solution path
install.packages("lars")
library(lars)
fit.lars = lars(X,Y, type="lasso",trace=TRUE)
plot(fit.lars)

# alternative way
install.packages("glmnet")
library(glmnet)
fit = glmnet(X,Y)
plot(fit)

 
 
#(b)
cv.fit.lars.f = cv.lars(X,Y,mode="fraction")
which.min(cv.fit.lars.f$cv)
min(cv.fit.lars.f$cv)


cvfit = cv.glmnet(X,Y)
plot(cvfit)
coef(fit,s=cvfit$lambda.min)
min(cvfit$cvm)
yhat = predict(fit, cvfit$lambda.min, newx = X  )

#(c)&(d)
bestindex = cv.fit.lars.f$index[which.min(cv.fit.lars.f$cv)]
bestindex
predict.lars(fit.lars,s=bestindex,mode="fraction",type="coefficients")  

###################Question 5 ##############################
### (a) 
library(ISLR)
attach(College)
n = length(Apps)
Z = sample(n,500)
train_College = College[Z,]
test_College = College[-Z,]

### (b)
install.packages("leaps")
library(leaps)
reg = lm( Apps ~ ., data=train_College)
summary(reg)
 
##
# nvmax: maximum size of subsets to examine
leaps=regsubsets(Apps~., data=train_College,nvmax=15)

# 
### 
summary(leaps)
reg.summary=summary(leaps)
  
### the adjusted R^2 of the top 15 models
reg.summary$adjr2
which.max(reg.summary$adjr2)

### the Cp of the top 15 models
reg.summary$cp

### the Cp of the top 15 models
reg.summary$bic
which.min(reg.summary$bic)


#  plot of RSS, adjusted R^2, Cp and BIC together
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Predictors", ylab="Residual Sum of Squares", type="l", xlim=c(0,11), ylim=c(min(reg.summary$rss), max(reg.summary$rss)))
points(which.min(reg.summary$rss), reg.summary$rss[which.min(reg.summary$rss)], cex=2, pch=20, col="red")

plot(reg.summary$cp, xlab="Number of Predictors", ylab="Cp", type="l", xlim=c(0,11), ylim=c(min(reg.summary$cp),max(reg.summary$cp)))
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], cex=2, pch=20, col="red")

plot(reg.summary$adjr2, xlab="Number of Predictors", ylab="Adjusted R Square", type="l", xlim=c(0,11), ylim=c(0,1))
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], cex=2, pch=20, col="red")

plot(reg.summary$bic, xlab="Number of Predictors", ylab="BIC", type="l", xlim=c(0,11))
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)], cex=2, pch=20, col="red")

## the best model by bic
reg = lm( Apps ~ Private + Accept + Enroll + Top10perc + Outstate + Room.Board + PhD + Expend, data = train_College )
summary(reg)
Yhat = predict( reg, test_College )
mean( (Yhat- test_College$App)^2 )
sum((Yhat- test_College$App)^2 )/(nrow(test_College)-1)


##### (c) ridge regression 
train_College$Private = as.numeric(train_College$Private)
X = as.matrix(train_College[,-2])
y = train_College[,2] 
cv.ridge = cv.glmnet(X,y)
cv.ridge$lambda.min
plot(cv.ridge)
ridge = glmnet( X, y, alpha=0, lambda=cv.ridge$lambda.min )
coef(ridge)

test_College$Private = as.numeric(test_College$Private)
newX = as.matrix(test_College[-2])
Yhat.ridge = predict( ridge, cv.ridge$lambda.min, newx = newX )
mean( (Yhat.ridge- test_College$App)^2 )
sum((Yhat.ridge- test_College$App)^2 )/(nrow(test_College)-1)

### (d) lasso
cv.lasso= cv.glmnet(X,y )
cv.lasso$lambda.min
plot(cv.lasso)
lasso = glmnet( X, y, alpha=1, lambda=cv.ridge$lambda.min )
coef(lasso)

test_College$Private = as.numeric(test_College$Private)
newX = as.matrix(test_College[,-2])
Yhat.lasso = predict( lasso, cv.lasso$lambda.min, newx = newX )
mean( (Yhat.lasso- test_College$App)^2 )
sum((Yhat.lasso- test_College$App)^2 )/(nrow(test_College)-1)

### (e) 
