install.packages("leaps")
install.packages("lasso2")
library(leaps)
library(lasso2)
data(Prostate)
attach(Prostate)


####################################################
## Subset Selection
####################################################

### best subset selction 
# nbest: number of subsets of each size to record
leaps=regsubsets(lpsa~lcavol+lweight+age
      +lbph+svi+lcp+gleason+pgg45, data=Prostate,nbest=5)

# nvmax: maximum size of subsets to examine
leaps=regsubsets(lpsa~., data=Prostate,nvmax=10)

#### 
pdf("bestsubset_leaps_r2.pdf")
plot(leaps,scale="r2")
dev.off()
pdf("bestsubset_leaps_adjr2.pdf")
plot(leaps,scale="adjr2")
dev.off()
pdf("bestsubset_leaps_Cp.pdf")
plot(leaps,scale="Cp")
dev.off()
pdf("bestsubset_leaps_bic.pdf")
plot(leaps,scale="bic")
dev.off()

### 
summary(leaps)
reg.summary=summary(leaps)

### the residual sum of squares of the top 10 models
reg.summary$rss

### the R^2 of the top 10 models
reg.summary$rsq

### the adjusted R^2 of the top 10 models
reg.summary$adjr2

### the Cp of the top 10 models
reg.summary$cp



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


### another way of producing C_p plot
subset<-leaps(x=Prostate[,1:8],y=Prostate[,9])
plot(x=subset$size,y=subset$Cp,xlab='size',ylab='Cp')



### full model
fit1<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,
  data=Prostate)
fit1<-lm(lpsa~ .,data=Prostate)

### null model
fit0<-lm(lpsa~1,data=Prostate)

### forward selection
fit.forward<-step(fit0,scope=list(lower=lpsa~1, upper=fit1),direction='forward')
summary(fit.forward)

### backward selection
fit.backward<-step(fit1,scope=list(lower=lpsa~1, upper=fit1),direction='backward')
summary(fit.backward)  

### stepwise regression by AIC criterion
fit.both<-step(fit0,scope=list(lower=lpsa~1, upper=fit1),direction='both')
summary(fit.both)  


####################################################
## Ridge Regression
####################################################
          
library(MASS)  

# in the presence of multi-collinearity 

x1  = rnorm(30)
x2  = rnorm(30,mean=x1,sd=.01)
y   = rnorm(30,mean=5+x1+x2)
lm(y~x1+x2)$coef
lm.ridge(y~x1+x2,lambda=1)

# Prostate example
prostate = scale(Prostate)
prostate = as.data.frame(prostate)
fit.ridge<-lm.ridge(lpsa~lcavol+lweight+age
         +lbph+svi+lcp+gleason+pgg45,
         data=prostate, lambda=seq(0,20,0.1))  
plot(fit.ridge)  
plot(seq(0,20,0.1),fit.ridge$GCV,xlab= expression(lambda),ylab="GCV")

select(fit.ridge)
round(fit.ridge$coef[, which(fit.ridge$lambda == 6.5)], 2)    
fit.ridge<-lm.ridge(lpsa~lcavol+lweight+age  
         +lbph+svi+lcp+gleason+pgg45,   
         data=prostate, lambda=6.5)   
fit.ridge$coef   
yhat = vector('numeric',length(prostate$lpsa))
for(i in 1:8)
  yhat = yhat + fit.ridge$coef[i]*(Prostate[,i]- mean(Prostate[,i]))/sd(Prostate[,i])
yhat1 = mean(Prostate[,9]) +sd(Prostate[,9])*yhat

## use the package glmnet 
install.packages("glmnet")
library(glmnet)
X = as.matrix(Prostate[,1:8])
y = Prostate$lpsa
cv.ridge = cv.glmnet( X,y, alpha=0, lambda=seq(0,10,0.001) )
cv.ridge$lambda.min
ridge = glmnet(X,y, alpha=0, lambda=cv.ridge$lambda.min )
coef(ridge)
yhat2 = predict( ridge, cv.ridge$lambda.min, newx = X )

####################################################
## Lasso
####################################################
install.packages("glmnet")
library(glmnet)
X = as.matrix(Prostate[,1:8])
y = Prostate$lpsa

fit = glmnet(X,y)
plot(fit)
cvfit = cv.glmnet(X,y)
plot(cvfit)
coef(fit,s=cvfit$lambda.min)
min(cvfit$cvm)
yhat = predict(fit, cv.fit$lambda.min, newx = X  )
 

##lars
install.packages("lars")
library(lars)
fit.lars = lars(X,y, type="lasso",trace=TRUE)
plot(fit.lars)
cv.fit.lars = cv.lars(X,y,mode="step")
cbind(cv.fit.lars$index,cv.fit.lars$cv)
bestindex = cv.fit.lars$index[which.min(cv.fit.lars$cv)]
which.min(cv.fit.lars$cv)
fit.lars$beta
bestindex
fit.lars$lambda
fit.lars$beta[bestindex,]


cv.fit.lars.f = cv.lars(X,y,mode="fraction")
which.min(cv.fit.lars.f$cv)
bestindex = cv.fit.lars.f$index[which.min(cv.fit.lars.f$cv)]
bestindex
predict.lars(fit.lars,s=bestindex,mode="fraction",type="coefficients") 
p.lars = predict.lars(fit.lars,s=bestindex,mode="fraction",type="coefficients")
p.lars$coefficient 





detach(Prostate)


