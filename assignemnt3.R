library(ISLR)
attach(Auto)

xyplot(acceleration~horsepower,xlab = "Horsepower",ylab = "Acceleration")
library(splines)

myPanel <- function(...)
{
  panel.smooth(...)
  panel.xyplot(...,alpha=.5)
}


panel.smooth <- function(x,y,...)
{
  knots=quantile(x,c(1/3,2/3))
  fit = lm(y~bs(x,knots=knots,degree=3))
  x0 = seq(min(x),max(x),len=101)
  llines(x0,predict(fit,data.frame(x=x0)),col="black")
  lsegments(x0=knots[1],y0=min(y),x1=knots[1],y1=max(y),col="black",lty=2)
  lsegments(x0=knots[2],y0=min(y),x1=knots[2],y1=max(y),col="black",lty=2)
}
xyplot(acceleration~horsepower,Auto,panel=myPanel)
knots=quantile(horsepower,c(1/3,2/3))
knots
fit = glm(acceleration~bs(horsepower,knots=knots,degree=3))
summary(fit)
mse.rs= cv.glm(Auto,fit)$delta[1]



#########b)
plot(acceleration~horsepower, data=Auto,xlab = "Horsepower",ylab = "Acceleration")
smoothing.param=smooth.spline(horsepower,acceleration,cv=T)
smoothing.param$lambda
smoothing.param$cv.crit
summary(smoothing.param)


lines(smoothing.param, col="blue")
lines(sort(horsepower),fitted(fit)[order(horsepower)], col="red")
legend("topright",legend=c("Regression spline","smoothing spline") ,col=c("red","blue") , lty =1 , lwd =2 , cex =.8)

################ 2 ################ 

attach(Boston)
Boston

range(dis)


poly.fit=lm(nox~poly(dis,3))
summary(poly.fit)
plot(poly.fit)
plot(dis, nox)
lines(sort(dis), fitted(poly.fit)[order(dis)], col='red', type='b') 

######b
cv.err = rep(NA,10)
color=c("red","blue","green","yellow","black","grey","orange","brown","pink","purple")
plot(dis, nox)
for (i in 1:10){
  fit = glm(nox~poly(dis,i),data=Boston)
  lines(sort(dis), fitted(fit)[order(dis)], col=color[i])
  cv.err[i] = sum(fit$residuals^2)
  
} 
legend("topright",legend=c("polynomial 1st degree","polynomial 2nd degree","polynomial 3rd degree","polynomial 4th degree","polynomial 5th degree","polynomial 6th degree","polynomial 7th degree","polynomial 8th degree","polynomial 9th degree","polynomial 10th degree") ,col=c("red","blue","green","yellow","black","grey","orange","brown","pink","purple") , lty =1 , lwd =2 , cex =.8)
cv.err
point_min=which.min(cv.err)
plot(1:10, cv.err, xlab = "Degree", ylab = "Test MSE", type = "l")
points(point_min,cv.err[point_min], col="red")


####  c
cv.error=seq(1,10)
for (i in 1:10){
  glm.fit=glm(nox~poly(dis,i),data=Boston)
  cv.error[i]=cv.glm(Boston,glm.fit,K=5)$delta[1]
}
cv.error
point_min=which.min(cv.error)
plot(1:10, cv.error, xlab = "Degree", ylab = "Test MSE", type = "l")
points(point_min,cv.error[point_min], col="red")

################  d
xyplot(nox~dis,Boston)

myPanel <- function(...)
{
  panel.smooth(...)
  panel.xyplot(...,alpha=.5)
}


panel.smooth <- function(x,y,...)
{
  #knots=quantile(x,c(1/3,2/3))
  fit = lm(y ~ bs(x,df=4))
  summary(fit)
  x0 = seq(min(x),max(x),len=101)
  llines(x0,predict(fit,data.frame(x=x0)),col="black")
  knots=attr(bs(x,df=4),"knots")##id df is given use this knots
  for(i in 1:length(knots)){
    lsegments(x0=knots[1],y0=min(y),x1=knots[i],y1=max(y), col="black",lty=2)
  }
}
xyplot(nox~dis,Boston,panel=myPanel)
fit = lm(nox ~ bs(dis,df=4))
summary(fit)
knots=attr(bs(dis,df=4),"knots")

####### e
#fit = seq(1,10)
rss <- rep(1, 12)
plot(dis, nox)
color=c("red","blue","green","yellow","black","grey","orange","brown","pink","purple")
for (i in 3:12) {
  fit <- lm(nox ~ bs(dis, df = i), data = Boston)
  lines(sort(dis), fitted(fit)[order(dis)], col=color[i-2])
  rss[i] <- sum(fit$residuals^2)
  
}

legend("topright",legend=c("polynomial 3rd degree of freedom","polynomial 4th degree of freedom","polynomial 5th degree of freedom","polynomial 6th degree of freedom","polynomial 7th degree of freedom","polynomial 8th degree of freedom","polynomial 9th degree of freedom","polynomial 10th degree of freedom","polynomial 11th degree of freedom","polynomial 12th degree of freedom") ,col=c("red","blue","green","yellow","black","grey","orange","brown","pink","purple") , lty =1 , lwd =2 , cex =.8)
rss
which.min(rss)

################  f

cv.error=rep(1,10)
for (i in 3:10){
  ##knots=quantile(dis,c(1/3,2/3))
  glm.fit=glm(nox~bs(dis,df=i),data=Boston)
  cv.error[i]=cv.glm(Boston,glm.fit,K=5)$delta[1]
}
cv.error
glm.fit$rank
which.min(cv.error)
min(cv.error)

################ question 4 #################
################ a ################
library(ISLR)
auto_df=data.frame(Auto)
attach(auto_df)
mpg.mean =mean(mpg)
mpg.mean

################ b ################
boot.fn = function(data, index) return(mean(data[index]))
library(boot)
bstrap = boot(mpg, boot.fn, 1000)
bstrap

################ c ################
c(bstrap$t0 - 2 * 0.3890956, bstrap$t0 + 2 * 0.3890956)

################ d ################
mpg.median =median(mpg)
mpg.median

################ e ################
boot.med.fn = function(data, index) return(median(data[index]))

bstrap.med = boot(mpg, boot.med.fn, 1000)
bstrap.med

################ f ################
c(bstrap.med$t0 - 2 * 0.7652976, bstrap.med$t0 + 2 * 0.7652976)



################ question 5 #################
#### a

set.seed(100)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

###n=100 and p=2

####### b

plot(x, y)
 #### c

new.data = data.frame(x, y)

glm.fit = glm(y ~ x)
cv.glm(new.data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 2))
cv.glm(new.data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 3))
cv.glm(new.data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 4))
cv.glm(new.data, glm.fit)$delta
####### d ##########
set.seed(19)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

new.data = data.frame(x, y)

glm.fit = glm(y ~ x)
cv.glm(new.data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 2))
cv.glm(new.data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 3))
cv.glm(new.data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 4))
cv.glm(new.data, glm.fit)$delta

####### f ##########
summary(glm.fit)

#################### 6 #################### 

fsample<-function(mu,sigma,n,nsim,alpha)
{
  xbar<-rep(0,nsim);x_sd<-rep(0,nsim);flag=0
  for(i in 1:nsim)
  {
    set.seed(i)
  # x contains a random sample of size n of the variable X
    x<-rnorm(n,mu,sigma)
    xbar[i]<-mean(x)
    x_sd[i]<-sd(x)
    cat('sample mean:',xbar[i],"\n")
    cat('Confidence interval for sample mean:',xbar[i] - qnorm(1-(alpha/2)) * x_sd[i], xbar[i] + qnorm(1-(alpha/2)) * x_sd[i],"\n")
    if((mu<xbar[i] + qnorm(1-(alpha/2)) * x_sd[i]) && (mu>xbar[i] - qnorm(1-(alpha/2)) * x_sd[i])){
      flag=flag+1
    }
  }
  cat('Coverage:',flag/nsim,"\n")
}

fsample(2,sqrt(5),10,1000,0.05)
fsample(2,sqrt(5),10,1000,0.025)
fsample(2,sqrt(5),100,1000,0.05)
fsample(2,sqrt(5),100,1000,0.025)


################## c

fsample<-function(mu,n,nsim,alpha)
{
  xbar<-rep(0,nsim);x_sd<-rep(0,nsim);flag=0
  for(i in 1:nsim)
  {
    set.seed(i)
    # x contains a random sample of size n of the variable X
    x<-rnorm(n,mu)
    xbar[i]<-mean(x)
    x_sd[i]<-sd(x)
    cat('sample mean:',xbar[i],"\n")
    cat('Confidence interval for sample mean:',xbar[i] - qnorm(1-(alpha/2)) * x_sd[i], xbar[i] + qnorm(1-(alpha/2)) * x_sd[i],"\n")
    if((mu<xbar[i] + qnorm(1-(alpha/2)) * x_sd[i]) && (mu>xbar[i] - qnorm(1-(alpha/2)) * x_sd[i])){
      flag=flag+1
    }
  }
  cat('Coverage:',flag/nsim,"\n")
}

fsample(2,10,1000,0.05)
fsample(2,10,1000,0.025)
fsample(2,100,1000,0.05)
fsample(2,100,1000,0.025)

#################### 3 #################### 



library(splines)
n=50
x0=runif(n,0,1)
x0=sort(x0)
X=cbind(1,x0,x0^2,x0^3)
eps=rnorm(length(x0))
beta <- c(4.3965,1.9407,-0.1215,0.1535)#assumed true coefficients
Y=X%*%beta+eps
#Y=cos(x0)+eps

###cubic spline 
N=bs(x0,knots=c(0.33,0.66),degree =6)
N=cbind(1,N)
H=N%*%solve(t(N)%*%N)%*%t(N)
yhat_1=H%*%Y
sigma2_1=(1/(n-ncol(N)))*sum((yhat_1-Y)^2)
var1=diag(H)
###natural cubic spline
v=seq(0.1,0.9,length=6)
N2=ns(x = x0,knots = v[2:5],Boundary.knots = c(0.1,0.9),df = 6)
H2=N2%*%solve(t(N2)%*%N2)%*%t(N2)
yhat_2=H2%*%Y
sigma2_2=(1/(n-ncol(N2)))*sum((yhat_2-Y)^2)
var2=diag(H2)
#ploynomial fit
P=poly(x0,3)
H3=P%*%solve(t(P)%*%P)%*%t(P)
yhat_3=H3%*%Y
sigma2_3=(1/(n-ncol(P)))*sum((yhat_3-Y)^2)
var3=diag(H3)
#global linear
X1=cbind(1,x0)
H4=X1%*%solve(t(X1)%*%X1)%*%t(X1)
yhat_4=H4%*%Y
sigma2_4=(1/(n-ncol(X1)))*sum((yhat_4-Y)^2)
var4=diag(H4)

minY=min(var1,var2,var3,var4)
maxY=max(var1,var2,var3,var4)
plot(x0,var1,pch=20,type='b',ylim=c(minY,maxY+0.1),col="green",
     xlab = "X",ylab = "pointwisevariance",main = "Pointwise variance curves")
lines(x0,var2,pch=20,type='b',col="blue")
lines(x0,var3,pch=20,type='b',col="red")
lines(x0,var4,pch=20,type='b',col="orange")
legend("top",legend=c("cubic spline","natural cubic spline","polynomial fit","global linear"),
       col=c("green","blue","red","orange"),lty=2)
