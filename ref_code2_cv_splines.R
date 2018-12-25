library(boot)
###for cv.glm

# k-Fold Cross-Validation
set.seed(3)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

library(boot)
# Leave-One-Out Cross-Validation
####just leave the K value out
cv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error




############################ Splines   ###################################

###### Regression
#plot fit
library(lattice)
myPanel <- function(...)
{
  panel.smooth(...)
  panel.xyplot(...,alpha=.5)
}

panel.smooth <- function(x,y,...)
{
  ###either use knots here of use df in bs()
  # knots=quantile(x,c(1/3,2/3))
  ###fit the model here bs() is used for regression spline
  fit = glm(y~bs(x,df=4))
  ###new data is created for testing
  x0 = seq(min(x),max(x),len=101)
  #### plot the regression line
  llines(x0,predict(fit,data.frame(x=x0)),col="red")
  knots=attr(bs(x,df=4),"knots")##id df is given use this knots
  for(i in 1:length(knots)){
    lsegments(x0=knots[1],y0=min(y),x1=knots[i],y1=max(y), col="black",lty=2)
  }
  
}
##plot the fit
xyplot(nox~dis,Boston,panel=myPanel)

#################           Natural spline of df=5  ############################
###my panel pick from above
panel.smooth <- function(x,y,...)
{
  fit = lm(y~ns(x,df=5))
  x0 = seq(min(x),max(x),len=101)
  llines(x0,predict(fit,data.frame(x=x0)),col="black",lwd=3)
}
xyplot(spnbmd~age|gender,bone,panel=myPanel)


#######################Smooth splines using CV#################
plot(acceleration~horsepower, data=Auto,xlab = "Horsepower",ylab = "Acceleration")
smoothing.param=smooth.spline(horsepower,acceleration,cv=T)
###lambda for the spline
smoothing.param$lambda
summary(smoothing.param)
smoothing.param$df
###plot the spline line
lines(smoothing.param, col="blue")
#######################Smooth splines using df#################

smoothing.param=smooth.spline(horsepower,acceleration,df=4)




#########################Polynomial fit multiple values
###
###cv.err2 = NULL
cv.error=seq(1,10)
color=c("red","blue","green","yellow","black","grey","orange","brown","pink","purple")
plot(dis, nox)
for (i in 1:10){
  fit = glm(nox~poly(dis,i),data=Boston)
  lines(sort(dis), fitted(fit)[order(dis)], col=color[i])
  cv.error[i]=cv.glm(Boston,glm.fit,K=5)$delta[1]
  ##cv.err = sum(fit$residuals^2)###get residual sum of sqs
  
} 
cv.error
point_min=which.min(cv.error)
plot(1:10, cv.error, xlab = "Degree", ylab = "Test MSE", type = "l")
points(point_min,cv.error[point_min], col="red")









