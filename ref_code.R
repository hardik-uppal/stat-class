##lasso solution path
##smoothing reg coefficients prediciton
##legend
##idence interval for the log-odds-ratio o

### Load dataset and divide it in train and test data
library(ISLR)
attach(College)
#insert dataset here
Name_df=data.frame(read.csv("bone.txt",sep=""))
Name_df=read.table("bone.txt",header = T)
n = length(Apps)
train_num = sample(n,floor(n*0.75))
train_College = College[train_num,]
test_College = College[-train_num,]
###### Create a categorical variable 
autodf$mpg01 = 0
autodf$mpg01[autodf$mpg>=median(autodf$mpg)] = 1

###################### LDA  ###############
##create a variable as categorical to use  this as in case of mpg01
fit.lda <- lda(mpg01~., data = Auto_train)
fit.lda 

pred.lda <- predict(fit.lda, Auto_test)
table(pred.lda$class, mpg01[-train_num])
mean(pred.lda$class != mpg01[-train_num])


#############################       QDA ###############
fit.qda <- qda(mpg01~., data = train_College)
fit.qda

pred.qda <- predict(fit.qda, Auto_test)
table(pred.qda$class, mpg01[-train_num])
mean(pred.qda$class != mpg01[-train_num])

########################### logistic regression##############################
fit.glm <- glm(mpg01~., data = train_College, family = binomial)
summary(fit.glm)

prob_logit <- predict(fit.glm, Auto_test, type = "response")
pred.glm = as.numeric(prob_logit>=0.5)
table(pred.glm, mpg01[-sp])
mean(pred.glm != mpg01[-sp])

###############################################  knn #############################################
require(class)
mpg01.knn= rep(0,100)
Auto_test$origin = as.numeric(Auto_test[,8])
Auto_training$origin = as.numeric(Auto_training[,8])
for (k in 1:100){
  knn.fit = knn( Auto_training[,c(2:5,7,8)], Auto_test[,c(2:5,7,8)], Auto_training[,9], k )
  mpg01.knn[k] = mean( knn.fit == Auto_test[,9] )
}
which.max(mpg01.knn)
max(mpg01.knn)


############################# ridge regression ########################################
#covert categorical variables to numeric
train_College$Private = as.numeric(train_College$Private)
##create a matrix of predictors leaving out response variable
X = as.matrix(train_College[,-2])
y = train_College[,2] ##2 is the response variable
cv.ridge = cv.glmnet(X,y, alpha=0, lambda=seq(0,20,0.1) )##lambda is calculated through cross validation
cv.ridge$lambda.min##mininmum is taken
plot(cv.ridge)
ridge = glmnet( X, y, alpha=0, lambda=cv.ridge$lambda.min )##model is fitted here with lambda calulated
coef(ridge)
##reposne variable is created as catagorical for test data
test_College$Private = as.numeric(test_College$Private)
newX = as.matrix(test_College[-2])
###predict response variable
Yhat.ridge = predict( ridge, cv.ridge$lambda.min, newx = newX )
##calculate test error
mean( (Yhat.ridge- test_College$App)^2 )
sum((Yhat.ridge- test_College$App)^2 )/(nrow(test_College)-1)


##################### (d) lasso regression##############################
cv.lasso= cv.glmnet(X,y, alpha=1, lambda=seq(0,40,0.1) )##lambda is calculated through cross validation
cv.lasso$lambda.min
plot(cv.lasso)
lasso = glmnet( X, y, alpha=1, lambda=cv.ridge$lambda.min )##model is fitted here with lambda calulated
coef(lasso)
##reposne variable is created as catagorical for test data
test_College$Private = as.numeric(test_College$Private)
newX = as.matrix(test_College[,-2])
###response variable
Yhat.lasso = predict( lasso, cv.lasso$lambda.min, newx = newX )
###test error
mean( (Yhat.lasso- test_College$App)^2 )
sum((Yhat.lasso- test_College$App)^2 )/(nrow(test_College)-1)



#################################  Best subset selection ##############################

# nvmax: maximum size of subsets to examine ,should be less than total number of variable
#all the models are fitted
All_fitted_models=regsubsets(Apps~., data=train_College,nvmax=15)
summary(All_fitted_models)
#save the summary in a variable
reg.summary=summary(leaps)
##now calculate best subset of all the combinations of model defined above 
#use R^2 BIC and CP to calculate the best subset

### the adjusted R^2 of the top 15 models 
reg.summary$adjr2
which.max(reg.summary$adjr2)

### the Cp of the top 15 models
reg.summary$cp
which.min(reg.summary$cp)
### the bic of the top 15 models
reg.summary$bic
which.min(reg.summary$bic)
##############################  plot of RSS, adjusted R^2, Cp and BIC together ###############
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Predictors", ylab="Residual Sum of Squares", type="l", xlim=c(0,11), ylim=c(min(reg.summary$rss), max(reg.summary$rss)))
points(which.min(reg.summary$rss), reg.summary$rss[which.min(reg.summary$rss)], cex=2, pch=20, col="red")

plot(reg.summary$cp, xlab="Number of Predictors", ylab="Cp", type="l", xlim=c(0,11), ylim=c(min(reg.summary$cp),max(reg.summary$cp)))
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], cex=2, pch=20, col="red")

plot(reg.summary$adjr2, xlab="Number of Predictors", ylab="Adjusted R Square", type="l", xlim=c(0,11), ylim=c(0,1))
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], cex=2, pch=20, col="red")

plot(reg.summary$bic, xlab="Number of Predictors", ylab="BIC", type="l", xlim=c(0,11))
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)], cex=2, pch=20, col="red")


############################stepwise selection using AIC ###############
### define null model
fit0<-lm(Apps~1,data=train_College)
##define fit_all
fit_all<-lm(Apps~ .,data=train_College)
### forward selection
fit.forward<-step(fit0,scope=list(lower=Apps~1, upper=fit_all),direction='forward')
summary(fit.forward)

### backward selection
fit.backward<-step(fit1,scope=list(lower=lpsa~1, upper=fit_all),direction='backward')
summary(fit.backward) 
