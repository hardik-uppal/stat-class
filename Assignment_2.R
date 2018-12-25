
#2. Consider the data set Auto in the R package ISLR. We wish to develop a model to
#predict whether or not a given car gets high or low gas mileage.
#(a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value above
#this median, and a 0 contains a value below its median.

library(ISLR)
autodf=data.frame(Auto)
autodf$name= NULL
autodf$origin= NULL
autodf$mpg01 = 0
autodf$mpg01[autodf$mpg>=median(autodf$mpg)] = 1
summary(autodf)
attach(autodf)

#(b) Explore the data graphically in order to investigate the association between mpg01
#and the other features. Which of the other features seem most likely to be useful
#in predicting mpg01? Scatterplots and boxplots may be useful tools to answer
#this question. Describe your findings.

#Scatterplots
pairs(autodf)


#Boxplots
par(mfrow=c(2,3))
boxplot(cylinders ~ mpg01, data = autodf, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = autodf, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = autodf, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = autodf, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = autodf, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = autodf, main = "Year vs mpg01")


cor(autodf)

##(c) Split the data into a training set and a test set.

# n = dim(autodf)[1]
# autodf2= autodf[sample(1:n),]
# 
# train = autodf2[1:(n*0.7),]
# test = autodf2[(n*0.7):n,]
set.seed(121)

sample = sample.int(n = nrow(autodf), size = floor(0.75*nrow(autodf)),replace = F)
train = autodf[sample,]
test = autodf[-sample,]
 
# (d) Perform LDA on the training data to predict mpg01 using the variables that
# seemed most associated with mpg01 in (b). Report all the parameter estimates.
# What is the test error of the model obtained?
library(MASS)
model_lda = lda(mpg01~cylinders+horsepower+displacement+weight, data = train)
model_lda

pred_lda = predict(model_lda,test)
names(pred_lda)

table(pred_lda$class,test$mpg01)

mean(pred_lda$class!=test$mpg01)

# (e) Perform QDA on the training data to predict mpg01 using the variables that
# seemed most associated with mpg01 in (b). Report all the parameter estimates.
# What is the test error of the model obtained?
 
model_qda = qda(mpg01~cylinders+horsepower+weight+displacement, data = train)
model_qda

pred_qda= predict(model_qda,test)
names(pred_qda)

table(pred_qda$class,test$mpg01)
mean(pred_qda$class!=test$mpg01)

# (f) Perform logistic regression on the training data to predict mpg01 using the vari-
#   ables that seemed most associated with mpg01 in (b). Report all the parameter
# estimates. What is the test error of the model obtained?

model_logit = glm(mpg01~cylinders+displacement+horsepower+weight, family = binomial(link = 'logit'),data = train)

model_logit

pred_logit=predict(model_logit,test)
names(pred_logit)

pred_logit = as.numeric(pred_logit>0.5)

table(pred_logit,test$mpg01)
mean(pred_logit!=test$mpg01)
# (g) Using logistic regression, LDA, and QDA to estimate the probability that a dodge
# challenger se car with the following setting (cylinders = 6, displacement = 400,
# horsepower = 110, weight = 3000, acceleration = 15, year = 75, origin = 1)) gets
# high gas mileage.
new_test= data.frame(cylinders = 6, displacement = 400, horsepower = 110, weight = 3000, acceleration = 15, year = 75, origin = 1)

new_pred_lda = predict(model_lda,new_test)
new_pred_lda$class
new_pred_lda$posterior

new_pred_qda = predict(model_qda,new_test)
new_pred_qda$posterior
new_pred_qda$class

new_pred_logit= predict(model_logit,new_test, type= "response")
as.numeric(new_pred_logit>0.5)


##(h) (Graduate students only) Perform KNN on the training data, with several values
##of K, in order to predict mpg01. Use only the variables that seemed most asso-
##ciated with mpg01 in (b). What test errors do you obtain? Which value of K
##seems to perform the best on this data set?
library(class)
train.X = cbind(cylinders,horsepower,weight,displacement)[sample,]
test.X = cbind(cylinders,horsepower,weight,displacement)[-sample,]
train.mpg01 = mpg01[sample]
test.mpg01 = mpg01[-sample]

knn.pred = knn(train.X,test.X,train.mpg01,k=1)
table(knn.pred,test.mpg01)
mean(knn.pred!=test.mpg01)

knn.pred = knn(train.X,test.X,train.mpg01,k=2)
table(knn.pred,test.mpg01)
mean(knn.pred!=test.mpg01)

knn.pred = knn(train.X,test.X,train.mpg01,k=10)
table(knn.pred,test.mpg01)
mean(knn.pred!=test.mpg01)

knn.pred = knn(train.X,test.X,train.mpg01,k=100)
table(knn.pred,test.mpg01)
mean(knn.pred!=test.mpg01)



#3. Consider a dataset (X; Y ) in which Y is output and X represents inputs. Let n be
#the number of observations and p be the number of inputs in the dataset. Consider a
#special case n = p = 1. The ridge regression aims to minimize


par(mfrow=c(2,2))
y=10
lambda=9
plot_ridge = function(y, lambda) {
  curve((y - x)^2 + (lambda*x^2),from=1, to= 10,xlab="beta",ylab="RSS")
}

ridge = function(beta){
  return ((y - beta)^2 + (lambda*beta^2))
}

val_ridge = lapply(1:10,ridge)
ridge_optimise = optimise(ridge,1:10)
plot_ridge(10,9)
points(ridge_optimise$minimum,ridge_optimise$objective)
print(ridge_optimise$minimum)








#lasso
y=32
lambda=5
plot_lasso = function(y, lambda) {
  curve((y - x)^2 + (lambda*abs(x)),from=1, to= 10,xlab="beta",ylab="Ridge Expression", col= "Red")
}
ridge = function(beta){
  return ((y - beta)^2 + (lambda*abs(beta)))
}
val_lasso = lapply(1:10,ridge)
lasso_optimise = optimise(ridge,1:10)
plot_lasso(10,9)
points(lasso_optimise$minimum,lasso_optimise$objective)
print(lasso_optimise$minimum)




##(b) Choose a few random values of Y , and for each value of Y , plot ^ ridge and ^ lasso
##on the same axes, as functions of . Describe the observations from the plots.
y=1
beta_ridge = function(lambda){y/(1+lambda)}
beta_lasso = function(lambda) {
  comp=lambda/2
  if(y > comp){
    y-comp
  }
  else if(y < -comp){
    y+comp
  }
  else{
    0
  }
}

lambda = seq(-50,50,1)
matplot(lambda,cbind(beta_ridge(lambda),beta_lasso(lambda)),type="l",col=c("green","red"), xlab = "Lambda", ylab = "Beta_hat", main = "For Y=1")

# 
# 4. Generate the data fXi1;Xi2;Xi3; Yig200
# i=1 using the model Yi = 10 + 1:5Xi1 􀀀 0:3Xi2 +
#   10:7Xi3 + i, where Xi1  Unif(0; 4);Xi2  Unif(3; 8);Xi3  Unif(􀀀1; 5); i 
# N(0; 3). Now add ve more predictor variables Z1 = 1:5X1X2;Z2 = 􀀀3:6X1X3;Z3 =
#   X2X3;Z4  N(20; 40);Z5  N(10; 1)


X1 = runif(200,0,4)
X2 = runif(200,3,8)
X3 = runif(200,-1,5)
ebsilon = rnorm(200,0,3)

Z1=1.5 * X1 * X2
Z2 = -3.6* X1 *X3
Z3 = X2 * X3
Z4 = rnorm(200,20,40)
Z5 = rnorm(200,10,1)

Y = 10+1.5 *X1-0.3*X2+10.7*X3+ebsilon
X=cbind(X1,X2,X3,Z1,Z2,Z3,Z4,Z5)

library("lars")
fit.lars = lars(X,Y, type="lasso",trace=TRUE)
plot(fit.lars)
install.packages("glmnet")
library(glmnet)
fit = glmnet(X,Y)
plot(fit)
cvfit = cv.glmnet(X,Y)
plot(cvfit)
bestlam =cvfit$lambda.min
min(cvfit$cvm)
lasso.coef=predict(fit ,type ="coefficients",s=bestlam )[1:9,]
updated_X=cbind(X1,X2,X3,Z2,Z4,Z5)
fit_updated=glmnet(updated_X,Y)
plot(fit_updated)
# 5. Consider the data set College in the R package ISLR. The response variable is the
# number of applications received and the other variables are the predictors.


library(ISLR)
library(glmnet)
college_df= data.frame(College)
attach(college_df)
set.seed(101)
train=sample.int(n=nrow(college_df),size = floor(nrow(college_df)*0.75),replace = F)
train_df=college_df[train,]
test_df=college_df[-train,]
test.response=Apps[-train]

App_fit=lm(Apps~., data = train_df)

pred_apps=predict(App_fit,test_df)
mean((pred_apps-test.response)^2)
# (c) Fit a ridge regression model on the training set, with  chosen by cross-validation.
# Report the test error obtained.
train_mat = model.matrix(Apps ~ ., data = train_df)
test_mat = model.matrix(Apps ~ ., data = test_df)
grid = 10 ^ seq(4, -2, length = 100)
fit.ridge = glmnet(train_mat, train_df$Apps, alpha = 0, lambda = grid)
cv.ridge = cv.glmnet(train_mat, train_df$Apps, alpha = 0, lambda = grid)
bestlam.ridge = cv.ridge$lambda.min
bestlam.ridge

pred_ridge = predict(fit.ridge, s = bestlam.ridge, newx = test_mat)
mean((pred_ridge - test.response)^2) 

# (d) Fit a lasso model in the training set, with  chosen by cross-validation. Report
# the test error obtained, along with the number of non-zero coecient estimates.

fit.lasso = glmnet(train_mat, train_df$Apps, alpha = 1, lambda = grid)
cv.lasso = cv.glmnet(train_mat, train_df$Apps, alpha = 1, lambda = grid)
bestlam.lasso = cv.lasso$lambda.min
bestlam.lasso


pred_lasso = predict(fit.lasso, s = bestlam.lasso, newx = test_mat)
mean((pred_lasso - test.response)^2)

predict(fit.lasso, s = bestlam.lasso, type = "coefficients")
# (e) Comment on the results obtained. How accurately can we predict the number
# of college applications received? Is there much dierence among the test errors
# resulting from these three approaches?

test_avg <- mean(test_df$Apps)
lm_r2 <- 1 - mean((pred_apps - test_df$Apps)^2) / mean((test_avg - test_df$Apps)^2)
ridge_r2 <- 1 - mean((pred_ridge - test_df$Apps)^2) / mean((test_avg - test_df$Apps)^2)
lasso_r2 <- 1 - mean((pred_lasso - test_df$Apps)^2) / mean((test_avg - test_df$Apps)^2)
