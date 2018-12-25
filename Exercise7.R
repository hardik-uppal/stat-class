###  Question 1
library(ISLR)
data(Default)


# (a)
model_logit = glm(default ~ income+balance , family = binomial(link='logit'), data=Default)


#(b)
summary(model_logit)


# (c)
B = 1000
est = matrix(0,B,3)
for( i in 1:B)
{

subset = sample(1:nrow(Default),nrow(Default), replace=T)
subset_Default = Default[subset,]
model_logit = glm(default ~ income+balance , family = binomial(link='logit'), data=subset_Default)
est[i,] = coef(model_logit)

}

apply(est,2,sd)


### Question 2
B = 100

## true model
p = 3
beta = vector("numeric",p+1)
beta = c(-2,4.5,10)


beta_logit = matrix(0,B,p)
beta_lda = matrix(0,B,p)
 
library(MASS)

n = 100
for(i in 1:B)
{
 ## generate X and Y
   X = matrix(rnorm(n*p,1,2),n,p, byrow=T)
   reg = cbind(rep(1,n),X)%*% beta
   prob = exp(reg)/(1+exp(reg))
   Y =as.numeric( prob>0.5)
   dataset = as.data.frame(cbind(X,Y))

 ## obtain beta hat based on each of logistic, LDA 
   model_logit = glm(Y ~ . , family = binomial(link='logit'), data=dataset)
   beta_logit[i,] = coef(model_logit)[-1]

   model_lda = lda(Y~.,data=dataset)
   ## compute the estimated regression coefficient based on pages 28 and 30 of unit4.pdf
   beta_lda[i,] =   


}

### make comparison by making the boxplots of beta_logit, beta_lda
