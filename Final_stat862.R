
################## Question 1 ##################
p=5
n=1000
q=4
set.seed(38)

############### either unifrom or normal
# Generate Observations uniform
X=c()
  for(i in 1:p)
  X = cbind(X,runif(n=n, min=1, max=20))
colnames(X)<-paste('X',1:5,sep = "")

# Generate Observations normal for part(d)
X=c()
for(i in 1:p)
  X = cbind(X,rnorm(n,0,1))
colnames(X)<-paste('X',1:5,sep = "")

# Generate Coefficients
betas=c(4,5,-4,5,-1,-4)
eq_x = betas[1] + betas[2]*X[,1] + betas[3]*X[,2] + betas[4]*X[,3] + betas[5]*X[,4] + betas[6]*X[,5] 
pi_x <- exp(eq_x) / (1 + exp(eq_x))
Y <- as.numeric(pi_x>=0.5)
data <- data.frame(Y, X)
############### either unifrom or normal
# Generate Insignificant inputs uniform
Z=c()
for(i in 1:q)
  Z = cbind(Z,runif(n=n, min=1, max=10))
colnames(Z)<-paste('Z',1:4,sep = "")

data <- data.frame(Y, X, Z)

# Generate Insignificant inputs normal for part(d)
Z=c()
for(i in 1:q)
  Z = cbind(Z,rnorm(n,0,1))
colnames(Z)<-paste('Z',1:4,sep = "")

data <- data.frame(Y, X, Z)


#divide data

train_ind <- sample(n, size = floor(0.75 * n))

train <- data[train_ind, ]
test <- data[-train_ind, ]

library(boot)
library(MASS)
cv.error.5.logistic=c()
mse.logistic=c()
cv.error.5.lda=c()
mse.lda=c()
cv.error.5.qda=c()
mse.qda=c()
################## b ################## 
#fit logistic model
glm.fit=glm(Y~.,data=train)
cv.error.5.logistic=cv.glm(train,glm.fit,K=5)$delta[1]
pred.logistic <- predict(glm.fit, test)
pred.logistic = as.numeric(pred.logistic>=0.5)
mse.logistic = mean((pred.logistic != test$Y))
cv.error.5.logistic
mse.logistic
########## LDA #############
cv.err.slr = NULL 
indx = sample(1:length(train$Y),750)
fit.lda = lda(Y~., data = train)
for(l in 1:5)
{
  fit.lda.loop = lda(Y~., data = train, subset = indx[- (((l-1)*150+1):(l*150))])
  pred.lda.loop=predict(fit.lda.loop,new = train[((l-1)*150+1):(l*150),-1])
  cv.err.slr = c(cv.err.slr, mean( train$Y[((l-1)*150+1):(l*150)] != pred.lda.loop$class))
} 
cv.error.5.lda=mean(cv.err.slr)
pred.lda = predict(fit.lda, test)
mse.lda = mean((pred.lda$class != test$Y))
cv.error.5.lda
mse.lda
########## QDA #############
cv.err.slr = NULL 
indx = sample(1:length(train$Y),750)
fit.qda = qda(Y~., data = train)
for(m in 1:5)
{
  fit.qda.loop = qda(Y~., data = train, subset = indx[- (((m-1)*150+1):(m*150))])
  pred.qda.loop=predict(fit.qda.loop,new = train[((m-1)*150+1):(m*150),-1])
  cv.err.slr = c(cv.err.slr, mean( train$Y[((m-1)*150+1):(m*150)] != pred.qda.loop$class))
} 
cv.error.5.qda=mean(cv.err.slr)
pred.qda = predict(fit.qda, test)
mse.qda = mean((pred.qda$class != test$Y))
cv.error.5.qda
mse.qda

################## c ################## 

########################### Logistic part ######################
i=NULL
for (i in 1:100){
  # Generate Insignificant inputs uniform
  Z=c()
  for(k in 1:q)
    Z = cbind(Z,runif(n=n, min=1, max=10))
  colnames(Z)<-paste('Z',1:4,sep = "")
  
  data <- data.frame(Y, X, Z)
  #generate train and test
  train_ind <- sample(n, size = floor(0.75 * n))
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  #fit logistic model
  glm.fit=glm(Y~.,data=train)
  cv.error.5.logistic[i]=cv.glm(train,glm.fit,K=5)$delta[1]
  pred.logistic <- predict(glm.fit, test)
  pred.logistic = as.numeric(pred.logistic>=0.5)
  mse.logistic[i] = mean((pred.logistic != test$Y))
  
}



par(mfrow=c(1,2))
boxplot(cv.error.5.logistic,xlab = "CV error")
boxplot(mse.logistic,xlab ="MSE")
########################### LDA part ######################
i=NULL
for (i in 1:100){
  
  # Generate Insignificant inputs uniform
  Z=c()
  for(k in 1:q)
    Z = cbind(Z,runif(n=n, min=1, max=10))
  colnames(Z)<-paste('Z',1:4,sep = "")
  
  data <- data.frame(Y, X, Z)
  #generate train and test
  train_ind <- sample(n, size = floor(0.75 * n))
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  ########## LDA #############
  cv.err.slr = NULL 
  indx = sample(1:length(train$Y),750)
  fit.lda = lda(Y~., data = train)
  for(l in 1:5)
  {
    fit.lda.loop = lda(Y~., data = train, subset = indx[- (((l-1)*150+1):(l*150))])
    pred.lda.loop=predict(fit.lda.loop,new = train[((l-1)*150+1):(l*150),-1])
    cv.err.slr = c(cv.err.slr, mean( train$Y[((l-1)*150+1):(l*150)] != pred.lda.loop$class))
  } 
  cv.error.5.lda[i]=mean(cv.err.slr)
  pred.lda = predict(fit.lda, test)
  mse.lda[i] = mean((pred.lda$class != test$Y))
  
}

par(mfrow=c(1,2))
boxplot(cv.error.5.lda,xlab = "CV error")
boxplot(mse.lda,xlab ="MSE")

########################### QDA part ######################
i=NULL
for (i in 1:100){
  
  # Generate Insignificant inputs uniform
  Z=c()
  for(k in 1:q)
    Z = cbind(Z,runif(n=n, min=1, max=10))
  colnames(Z)<-paste('Z',1:4,sep = "")
  
  data <- data.frame(Y, X, Z)
  #generate train and test
  train_ind <- sample(n, size = floor(0.75 * n))
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  ########## QDA #############
  cv.err.slr = NULL 
  indx = sample(1:length(train$Y),750)
  fit.qda = qda(Y~., data = train)
  for(m in 1:5)
  {
    fit.qda.loop = qda(Y~., data = train, subset = indx[- (((m-1)*150+1):(m*150))])
    pred.qda.loop=predict(fit.qda.loop,new = train[((m-1)*150+1):(m*150),-1])
    cv.err.slr = c(cv.err.slr, mean( train$Y[((m-1)*150+1):(m*150)] != pred.qda.loop$class))
  } 
  cv.error.5.qda[i]=mean(cv.err.slr)
  pred.qda = predict(fit.qda, test)
  mse.qda[i] = mean((pred.qda$class != test$Y))
  
}
par(mfrow=c(1,2))
boxplot(cv.error.5.qda,xlab = "CV error")
boxplot(mse.qda,xlab ="MSE")

#################### D ##################
########################### Logistic part ######################
i=NULL
for (i in 1:100){
  # Generate Insignificant inputs normal for part(d)
  Z=c()
  for(m in 1:q)
    Z = cbind(Z,rnorm(n,0,1))
  colnames(Z)<-paste('Z',1:4,sep = "")
  
  data <- data.frame(Y, X, Z)
  #generate train and test
  train_ind <- sample(n, size = floor(0.75 * n))
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  #fit logistic model
  glm.fit=glm(Y~.,data=train)
  cv.error.5.logistic[i]=cv.glm(train,glm.fit,K=5)$delta[1]
  pred.logistic <- predict(glm.fit, test)
  pred.logistic = as.numeric(pred.logistic>=0.5)
  mse.logistic[i] = mean((pred.logistic != test$Y))
  
}



par(mfrow=c(1,2))
boxplot(cv.error.5.logistic,xlab = "CV error")
boxplot(mse.logistic,xlab ="MSE")
########################### LDA part ######################
i=NULL
for (i in 1:100){
  
  # Generate Insignificant inputs normal for part(d)
  Z=c()
  for(m in 1:q)
    Z = cbind(Z,rnorm(n,0,1))
  colnames(Z)<-paste('Z',1:4,sep = "")
  
  data <- data.frame(Y, X, Z)
  #generate train and test
  train_ind <- sample(n, size = floor(0.75 * n))
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  ########## LDA #############
  cv.err.slr = NULL 
  indx = sample(1:length(train$Y),750)
  fit.lda = lda(Y~., data = train)
  for(l in 1:5)
  {
    fit.lda.loop = lda(Y~., data = train, subset = indx[- (((l-1)*150+1):(l*150))])
    pred.lda.loop=predict(fit.lda.loop,new = train[((l-1)*150+1):(l*150),-1])
    cv.err.slr = c(cv.err.slr, mean( train$Y[((l-1)*150+1):(l*150)] != pred.lda.loop$class))
  } 
  cv.error.5.lda[i]=mean(cv.err.slr)
  pred.lda = predict(fit.lda, test)
  mse.lda[i] = mean((pred.lda$class != test$Y))
  
}

par(mfrow=c(1,2))
boxplot(cv.error.5.lda,xlab = "CV error")
boxplot(mse.lda,xlab ="MSE")

########################### QDA part ######################
i=NULL
for (i in 1:100){
  
  # Generate Insignificant inputs normal for part(d)
  Z=c()
  for(m in 1:q)
    Z = cbind(Z,rnorm(n,0,1))
  colnames(Z)<-paste('Z',1:4,sep = "")
  
  data <- data.frame(Y, X, Z)
  #generate train and test
  train_ind <- sample(n, size = floor(0.75 * n))
  
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  ########## QDA #############
  cv.err.slr = NULL 
  indx = sample(1:length(train$Y),750)
  fit.qda = qda(Y~., data = train)
  for(m in 1:5)
  {
    fit.qda.loop = qda(Y~., data = train, subset = indx[- (((m-1)*150+1):(m*150))])
    pred.qda.loop=predict(fit.qda.loop,new = train[((m-1)*150+1):(m*150),-1])
    cv.err.slr = c(cv.err.slr, mean( train$Y[((m-1)*150+1):(m*150)] != pred.qda.loop$class))
  } 
  cv.error.5.qda[i]=mean(cv.err.slr)
  pred.qda = predict(fit.qda, test)
  mse.qda[i] = mean((pred.qda$class != test$Y))
  
}
par(mfrow=c(1,2))
boxplot(cv.error.5.qda,xlab = "CV error")
boxplot(mse.qda,xlab ="MSE")

###################### Question 2 ###################################
##################  a #################

n = 6000
T_from_Norm = function (n, v) 
{
  vctr_1 = vector("numeric", n)
  vctr_1[1] = 1
  for (i in 2:n) {
    y = rnorm(1)
    
    aprob = min(1, (dt(y, v)*dnorm(vctr_1[i-1]))/dt(vctr_1[i-1], v)/dnorm(y))
    u = runif(1)
    if (u < aprob) 
      vctr_1[i] = y
    else 
      vctr_1[i] = vctr_1[i-1]
  }
  return(vctr_1)
}
t_vctr = T_from_Norm(n,6)

mean_dist_norm = mean(t_vctr)




vec_actual = rt(n, 6)
mean_actual = mean(vec_actual)

mean_dist_norm
mean_actual

par(mfrow=c(2,1))
plot(ts(t_vctr))
hist(t_vctr)



T_from_T = function (n, v) 
{
  vctr_1 = vector("numeric", n)
  vctr_1[1] = 0
  for (i in 2:n) {
    y = rnorm(1)
    
    aprob = min(1, (dt(y, v)*dt(vctr_1[i-1], 3))/dt(vctr_1[i-1], v)/dt(y, 3))
    u = runif(1)
    if (u < aprob) 
      vctr_1[i] = y
    else 
      vctr_1[i] = vctr_1[i-1]
  }
  return(vctr_1)
}
t_vctr2 = T_from_T(n,6)

mean_dist_t = mean(t_vctr2)




mean_dist_t
mean_actual

par(mfrow=c(2,1))
plot(ts(t_vctr2))
hist(t_vctr2)



################## b #################
estint=cumsum(t_vctr)/(1:n)
esterr=sqrt(cumsum((t_vctr-estint)^2))/(1:n)
plot(estint, xlab="Mean and error range T from Normal",type="l",lwd=2,ylim=mean(t_vctr)+20*c(-esterr[n],esterr[n]),ylab="")
lines(estint+2*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)
length(unique(t_vctr))/n

estint=cumsum(t_vctr2)/(1:n)
esterr=sqrt(cumsum((t_vctr2-estint)^2))/(1:n)
plot(estint, xlab="Mean and error range T from T",type="l",lwd=2,ylim=mean(t_vctr2)+20*c(-esterr[n],esterr[n]),ylab="")
lines(estint+2*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)

length(unique(t_vctr2))/n

###################### Question 3 ###################################
######################### c ####################################
x=rnorm(100,0,1)
a = 3
b = 3
tau2 = 10
mu0 = 5
n=length(x)
Nsim=10000

result_mat = matrix(nrow = Nsim, ncol = 2)

x_bar=mean(x)

sigma2 = rep(0,Nsim)
sigma2[1]=1/rgamma(1,shape=a,rate=b)

B=sigma2[1]/(sigma2[1]+n*tau2)

mu = rep(0,Nsim)
mu[1]=rnorm(1,m=B*mu0+(1-B)*x_bar,sd=sqrt(tau2*B))

result_mat[1, ] = c(mu[1], sigma2[1])

for (i in 2:Nsim){
  B=sigma2[i-1]/(sigma2[i-1]+n*tau2)
  mu[i]=rnorm(1,m=B*mu0+(1-B)*x_bar,sd=sqrt(tau2*B))
  
  sigma2[i]=1/rgamma(1,shape=(n/2)+a,rate=(1/2)*(sum((x-mu[i])^2))+b)
  
  result_mat[i, ] <- c(mu[i], sigma2[i]) 
}

result_mat=result_mat[-(1:2500),]

colSums(result_mat)/7500



############################ Question 5 ##########################
age = seq(23,40)
home_runs = c(1,18,36,38,38,38,21,45,48,31,35,40,36,33,37,35,12,6)
at_bats = c(34,367,568,562,584,544,513,541,548,354,514,534,528,549,552,522,390,148)
data_q4 = data.frame(home_runs,age,at_bats) 
 # beta = c(30.4444,-0.9086,-42.9183)
# function to compute the logarithm of the posterior density with the tranformed parameters
betapost=function (beta, data) 
{
  x = data[, 2]
  n = data[, 3]
  y = data[, 1]
  
  beta0 = beta[1]
  beta1 = beta[2]
  beta2 = beta[3]
  
  logf=function(x,n,y,beta0,beta1,beta2)
  {  
    
    lp = beta0 + beta1 * x + beta2 * (x)^2
    
    p = exp(lp)/(1 + exp(lp))
  
    return(y * log(p) + (n - y) * log(1 - p))
  }
  
  return(sum(logf(x,n,y,beta0,beta1,beta2)))
}

################ b  ################
x = data_q4[, 2]
n = data_q4[, 3]
y = data_q4[, 1]
response=cbind(y, n - y)
x2=x^2
resp_fit=glm(response~ x + x2,data_q4,family = binomial)
fit = laplace(betapost,c(0,0,0),data_q4)
fit$mode
fit$var
################ c  ################

proposal=list(var=fit$var,scale=20 )

bayesfit=rwmetrop(betapost,proposal,fit$mode,100000,data_q4)

bayesfit$accept

