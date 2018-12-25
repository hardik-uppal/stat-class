

######Q 1#######

#### part a
generate_beta= function(n,alpha,beta)
{
  X1 = rgamma(n,alpha,1)
  X2 = rgamma(n,beta,1)
  X = X1/(X1+X2)
  return(X)
}


X0 = seq(0,1,length=1000)
par(mfrow= c(2,2))
alpha_and_beta = cbind(c(1,2,3,4),c(4,3,2,1))

## make plots
n = 10000
for (i in 1:nrow(alpha_and_beta)) {
  X = generate_beta(n,alpha_and_beta[i,1],alpha_and_beta[i,2])
  hist(X,freq = FALSE,main = paste("alpha =", alpha_and_beta[i,1], "and beta=",alpha_and_beta[i,2]))
  dX0 = dbeta(X0,alpha_and_beta[i,1],alpha_and_beta[i,2])
  lines(X0,dX0)
}


#### part b

install.packages("msm")
library(msm)
n = 10000
alpha_beta = cbind(c(1,2,3,4),c(4,3,2,1))

## the proposed distribution: uniform distribution
par(mfrow=c(2,2))
for (i in 1:nrow(alpha_beta)) {
  
  
  alpha = alpha_beta[i,1]
  beta = alpha_beta[i,2]
  X0 = seq(0,1,length=1000)
  
  xmax = (alpha-1)/(alpha+beta-2)
  # print approximate c and theoretical c
  print(c(max( X0^(alpha-1)*(1-X0)^(beta-1)/beta  (alpha,beta)),xmax^(alpha-1)*(1-xmax)^(beta-1)/beta(alpha,beta)))
  c_unif = xmax^(alpha-1)*(1-xmax)^(beta-1)/beta(alpha,beta)
  
  
  # generate x by rejection sample 
  x_unif = NULL
  for(j in 1:n)
  {
    u = runif(1)
    x = runif(1)
    if(u< dbeta(x,alpha,beta)/(c_unif*dunif(x))) x_unif = c(x_unif,x)    
  }
  
  # make plots
  hist(x_unif,freq = FALSE,main = paste("alpha=",alpha, ",beta =",beta),30)
  dX0 = dbeta(X0,alpha_beta[i,1],alpha_beta[i,2])
  lines(X0,dX0)
}

## the proposed distribution: truncated normal distribution
musigma = cbind(c(0,0,0.5,0.5),c(1,2,1,2))
par(mfrow=c(4,4))
install.packages("truncnorm")
library(truncnorm)
for (i in 1:nrow(alpha_beta)) {
  
  alpha = alpha_beta[i,1]
  beta = alpha_beta[i,2]
  ## numerical way to approximate c
  accept_tnorm= matrix(0,n,4)
  y_tnorm = matrix(0,n,4)
  for(q in 1:nrow(musigma))
  {
    X0 = seq(0,1,length=1000)
    mu = musigma[q,1]
    sigma = musigma[q,2]
    fx = dbeta(X0,alpha,beta)
    gx = dtruncnorm(X0,a=0,b=1)
    c_tnorm = max(fx/gx)
    y_tnorm[,q] = rtruncnorm(n,a=0,b=1)
    for(j in 1:n)
    {
      u = runif(1)
      x = y_tnorm[j,q] 
      if(u<= dbeta(x,alpha,beta)/(c_tnorm*dtruncnorm(x,a=0,b=1 ))) 
        accept_tnorm[j,q]=1  
    }
    mean(accept_tnorm[,q])
    # make plots
    
    hist(y_tnorm[accept_tnorm[,q]==1,q],freq = FALSE,30,main = paste("alpha =",alphabeta[i,1],",beta =",alphabeta[i,2],",mu=",mu,",sigma =",sigma),xlab="x")
    dX0 = dbeta(X0,alpha_beta[i,1],alpha_beta[i,2])
    lines(X0,dX0)
  }
  
}




#################################### 2_a ################################


set.seed(0)
a_func <- function (x) {
  ifelse(x > 0, (exp(-sqrt(x) - 0.5 * x) * sin(x) ^ 2), 0)
}
a_func1 <- function (x) {
  ifelse(x > 0, (exp(-sqrt(x) - 0.5 * x) * sin(x) ^ 2)/(0.5*exp(-0.5*x)), 0)
}

# M is the number of samples
M=100#for M=100,500,1000,2000
X = rexp(M,rate=0.5)

I = mean(a_func1(X))
I
actual_value=integrate(a_func, lower = 0, upper = Inf)

#################################### 2_b_a ################################
library(rmutil)
M_list=c(100,500,1000,2000)
#est_a=c()
#est_sd_a=c()
for (i in 1:length(M_list)) {
  M=M_list[i]#for M=100,500,1000,2000
  x1 = rlaplace(n=M,m=0,s=1)
  x1[x1<0]=0
  est_a[i]=mean(a_func(x1)/ dlaplace(x1,m=0,s=1))
  
  est_sd_a[i]=(sd(a_func(x1)/ dlaplace(x1,m=0,s=1))/sqrt(M))
}

plot(M_list,est_a,ylab = "Estimates",xlab = "No of Samples")
lines(M_list,est_a)
lines(M_list,rep(actual_value$value,4))
plot(M_list,est_sd_a,ylab = "SD",xlab = "No of Samples")
lines(M_list,est_sd_a)




#################################### 2_b_b ######################################################

est_b=c()
est_sd_b=c()
for (i in 1:length(M_list)) {
  M=M_list[i]#for M=100,500,1000,2000
  x2 = rcauchy(M,l=0,s=2)
  x2[x2<0]=0
  est_b[i]=mean(a_func(x2)/ dcauchy(x2,location =0,scale =2))
  
  est_sd_b[i]=(sd(a_func(x2)/ dcauchy(x2,location =0,scale =2))/sqrt(M))
}

plot(M_list,est_b,ylab = "Estimates",xlab = "No of Samples")
lines(M_list,est_b)
lines(M_list,rep(actual_value$value,4))
plot(M_list,est_sd_b,ylab = "SD",xlab = "No of Samples")
lines(M_list,est_sd_b)






############################ 2_b_c ################################

est_c=c()
est_sd_c=c()
for (i in 1:length(M_list)) {
  M=M_list[i]#for M=100,500,1000,2000
  x3 = rnorm(M,mean=0,sd=1)
  x3[x3<0]=0
  est_c[i]=mean(a_func(x3)/ dnorm(x3,mean=0,sd=1))
  
  est_sd_c[i]=(sd(a_func(x3)/ dnorm(x3,mean=0,sd=1))/sqrt(M))
}

plot(M_list,est_c,ylab = "Estimates",xlab = "No of Samples")
lines(M_list,est_c)
lines(M_list,rep(actual_value$value,4))
plot(M_list,est_sd_c,ylab = "SD",xlab = "No of Samples")
lines(M_list,est_sd_c)





############################ 2_c ################################

library(LaplacesDemon)
p <- c(0.3,0.7)
mu <- c(-1, 6)
sigma <- c(1,2)
x4=rnormm(10000, p, mu, sigma)

mean(a_func(x4)/dnormm(x4, p, mu, sigma))
############################ Q3 ################################

install.packages("purrr") 
library(purrr)

# a
binom_genrt = function (n, p, t) 
{
  vec = vector("numeric", t)
  vec[1] = n*p
  for (i in 2:t) {
    y = rdunif(1, 1, n)
    aprob <- min(1, ((dbinom(y, n, p)/dbinom(vec[i-1], n, p))/(dunif(y, 1, n)/dunif(vec[i-1], 1, n))))
    u = runif(1)
    if (u < aprob)
      vec[i] = y
    else
      vec[i] = vec[i-1]
  }
  return(vec)
}

par(mfrow=c(2,1))

new_binomial = binom_genrt(50,0.7,10000)
plot(ts(new_binomial))
hist(new_binomial)

mean_new_binomial = mean(new_binomial)
mean_new_binomial
var_new_binomial = var(new_binomial)
var_new_binomial

theoretical_binomial = rbinom(10000, 50,0.7)
mean_theoretical = mean(theoretical_binomial)
mean_theoretical
var_thearetical = var(theoretical_binomial)
var_thearetical
####
new_binomial2 = binom_genrt(60,0.7,10000)
par(mfrow=c(2,1))
plot(ts(new_binomial2))
hist(new_binomial2)

mean_new_binomial2 = mean(new_binomial2)
mean_new_binomial2 
var_new_binomial2 = var(new_binomial2)
var_new_binomial2

theoretical_binomial2 = rbinom(10000, 60, 0.7)
mean_theoretical2 = mean(theoretical_binomial2)
mean_theoretical2
var_thearetical2 = var(theoretical_binomial2)
var_thearetical2



############# b 
normal_func = function (n, init_val, sigma_1) 
{
  mu = 0
  sigma = 1
  vec = vector("numeric", n)
  vec[1] = init_val
  for (i in 2:n) {
    y = rnorm(1, vec[i-1], sigma_1)
    aprob = min(1, (dnorm(y, mu, sigma)/dnorm(vec[i-1], mu, sigma)/dnorm(y, vec[i-1], sigma_1)/dnorm(vec[i-1], vec[i-1], sigma_1)))
    u = runif(1)
    if (u < aprob)
      vec[i] = y
    else
      vec[i] = vec[i-1]
  }
  return(vec)
}


new_normal = normal_func(100000, 1, 0.25)
mean_new_normal = mean(new_normal)
mean_new_normal
var_new_normal = var(new_normal)
var_new_normal

actual_normal=rnorm(100000, 1, 0.25)
mean_actual_normal = mean(actual_normal)
mean_actual_normal
var_actual_normal = var(actual_normal)
var_actual_normal



par(mfrow=c(2,1))
plot(ts(new_normal))
hist(new_normal,30)


new_normal2 = normal(100000, 1, 0.01)
mean_new_normal2 = mean(new_normal2)
mean_new_normal2
var_new_normal2 = var(new_normal2)
var_new_normal2 

par(mfrow=c(2,1))
plot(ts(new_normal2))
hist(new_normal2,30)


############################ Q4 ################################
library(invgamma)


data = read.table("dataq4.txt",header = T)
# beta_0 hyper parameters (known)


x=data$x
y=data$y


gibbs_gentr = function(y, x, gama0, tau0, gama1, tau1, del, v2, iter = 10000){
  n = length(y)
  sig2 = mean(y)
  result = matrix(nrow = iter, ncol = 3) 
  for (i in 1:iter) {
    b0 = rnorm(1, mean = ((sig2*gama0)/(sig2+n*tau0))+((mean(y)*n*tau0)/(sig2+n*tau0)),
               sd = (sig2*tau0)/(sig2 + n*tau0))
    
    b1 = rnorm(1, mean = ((sig2*gama1)/(sig2+n*tau1))+((mean(y)*n*tau1)/(sig2+n*tau1)), 
               sd =(sig2*tau1)/(sig2 + n*tau1))
    
    sig2 = rinvgamma(1, shape = (n+del)/2, 
                     rate = (((del+v2)/2)+(0.5 * sum((y-b0-b1*x)^2))))
    
    result[i, ] = c(b0, b1, sig2) 
  }
  return(result)
}

gibbs_sample = gibbs_gentr(y,x,0,5,0,5,3,1)

# Drop the burn-in samples
gibbs_sample=gibbs_sample[-(1:2000),]
# posterior means
colSums(gibbs_sample)/8000





