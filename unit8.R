###################### conjugate priors##############################
alpha = 5
beta = 6
theta = seq(0,100, length.out =101)
prior = dgamma(theta, shape = alpha, scale = beta)
x = 42
posterior = dgamma(theta, shape = x+alpha, scale = 1/(1+1/beta) ) 

# make plot
plot(theta, posterior, xlab = expression(theta), ylab = "density")
lines(theta, prior, lty = 3)

# posterior draws
postdraw = rgamma(2000, shape = x+alpha, scale = 1/(1+1/beta) ) 
r1= hist(postdraw, freq = F, breaks = 20, plot =F)
lines(r1, lty=3, freq = F, col = "gray90")

################## a beta-binomial model ###################

# function to compute the logarithm of the posterior density
betabinexch0=function (theta, data)
{
eta = theta[1]
K = theta[2]
y = data[, 1]
n = data[, 2]
N = length(y)
logf = function(y, n, K, eta) lbeta(K * eta + y, K * (1 -
eta) + n - y) - lbeta(K * eta, K * (1 - eta))
val = sum(logf(y, n, K, eta))
val = val - 2 * log(1 + K) - log(eta) - log(1 - eta)
return(val)
}
# read the data
cancermortality = read.table("cancer.txt",header = T)
# contour plot after install LearnBayes package
install.packages("LearnBayes")
library(LearnBayes)
mycontour(betabinexch0,c(.0001,.003,1,20000),cancermortality, xlab=expression(eta),ylab="K")


# function to compute the logarithm of the posterior density with the tranformed parameters
betabinexch=function (theta, data)
{
eta = exp(theta[1])/(1 + exp(theta[1]))
K = exp(theta[2])
y = data[, 1]
n = data[, 2]
N = length(y)
logf = function(y, n, K, eta) lbeta(K * eta + y, K * (1 -
eta) + n - y) - lbeta(K * eta, K * (1 - eta))
val = sum(logf(y, n, K, eta))
val = val + theta[2] - 2 * log(1 + exp(theta[2]))
return(val)
}

# contour plot
mycontour(betabinexch,c(-8,-4.5,3,16.5),cancermortality, xlab="logit eta",ylab="log K")

### estimating the mean of p^2
p<- rbeta(1000,14.26,23.19)
est<-mean(p^2)
se<-sd(p^2)/sqrt(1000)
c(est,se)

## integral of h(x)
h=function(x){(cos(50*x)+sin(20*x))^2}
par(mar=c(2,2,2,1),mfrow=c(2,1))
curve(h,xlab="Function",ylab="",lwd=2)
integrate(h,0,1)
x=h(runif(10^4))
estint=cumsum(x)/(1:10^4)
esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
plot(estint, xlab="Mean and error range",type="l",lwd=2,ylim=mean(x)+20*c(-esterr[10^4],esterr[10^4]),ylab="")
lines(estint+2*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)


##################### Rejection sampling
## Example 1
x = NULL
n = 100
r = 2
for(i in 1:(100*sqrt(2*pi/exp(1))))
{

u = runif(1)
y = rcauchy(1)
if( u <=  exp(-y^2/2)*(1+y^2)*sqrt(exp(1))/2) 
 { x = c(x,y)}
}

# investigate the acceptance rate
N= 1000

c = sqrt(2*pi/exp(1))

# you can identify C using the following code
X = seq(-3,3,0.01)
c = max(dnorm(X)/dcauchy(X))

u = runif(N)
y=rcauchy(N)
accept = ( u < dnorm(y)/(c*dcauchy(y)))
x = y[accept]
mean(accept)
1/c

par(mfrow=c(1,2))
hist(x)
hist(rnorm(100))

##################### importance sampling

######## computing(X>c) for N(0,1) based on N(mu,1)

### function imp returns entries for a row of estimate, standard deviation and confidence interval

imp<-function(mu=0,c=0,n=1000000,X=rnorm(n))
{
  IS = mean((mu+X>c)*exp(mu*(mu-2*(mu+X))/2))
  standev=sd((mu+X>c)*exp(mu*(mu-2*(mu+X))/2))/sqrt(n)
  return(c(mu,IS,standev,IS-qnorm(0.975)*standev,IS+qnorm(0.975)*standev))
}

#fix n and X in advance
n =10000000
X = rnorm(n) 
# case when c = 3
c= 3
imp(0,c,n,X)
imp(1,c,n,X)
imp(2,c,n,X)
imp(3,c,n,X)
imp(3.1,c,n,X)
imp(3.15,c,n,X) # the (near-)optimal value of mu slightly above c
imp(3.2,c,n,X)
imp(4,c,n,X)

# case when c = 4.5
c = 4.5
imp(0,c,n,X)
imp(4.5,c,n,X)
imp(4.6,c,n,X) #we find the (near-)optimal value of mu slightly above c
imp(4.7,c,n,X)

# plot confidence intervals as mu varies
c=3
mu = (0:120)/20 #choose range from 0 to 6 in steps of 0.05
imp3  = matrix(1:605,121,5) #initialize impsave3, to save imp for c=3
for (i in 1:121){imp3[i,] = imp(mu[i],c,n,X)} #save imp in impsave3
plot(mu,imp3[,2],pch=16,xlab=expression(mu),ylab="estimate",ylim=c(0.001335,0.001359)) #estimates
lines(mu,imp3[,4],col="red") #lower CI bound
lines(mu,imp3[,5], col="red") #upper CI bound
lines(c(3.15,3.15),c(imp3[64,4],imp3[64,5]),lwd=3, col="blue") #emphasize mu=3.15
lines(c(0,6),c(imp3[64,2],imp3[64,2])) #horizontal line

plot(imp3[,2],mu,pch=16,ylab=expression(mu),xlab="estimate",xlim=c(0.001335,0.001359)) #estimates
lines(imp3[,4],mu,col="red") #lower CI bound
lines(imp3[,5], mu,col="red") #upper CI bound
lines(c(imp3[64,4],imp3[64,5]),c(3.15,3.15),lwd=3, col="blue") #emphasize mu=3.15
lines(c(imp3[64,2],imp3[64,2]),c(0,6)) #horizontal line

###################  esetimate expectation of absolute value of X, where X follows t distribution with d.o.f 3
# plot of different distributions
x =seq(-4.5,4.5,by =0.01)
p1 = abs(x)*dt(x,3)
p2 = dt(x,3)
p3 = dt(x,1)
p4 = dnorm(x)
plot(x,p1,xlab="x",ylab="density",ylim=c(0,0.43),type="l",col=2)
lines(x,p2,col=3)
lines(x,p3,col=4)
lines(x,p4,col=5)
legend(1.3,0.4,c('Target','t3    ','t1    ','N(0,1)'),lty=c(1,1,1,1),col=2:5)

# estimates
N = 1000
q1 = vector("numeric",N)
q2 = vector("numeric",N)
q3 = vector("numeric",N)
# fit.set
se1 = vector("numeric",N)
se2 = vector("numeric",N)
se3 = vector("numeric",N)

for(i in 1:N)
{
# direct
 x = rt(i,3)
 q1[i] = mean(abs(x))
 se1[i] = sd(abs(x))/sqrt(i)


# proposed t1
 x = rt(i,1)
 q2[i] = mean(abs(x)*dt(x,3)/dt(x,1))
 se2[i] = sd(abs(x)*dt(x,3)/dt(x,1))/sqrt(i)

# proposed norm
 x = rnorm(i)
 q3[i] = mean(abs(x)*dt(x,3)/dnorm(x))
 se3[i] = sd(abs(x)*dt(x,3)/dnorm(x))/sqrt(i)
}

# direct estimate
par(mfrow=c(1,3))
plot(1:N,q1,xlab = "sample size",ylab="estimate",type="l",ylim=c(0,3.3))
lines(1:N, q1-1.96*se1 , col="red")
lines(1:N, q1+1.96*se1 , col="red")

plot(1:N,q2,xlab = "sample size",ylab="estimate",type="l",ylim=c(0,3.3))
lines(1:N, q2-1.96*se2 , col="red")
lines(1:N, q2+1.96*se2 , col="red")

plot(1:N,q3,xlab = "sample size",ylab="estimate",type="l",ylim=c(0,3.3))
lines(1:N, q3-1.96*se3 , col="red")
lines(1:N, q3+1.96*se3 , col="red")

### weight plot
n =50
w1 = vector("numeric",n)
w2 = vector("numeric",n)
w3 = vector("numeric",n)
# direct
# proposed t1
 x2 = rt(n,1)
 w2 = dt(x2,3)/dt(x2,1)
# proposed norm
 x3 = rnorm(n)
 w3 = dt(x3,3)/dnorm(x3)
par(mfrow=c(1,2))
plot(x2,w2,xlab ="x",ylab="weight in using t1",xlim=c(-3,3))
plot(x3,w3,xlab ="x",ylab="weight in using N(0,1)",xlim=c(-3,3))



 


### a simple random walk
p<-0.5
n<-1000
X<-rep(NA,n)
X[1]<--1
for (j in 2:n) {
   X[j]<-X[j-1]+2*(runif(1)<=p)-1
 }
t<-1:n
plot(t,X,type="l",xlab="time",ylab="X")

### random MH, generate standard normal from uniform 
norm = function (n, alpha) 
{
        vec = vector("numeric", n)
        vec[1] = 0
        for (i in 2:n) {
                y = runif(1, -alpha, alpha )
                y = y + vec[i-1]
                aprob = min(1, dnorm(y)/dnorm(vec[i-1]))
                u = runif(1)
                if (u < aprob) 
                    vec[i] = y
                else 
                    vec[i] = vec[i-1]
        }
        return(vec)
}
normvec<-norm(10000,1)
par(mfrow=c(2,1))
plot(ts(normvec))
hist(normvec,30)
 

### independence chain, generate gamma from normal with the same mean and variance 
gamm = function (n, alpha, beta) 
{
   mu = alpha/beta
   sigma = sqrt(alpha/(beta^2))
   vec = vector("numeric", n)
   vec[1] = alpha/beta
   for (i in 2:n) {
      y <- rnorm(1, mu, sigma)
      aprob <- min(1, (dgamma(y, alpha, beta)/dgamma(vec[i-1], 
        alpha, beta))/(dnorm(y, mu, sigma)/dnorm(vec[i-1], 
          mu, sigma)))
      u <- runif(1)
      if (u < aprob) 
          vec[i] = y
      else 
          vec[i] = vec[i-1]
    }
    return(vec)
}
vec<-gamm(10000,2,4)
par(mfrow=c(2,1))
plot(ts(vec))
hist(vec,20)

#########################################
#### Gibss samplers for bivariate normal
rho = 0.2
x0 = rnorm(1)
N= 10000
X = x0
Y = NULL
## for t=1
Y = rnorm(1,rho*x0,sqrt(1-rho^2))
X = rnorm(1,rho*Y[t],sqrt(1-rho^2))
## the main loop of generating samples using gibbs sampler
for(t in 2:N)
{
    Y = c(Y,rnorm(1,rho*X[t-1],sqrt(1-rho^2)))
    X = c(X,rnorm(1,rho*Y[t],sqrt(1-rho^2)))
 }

## compare with the true standard normal 
par(mfrow=c(2,2))
X0 = seq(-100,100,by = 0.1)
hist(X[1000:N],breaks = 100, freq = FALSE,xlab="X")
dX0 = dnorm(X0)
lines(X0,dX0)
hist(Y[1000:N],breaks = 100, freq = FALSE,xlab="Y")
lines(X0,dX0)

## correlation 
cor(cbind(X,Y))

## compare with the samples generated using mvrnorm
library(MASS)
mu = c(0,0)
Sigma = rbind(c(1,rho),c(rho,1))
U = mvrnorm(N,mu,Sigma)
hist(U[,1],breaks = 100, freq = FALSE,xlab="X")
dX0 = dnorm(X0)
lines(X0,dX0)
hist(U[,2],breaks = 100, freq = FALSE,xlab="Y")
lines(X0,dX0)
## correlation 
cor(U)


#### hierarchical model, binomial-beta
install.packages("VGAM")
library(VGAM)
N=10000  
n= 30
a=3
b=7
X= vector("numeric",N)  
theta = vector("numeric",N)
## for t=1
theta[1]=rbeta(1,a,b)  
X[1]=rbinom(1,n,theta[1])
for (i in 2:N)
{ 
   X[i]=rbinom(1,n,theta[i-1])
   theta[i]=rbeta(1,a+X[i],n-X[i]+b)
 }
## compare with the true marginal distribution 
par(mfrow=c(1,2))
X0 = seq(0,30,by = 1)
hist(X[1000:N], freq = FALSE,xlab="X")
lines(X0,dbetabinom.ab(X0,n,a,b))
theta0 = seq(0,1,by=0.01)
hist(theta[1000:N],breaks = 100, freq = FALSE,xlab=expression(theta))
lines(theta0,dbeta(theta0,a,b))


#######An example of Gibbs sampling########################
y = c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
t = c(94, 16, 63, 126, 5, 31, 1, 1, 2, 10)


#### function to draw lambda
lambda.draw = function(alpha, beta, y, t) 
 {
  rgamma(length(y), y + alpha, t + beta)
 }

#### function to draw beta
beta.draw = function(alpha, gamma, delta, lambda, y) 
{
  rgamma(1, length(y) * alpha + gamma, delta + sum(lambda))
 }

##### Gibbs sampling 
gibbs = function(n.sims, beta.start, alpha, gamma, delta, y, t, burnin = 0, thin = 1) 
{
  beta.draws = c()

  lambda.draws = matrix(NA, nrow = n.sims, ncol = length(y))

  beta.cur = beta.start

  for (i in 1:n.sims) {
  # draw lambda
  lambda.cur = lambda.draw(alpha = alpha, beta = beta.cur,y = y, t = t)

  # draw beta
  beta.cur = beta.draw(alpha = alpha, gamma = gamma, delta = delta, lambda = lambda.cur, y = y)

  # burn-in and thinning
  if (i > burnin & (i - burnin)%%thin == 0) {
     lambda.draws[(i - burnin)/thin, ] = lambda.cur
     beta.draws[(i - burnin)/thin] = beta.cur
   }
  }

  return(list(lambda.draws = lambda.draws, beta.draws = beta.draws))
}


#### Apply to the failure data set
posterior = gibbs(n.sims = 10000, beta.start = 1, alpha = 1.5,
                     gamma = 0.01, delta = 1, y = y, t = t)
round(colMeans(posterior$lambda.draws),3)
mean(posterior$beta.draws)
round(apply(posterior$lambda.draws, 2, sd),3)
round(sd(posterior$beta.draws),3)

