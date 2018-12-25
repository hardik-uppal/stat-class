# Newton method

# define a function to get the negative log likelihood of logistic distribution
nloglogis<-function(theta, x)
{ n<-length(x) # sample size
 loglik<-n*theta-n*mean(x)-2*sum(log(1+exp(-(x-theta))))
 return(-loglik)
}

# mle by Newton method
n<-20
set.seed(101)
x<-rlogis(n)  # the true theta is 0
theta.start<-median(x) 
ml<- nlm(nloglogis, theta.start, x = x)
ml$estimate    #mle

n<-50
set.seed(101)
x<-rlogis(n,4)  # the true theta is 4
theta.start<-median(x)
ml<- nlm(nloglogis, theta.start, x = x)
ml$estimate    #mle

