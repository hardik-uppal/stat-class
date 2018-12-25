###################################### 
# Section 1.1   
###################################### 
setwd("C:/stat462/")
install.packages("alr3")
library(alr3)
help(mean)
?mean
example(mean)
help('if')     
?'if'
help(package=stats)

###################################### 
# Section 1.2  Create objects
###################################### 
n = 4
x = vector("numeric",n)
x = c(2,1,5,4)
length(x)     
x[1] #the first entry of x 
y =  matrix(1:4,2,2)
y[1,2] # the first row and 2nd column of y 
y[1,]  # the first row of y
y[,2]  # the second column of y 
y[1:2,] # the first two rows of y 
y[,1:2] # the first two columns of y
z=  array(1:22, c(2,3,2))
list1<-list(name="Max", lucknumber=c(1,7),
            grades=matrix(1:4,2,2), male=T, age=37)
list1$name
list1[[1]]
x2 = c("red", "white", "red", NA)
x3 = c(TRUE,TRUE,TRUE,FALSE)
x = data.frame(x,x2,x3)
names(x)<-c("ID","Color","Passed")
x
x$ID
x[,1]
length(x)
class(x)
str(x)
names(x)


###################################### 
# Section 1.3 Data Import and Export 
###################################### 
x<-read.table("friend_or_foe.DAT",header=TRUE)
class(x)
x<-read.table("classlist.csv", header=T, sep=",")
x<-matrix(scan("d4scan2.txt"),ncol=4,byrow=T)

cat("2 3 5 7", "11 13 17 19", file="ex.dat", sep="\n")
cat("3 1 4 1 5 9 2 6", file="ex.dat",append=T)
#To write a CSV file for input to Excel one might use
x <- data.frame(gender = factor(c(1,1,0), levels = c(0, 1),
                                labels = c("F", "M")), height = c(172, 186.5, 162),
                weight = c(71, 89, 50),stringsAsFactors=F)
write.table(x, file = "ex4.csv", sep = ",",
            row.names=F,qmethod = "double")
# write to a plain text file
write.table(x,file="ex5.txt",col.names=T, row.names=F)
write.table(x,file="ex5.txt",quote=F, col.names=F,
            row.names=F,append=T)



###################################### 
# Section 1.4 Basic operation and probability distributions
###################################### 
a = c(1,3,5,7)
a = seq(1,7, by =2)
a
a+2
a-2
a*2
a/2


b = a -2
sqrt(a)
a^2
log(a)
exp(a)
d = (a^3 + sqrt(a))/(exp(6)+1)

b + d
b - d
b/d
b*d


summary(a)
mean(a)
max(a)
min(a)
var(a)
sd(a)
quantile(a)

sort(a,decreasing = FALSE )
rank(a)
sum(a)

## get help on distributions
help(Distributions)
help(Normal)

## normal distributions
# density
dnorm(0)
dnorm(0)*sqrt(2*pi)
dnorm(0,mean=3)
dnorm(0,mean=3,sd=10)
a =  c(0,1,2)
dnorm(a)
x =  seq(-20,20,by=.1)
y =  dnorm(x)
plot(x,y)
y = dnorm(x,mean=2.5,sd=0.1)
plot(x,y)

#  cumulative density function
pnorm(0)
pnorm(1)
pnorm(0,mean=3)
pnorm(0,mean=2,sd=5)
a =  c(0,1,2)
pnorm(a)
x = seq(-20,20,by=.1)
y = pnorm(x)
plot(x,y)
y <- pnorm(x,mean=3,sd=4)
plot(x,y)

# quantile
qnorm(0.5)
qnorm(0.5,mean=1)
qnorm(0.5,mean=1,sd=3)
qnorm(0.5,mean=2,sd=3)
qnorm(0.5,mean=2,sd=4)
qnorm(0.25,mean=2,sd=2)
qnorm(0.333)
qnorm(0.333,sd=2)
qnorm(0.75,mean=3,sd=2)
a = c(0.1,0.3,0.75)
qnorm(a)
x = seq(0,1,by=.05)
y = qnorm(x)
plot(x,y)
y = qnorm(x,mean=3,sd=2)
plot(x,y)
y = qnorm(x,mean=3,sd=0.1)
plot(x,y)

# random number
rnorm(4)
1.2387271 -0.2323259 -1.2003081 -1.6718483
rnorm(4,mean=2)
rnorm(4,mean=2,sd=3)
y =  rnorm(200)
hist(y)
y = rnorm(200,mean=-2)
hist(y)
y = rnorm(200,mean=-2,sd=4)
hist(y)
qqnorm(y)
qqline(y)






###################################### 
# Section 1.5 Graphics
###################################### 
pdf(file="mygraph.pdf",width=6,height=6)
plot(x=rnorm(20),y=rnorm(20),xlab="X", ylab="y",
     main="example of creating a graph")
dev.off()

fuel<-read.table("fuel.txt",header=TRUE)
plot(fuel$Percent_driver,fuel$Fuel,type="p",xlab="Percent of Driver",
     ylab="Fuel",col="red",pch="* ")
title("Plot of Fuel versus Percent of Driver")
abline(-227.309,14.098)
text(mean(fuel$Percent_driver), mean(fuel$Fuel),"Average")
lines(lowess(fuel$Percent_driver,fuel$Fuel), col = 3)
legend(64,450, c("Fuel","SLR Fiited Fuel","Smoothed Fuel"), pch="*  ",
       lty=c(0,1,1),col=c("red","black","green"))


## Graphics parameter setting
par(mfrow=c(2,3))
x<-seq(0,2,by=0.1)
plot(x,x,xlab="x value",ylab="y value",main="x versus x",
     col=4,type="p",sub="subtitle", cex=2)
plot(x,x^2,xlab="x value",ylab="y value",main=
       expression(paste(x^2, " versus x")),col="red",type="l",
     lty="dashed")
plot(x,x^2+x,xlab="x value",ylab="y value",main=
       expression(paste(x^2+x, " versus x")),col="green",type="b",
     lty=3,lwd=4)
plot(x,x^3+x,xlab="x value",ylab="y value",main=expression
     (paste(x^3+x, " versus x")),col="green",type="p",pch=6,font=3)
par(mfg=c(2, 3, 2, 3))
plot(x,x^3+x^2,xlab="x value",ylab="y value",main=expression(
  paste(x^3+x^2, " versus x")),col="purple",type="p",pch=22,tck=0.03)
par(mfg=c(2, 2, 2, 3) )
plot(x,x^3+x^2+x,xlab="x value",ylab="y value",main=expression(
  paste(x^3+x^2+x, " versus x")),col="black",type="p",pch=23,tck=1,
  font.lab=4,font.main=4)


###################################### 
# Section 1.6 Function
###################################### 
x = 2
if ( x < 0.5)
{     x <- x + 1
cat("increase number by 1!\n")
} else
{   x <- x - 1
cat("decrease number by 1.\n");
}

heart = function(name){
  t = seq(-1,1,len=5000)
  x = sin(t) * cos(t) * log(abs(t))
  y = sqrt(abs(t)) * cos(t)
  plot(x,y,type='l',axes=FALSE,xlab='',ylab='')
  text(0,.38,"Happy Valentine's Day",col='red',cex=2.5)
  text(0,.32,name,col='red',cex=2.5)
}

heart1 = function(name){
  t = seq(0,60,len=100)
  plot(c(-8,8),c(0,20),type='n',axes=FALSE,xlab='',ylab='')
  x = -.01*(-t^2+40*t+1200)*sin(pi*t/180)
  y = .01*(-t^2+40*t+1200)*cos(pi*t/180)
  lines(x,y)
  lines(-x,y)
  text(0,7,"Happy Valentine's Day",col='red',cex=2.5)
  text(0,5.5,name,col='red',cex=2.5)
}


cat("Type (1 or 2)? ")
type = readLines(n=1)
cat("Name? ")
name = readLines(n=1)

if(type == 1)heart(name) else heart1(name)


