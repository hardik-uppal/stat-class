x = c(1,4,7,10)
y = c (2,5,6,8)

D1= matrix(c(x,y),nrow=4,ncol = 2)
D2= matrix(c(x,y),byrow = TRUE,ncol = 4, nrow = 2)


Length(x)

dim(D1)
D2[2,3]
D2[2,]
D2[,3]

x+y
sum(x+y)
x*y
dot(x)
D3=matrix(c(1,7,2,8,3,1,5,9), nrow = 4,ncol = 2)

t(D1)
D2%*%D3

========================
  
write.table(D3,file = "D3.txt",col.names = T,row.names = T)

write.csv(D3,file = "D3.csv",row.names = F)



read1 = read.csv("D3.csv",header = T)

read2 = read.table("D3.txt", header = T)

read3 = fread("//mast.queensu.ca/ ∼ cdlin/doc/stat462/D3.csv")
================================
  
par(mfrow=c(2,3))
plot(x,y,xlab = "x",ylab = "y",main = "x vs y")
lines(D3,xlab="x",ylab = "y")

boxplot(x,xlab="x", main="boxplot of x")
hist(x, main="histogram of x")

