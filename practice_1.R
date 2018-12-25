pdf(file="mygraph.pdf",width=6,height=6)
plot(x=rnorm(20),y=rnorm(20),xlab="X", ylab="y",
     main="example of creating a graph")
dev.off()
cat("taype 1 or 2")
read.lines




a=5
b=4
f = function(a,b){
  print(a)
  print(b)
}


f(4,5)


x <- data.frame(gender = factor(c(1,1,0), levels = c(0, 1),
                                labels = c("F", "M")), height = c(172, 186.5, 162),
                weight = c(71, 89, 50),stringsAsFactors=F)
write.table(x, file = "ex4.csv", sep = ",",
            row.names=F,qmethod = "double")


x=1
if(x>0.5) {
  x<-x+1
  cat("x increase by 1\n")
  print(x)
  } else {
  x<-x-1
  cat("x decrease by 1\n")
  print(x)
}


x = 2
if ( x < 0.5){     
  x <- x + 1
  cat("increase number by 1!\n")} 
else{   x <- x - 1
cat("decrease number by 1.\n");}
  

for(i in 1:10)
{print(i)}