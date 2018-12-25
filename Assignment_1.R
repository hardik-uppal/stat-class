#1. Use data(state) to load the data state in R. Consider one of its dataset, state.x77, and
#complete the following parts.

data(state)

df1 = data.frame(state.x77)
df1

#(a) Look up the help for state.x77.

?state.x77
##(b) Compute the dimension size of state.x77 and display its dimension names.

dim(df1)

atb = dimnames(df1)

heading=colnames(df1) 

##(c) Write an expression that returns the summary statistics, as given by summary(),
##for each of the columns in this matrix.
colsumry = lapply(df1,summary)

for(i in 1:length(heading)){
  print(colsumry[i])
}
colsumry[1]

##(d) Create a vector named popdense that contains the population density in persons
##per square mile. Note that the population in state.x77 is given in thousands.

popdense = c((df1$Population*1000)/df1$Area)

##(e) Apply the function pairs() to the matrix to create scatter plots of all columns
##against one another.

pairs(df1)

##(g) Select the states that have < 1% illiteracy, but a murder rate of > 10 per 100,000.
gdata = subset(df1,df1$Illiteracy<1&df1$Murder>10)
gdata

##(h) Select the states that have greater than average high school graduate rates, but
##less than average annual income.
avgHS = mean(df1$HS.Grad)
avgincm = mean(df1$Income)
hdata = subset(df1,df1$HS.Grad>avgHS&df1$Income<avgincm)
hdata
##(i) Create a vector named Murder that contains the murder rates.

murder=df1$Murder
##(j) Create a vector named Illiteracy that contains the illiteracy rates.

Illiteracy = df1$Illiteracy

##(k) Create a design matrix, X, that contains all 1's in the first column and the
##illiteracy rates in the second column. This will serve as our design matrix in part
##(m).
ones=c(rep(1,50))
X = cbind(ones,df1$Illiteracy,deparse.level = 2)

##(l) Assume that there is an approximate linear relationship between Illiteracy and
##Murder rates. Given the design matrix defined above, we can define a simple
##linear model as y = X + ?? where y(Murder) is the dependant variable, X is the
##design matrix of independent variables,  is the vector of parameters and ?? is the
##error term. The least squares estimate of  is: ^  = (XTX)????1XT y, where XT is
##the transpose of X. Write a function which has inputs X and y, and returns ^  in
##R.
state_murder_rate = df1$Murder
beta = function(design_matrix,murder_rate){
  b = (solve(t(design_matrix)%*%design_matrix))%*%t(design_matrix)%*%murder_rate
  print(b)
  
}
beta(X,state_murder_rate)

##(n) In a single plot, draw the following subplots (1) a scatter plot of Murder (y)
##versus Illiteracy (x); (2) a histogram of Murder; (3) a Quantile-Quantiale plot of
##Murder;(4) a boxplot of Murder and Illiteracy.

par(mfrow=c(2,2))
plot(df1$Murder,df1$Illiteracy, main = 'Murder vs Illiteracy', xlab = 'Murder', ylab = 'Illiteracy')
hist(df1$Murder, main = 'Histogram of Murder',xlab='murder rate')
qqnorm(df1$Murder, main = 'Q-Q plot of Murder', xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(df1$Murder)
boxplot(df1$Murder,df1$Illiteracy,main = 'Boxplot of Murder and Illiteracy',names = c('Murder','Illiteracy'))


##2. (Graduate students only) Tabulating quantiles of the t-distribution.
##(a) Create a vector called percentile that contains the values 0.75, 0.9, 0.95, 0.975,
##and 0.99 and 0.999.

percentile= c(0.75,0.9,0.95,0.975,0.99,0.999)
##(b) Create a vector named df that contains the integers 1 to 30 followed by 60 and
##Infinity. Do this without typing in the integers 1 through 30.

df = c(1:30,60,Inf)

##(c) Create a matrix named tTable that returns the percentile from a t-distribution
##where the rows represent the degrees of freedom as specified by the vector df and
##the columns are the quantiles as specified by percentile. That is, the cells are t
##such that P(Tdf <= t) = percentile where Tdf is a t-distribution with df degrees
##of freedom. Hint: you may need to perform a transpose to get the rows and
##columns as specified.

tValue = function(n){
  tTable = qt(n,df)
}

tTable = sapply(percentile,tValue)

tTable = round(tTable,4)
colnames(tTable) = percentile
rownames(tTable) = df
print(tTable)

##3. (Multiple linear regression) Install and load the R package ISLR and consider the
##dataset Auto in the package. Treat mpg as the dependent variable and all the other
##variables except name as the independent variables (predictors). Note that the 1,2,3
##of the variable origin correspond to American;European; Japanese.
install.packages("ISLR")
library(ISLR)

autodf = data.frame(Auto)
dep_autodf=autodf[,1:8]
##(a) Create a pairwise scatter plot for dependent and independent variables. Show the
##plot and make comment on the plot.
pairs(dep_autodf)


##(b) Computer the correlation matrix between the variables using cor() function.

cor(dep_autodf)

##(c) Fit the multiple linear regression model. Show the table of the fitted model:
##coefficients estimation, their standard deviation, t-statistic, and p-values. Show
##R2 and the estimation ^2.

fit = lm(data=dep_autodf,formula =  mpg ~ .)
sumfit = summary(fit)
# fit2 =  lm(data=dep_autodf,formula =  mpg ~ weight+year+origin)
# summary(fit2)
# fit3 =  lm(data=dep_autodf,formula =  mpg ~ horsepower*cylinders+year+origin)
# summary(fit3)
# anova(fit,fit3)
sumfit$coefficients
sumfit$r.squared
sumfit$sigma**2

##(d) Obtain the prediction of mean response, its associated prediction error and 100(1????
##)% confidence interval based on the fitted model for the new input cylinders = 8; displacement = 300; horsepower = 150;weight = 3600; acceleration = 13; year =
##   77; origin = 3.

new_data = data.frame(cylinders = 8, displacement = 300, horsepower = 150,weight = 3600,acceleration = 13,year = 77, origin = 3)
pred_mean_resp = predict.lm(fit,data = new_data , se.fit=TRUE, interval =  "confidence")

##(e) Is there relationship between the independent variables and the response variable?
sumfit$
sumfit$fstatistic[1]
1- pf(sumfit$fstatistic[1],sumfit$fstatistic[2],sumfit$fstatistic[3])

##(f) Which predictors appear to have a statistically significant relationship to the
##response?
coef(sumfit)
confint(fit)

##(g) Produce the residuals plots.
plot(fit)