setwd("~/Projects/STAT862")
filmData =  read.csv("filmData.txt",header = TRUE, sep="")

attach(filmData)
summary(filmData)

lapply(filmData,summary)

table(Oscar,Country)

boxplot(BoxOffice,Country)
cor(filmData)

logit_fit = glm(data = filmData, formula = Oscar~BoxOffice,family = binomial(link='logit'))
summary(logit_fit)
logit_fit$coefficients
exp(logit_fit$coefficients)
logit_fit$fit

fitted_resp1 = as.numeric(logit_fit$fit>0.5)

logit_fit2 = glm(data = filmData, formula = Oscar~.,family = binomial(link='logit'))
summary(logit_fit2)
logit_fit2$fitted.values
fitted_resp2 = as.numeric(logit_fit$fitted.values>0.5)
exp(logit_fit2$coefficients)


sum(fitted_resp2)
sum(filmData$Oscar)

##test error of these models????
library(MASS)

model_lda = lda(Oscar~BoxOffice,data = filmData)
model_lda$fitted.values
