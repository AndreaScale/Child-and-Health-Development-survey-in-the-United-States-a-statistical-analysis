###### SECOND ASSIGNMENTS

## Andrea Scalenghe - 913359
## Matteo Morabito - 918551

#First of all we extract the dataset from the UsingR libray

library(tidyverse)
library(UsingR)
data(babies)
attach(babies)
str(babies)

dim(babies)

#Exercise 1

#At first we plot some useful graphs for a complete descriptive analysis of wt

hist(wt, breaks = 20, col="blue", main="Babies birth weight", xlab = "Babies birth weight (ounces)")

#We can immediately see that the data distributes similarly to a normal whit mean 120

boxplot(wt, col="blue", main="Babies birth weight", xlab = "Babies birth weight (ounces)")

#Also the boxplot graph confirm what we saw on the histogram. In the end by 
#plotting the data in the Q-Q plot we can see that:

qqnorm(wt, main="Babies birth weight", xlab = "Babies birth weight (ounces)")
qqline(wt, col="red")

#This graphs states that, a part from the very end of the tails, data are
#distribute like a normal distribution

summary(wt)

#The mean, as predicted, is around 120 ounces (as matter of facts 119.6 ounces).
#The distribution of quantiles tells us that the first and third are equally 
#far from the mean. Considering the end of the tails the distribution has a slightly
#bigger left tail. 
#We now test the normality

shapiro.test(wt)

#The p-value is <0.05, which means that we refuse the normality. Although the shapiro
#test gives more likely false results for large size samples, so we must take  
#in consideration the fact that the test is biased
length(wt)

#For what concerns marital

hist(marital, col="blue", main="Marital status", xlab = "marital status")

#It's clear from the histogram the almost the totality of marital status are 1:
#most mother with babies are married

summary(marital)

#In fact the summary tells us that the median, the mean and the first and third quantile 
#are all 1. It means that, as said, almost all the values are 1.


#Exercise 2

#We aim to find the best estimator for the babies weight. In order to do so we
#compute a correlation test and look at the p-value. The two hypotheses are:
#H0 : The factors are uncorrelated
#H1 : The factors are correlated

dwt[dwt==999]<-NA
wt1[wt1==999]<-NA
gestation[gestation==999]<-NA

plot(dwt, wt)
plot(wt1, wt)
plot(gestation, wt)

#Linear correlation seems light in the first 2 plots, the stronger 
#one seems to be in the third

cor.test(dwt,wt)
cor.test(wt1,wt)  
cor.test(gestation,wt)

#The results obtained are:
#p-value for dwt-wt correlation: 8.999e-05
#p-value for wt1-wt correlation: 8.207e-08
#p-value for gestation-wt correlation: 2.2e-16
#Furthermore the higher cor values is the last one, let's check the linear
#regression for wt and gestation


model <- lm(wt~gestation)
summary(model)
residuals <- residuals(model)
qqnorm(residuals)
qqline(residuals)

#The model is good and residuals distributes almost perfectly as a normal

plot(gestation, wt)
abline(model, col = "red")

#We can state that the length of the gestation is the best estimator for babies 
#weight.

#In order to perform a multivariate analysis, as taught in the article linked,
#we use the library "Tidyverse" and the function "lm".

model <- lm(wt~dwt+wt1+gestation)
summary(model)

#The result is clear: p-value and F-statistic are not at all significant. Furthermore 
#the R-squared is far from 1, which means that the portion of variance explained
#in the outcome is little.
#We see also that the best predictor is confirmed to be the gestation. 


#Exercise 3

parity1<-parity
table(parity1)
parity1[parity1>2]<-3
as.factor(parity1)
table(parity1)

wta <- wt[parity==0]
wtb <- wt[parity==1]
wtc <- wt[parity==2]
wtd <- wt[parity>2]
summary(wta)
summary(wtb)
summary(wtc)
summary(wtd)

#From the summaries it is clear that the mean birth weight grows as parity grows
#Although we can also see that the minumum decreases as the parity grows. This
#means that the birth weight spreads as the parity grows.

#Let's compare graphically 
par(mfrow=c(1,4))
hist(wta, breaks=10, freq=F, col="red", main="parity=0", xlab = "birth weight" )
hist(wtb, breaks=10, freq=F, col="red", main="parity=1", xlab = "birth weight" )
hist(wtc, breaks=10, freq=F, col="red", main="parity=2", xlab = "birth weight" )
hist(wtd, breaks=10, freq=F, col="red", main="parity>2", xlab = "birth weight" )
par(mfrow=c(1,1))
hist(wta, freq=F, col="blue")
hist(wtb, freq=F, col="yellow", add=T)
hist(wtc, freq=F, col="orange", add=T)
hist(wtd, freq=F, col="purple", add=T)
legend(x="topright", legend = c(0,1,2,3), title = "Parity", pch=20, 
       col = c("blue","yellow","orange","purple"))

#We can see that all of the distribution are similar, although as the parity 
#grows the distribution spreads

par(mfrow=c(1,4))
qqnorm(wta, col="orange", main = "parity=0")
qqline(wta, col="red")
qqnorm(wtb, col="blue", main = "parity=1")
qqline(wtb, col="red")
qqnorm(wtc, col="yellow", main = "parity=2")
qqline(wtc, col="red")
qqnorm(wtd, col="green", main = "parity>2")
qqline(wtd, col="red")

#From this graphs the samples seem to distribute as a normal. 
#We test the normality by the shapiro test 

shapiro.test(wta)
shapiro.test(wtb)
shapiro.test(wtc)
shapiro.test(wtd)

#p-values are not homogeneous and wt2 and wt3 result not normally distributed. We must keep in mind the the test is biased by the large size of the sample
#Let's check the variances

l <- list(wta,wtb,wtc,wtd)
bartlett.test(l)

#Variances aren't equal, therefore we can't perform the ANOVA test. Let's perform the Kruskal-Wallis test insted

kruskal.test(wt~parity1)

#The null hypotheses cannot not be refused: the medians of the three groups cannot be considered different at a 95% significance level

par(mfrow=c(1,1))
boxplot(wt~parity1, col = "darkorange")

#Exercise 4

#Let's calculate the probability of each marital status:
P0 <- sum(marital==0)/length(marital)
P1 <- sum(marital==1)/length(marital)
P2 <- sum(marital==2)/length(marital)
P3 <- sum(marital==3)/length(marital)
P5 <- sum(marital==5)/length(marital)

#Then we simulate the samples 

n1 <- 1000
n2 <- 10000
n3 <- 100000
n4 <- 1000000

S1 <- sample(c(0,1,2,3,5), n1, replace = T, prob = c(P0, P1, P2, P3, P5))
S2 <- sample(c(0,1,2,3,5), n2, replace = T, prob = c(P0, P1, P2, P3, P5))
S3 <- sample(c(0,1,2,3,5), n3, replace = T, prob = c(P0, P1, P2, P3, P5))
S4 <- sample(c(0,1,2,3,5), n4, replace = T, prob = c(P0, P1, P2, P3, P5))

#We calculate now the relative frequency 

ftable1 <- table(S1)/n1
ftable2 <- table(S2)/n2
ftable3 <- table(S3)/n3
ftable4 <- table(S4)/n4
ftable <- table(marital)/length(marital)
cbind(ftable1, ftable2, ftable3, ftable4, ftable)

#It is clear from the table of contingency that the higher the number of sample 
#the better the estimation
#Let's calculate the errors

err1 <- ftable1 - ftable
err2 <- ftable2 - ftable
err3 <- ftable3 - ftable
err4 <- ftable4 - ftable
cbind(ftable, err1, err2, err3, err4)

#From the table it is clear that from n=100000 the error is extremely low, 
#magnitude e-04  

#Let's consider the squared errors

sum(err1*err1)
sum(err2*err2)
sum(err3*err3)
sum(err4*err4)

#It is clear the improvement, although there is less discrepance in the analysis
#of SSE
#We can also compare graphically the samples

par(mfrow=c(1,1))
barplot(cbind(ftable1, ftable2, ftable3, ftable4, ftable), beside = T, 
        names.arg = c("S1", "S2", "S3", "S4", "Type"), main = "Barplot of samples by type", 
        col=c("blue", "red", "green", "black", "purple"), ylim=c(0,1))
grid()
legend(x="topright", legend = c(0,1,2,3,5), pch=20, col=c("blue", "red", "green", "black", "purple"))

#We can see that, although error of low magnitude, the samples well approximate 








