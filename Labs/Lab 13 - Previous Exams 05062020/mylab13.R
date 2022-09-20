setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# clean workspace:
graphics.off()
rm(list=ls())

# load the function for m shapiro test:
load("C:/Users/Marco Paggiaro/OneDrive/Desktop/mieidati/Applied Statistics/mcshapiro.test.RData")


# Import all the libraries, just because
library(class)
library(MASS)
library(car)
library(rgl)

#_______________________________________________________________________________
###### Exam of 5/09/2008
######-----------------------------
#_______________________________________________________________________________
### Problem 1
# Famous Dantists believe that the number of characters (NC) and the 
# number of words (NP) contained in a generic sonnet of Dante follow
# approximately a jointly normal distribution with mean mu=c(400,90) 
# and covariance matrix Sigma=cbind(c(100,20), c(20,10)). Recently, 
# two new sonnets attributed to Dante have been discovered, and have 
# been identified for the moment with the codes 2008A and 2008B. 
# Assuming the number of characters and of words of the sonnet 2008A
# independent of the number of characters and words of the sonnet 2008B:
# a) identify in the plane NC x NP an ellipsoidal region in which the
#    sonnet 2008A is contained with probability 0.9.
# b) How likely only one of the two sonnets falls in the region 
#    identified at point (a)?

# a)
mu = c(400,90)
S = cbind(c(100,20), c(20,10))

# a)
r <- qchisq(0.9,2)

plot(mu[1],mu[2])
ellipse(mu, S, r)

# b) Just one of the two inside it:
0.9*0.1 + 0.1*0.9

# directions of the axes:
eigen(S)$vectors
# length of the axes:
r*sqrt(eigen(S)$values)

#_______________________________________________________________________________
### Problem 2
# The dataset eighteen.txt contains, for 100 Italian municipalities, 
# the percentages of underage boys and underage girls with respect to
# the entire population resident in the municipality. Assuming that 
# this is a sample from a bivariate normal distribution:
# a) perform a test to verify that in Italy the number of underage 
#    resident boys is the same as the number of underage resident girls;
# b) knowing that 60 million people reside in Italy, provide three 
#    T2-simultaneous intervals (global confidence 90%) for:
#    - The absolute number of underage boys who reside in Italy,
#    - The absolute number of underage girls who reside in Italy,
#    - The absolute number of minors who reside in Italy.

eighteen <- read.table('eighteen.txt', header=T)
head(eighteen)
dim(eighteen)
plot(eighteen)

# a)
diff <- eighteen$M - eighteen$F
plot(diff)
shapiro.test(diff)
?t.test
t.test(diff, mu=0)
# mean is not equal to zero!

# b) 
# 100 people:
n <- 100
p <- 2
alpha <- 0.1
# quantile:
cfr.fisher <- qf(1-alpha, p,n-p)
cfr.fisher

# three intervals:
# males:
a1 <- c(1,0)
# females: 
a2 <- c(0,1)

# mean
a3 <- c(0.5,0.5)

mean.X <- colMeans(eighteen)
mean.X
S <- cov(eighteen)
S
CI.males <- cbind(
  inf = a1%*%mean.X - sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a1)%*%S%*%a1/n),
  mean = a1%*%mean.X,
  sup = a1%*%mean.X + sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a1)%*%S%*%a1/n)
  )
CI.males
CI.females <- c(
  inf = a2%*%mean.X - sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a2)%*%S%*%a2/n),
  mean = a2%*%mean.X,
  sup = a2%*%mean.X + sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a2)%*%S%*%a2/n)
)
CI.females
CI.mean <-c(
  inf = a3%*%mean.X - sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a3)%*%S%*%a3/n),
  mean = a3%*%mean.X,
  sup = a3%*%mean.X + sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a3)%*%S%*%a3/n)
)
CI.mean

# what about 60 millions???
# You have to change a!!!!
a1 <- 60*10^6/100*a1
a1
a2 <- 60*10^6/100*c(0,1)
a3 <-60*10^6/100*a3

CI.males <- cbind(
  inf = a1%*%mean.X - sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a1)%*%S%*%a1/n),
  mean = a1%*%mean.X,
  sup = a1%*%mean.X + sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a1)%*%S%*%a1/n)
)
CI.males
CI.females <- c(
  inf = a2%*%mean.X - sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a2)%*%S%*%a2/n),
  mean = a2%*%mean.X,
  sup = a2%*%mean.X + sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a2)%*%S%*%a2/n)
)
CI.females
CI.mean <-c(
  inf = a3%*%mean.X - sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a3)%*%S%*%a3/n),
  mean = a3%*%mean.X,
  sup = a3%*%mean.X + sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(t(a3)%*%S%*%a3/n)
)
CI.mean

#_______________________________________________________________________________
### Problem 3
# The West Sussex Bread Association has randomly selected 60 business
# trades in which doughnuts are commonly sold. 30 activities are based
# in the city of Brighton and 30 in the town of Worthing. For each of 
# the two cities, the price of a plain doughnut was recorded in 10 
# activities, the price of a doughnut filled with cream in other 10 
# activities and the price of a doughnut filled with jam in the 
# remaining 10 activieties.
# The data are reported in doughnut.txt dataset.
# a) Describe the ANOVA model you deem appropriate for the analysis of 
#    these data.
# b) Identifying factors that significantly influence the distribution
#    of the price of doughnuts, propose a possible reduced model.
# c) Using the Bonferroni's inequality estimate through bilateral 
#    confidence intervals (with global confidence 95%) the means and the
#    variances of the subpopulations associated with the reduced model
#    identified at step (b).

doughnuts <- read.table('doughnut.txt', header=TRUE)
head(doughnuts)
dim(doughnuts)

attach(doughnuts)

aov.dough <- aov(prezzo ~ citta + tipo + citta:tipo)
summary(aov.dough)
# assumptions : gaussianity of each group + equal variances!

# b) Reduce the model:
aov.add <- aov(prezzo ~ citta + tipo)
summary(aov.add)

aov.final <- aov(prezzo ~ tipo)
summary(aov.final)

# c)
# assumption: gaussianity:
levels(tipo)
shapiro.test(prezzo[which(tipo == "crema")])
shapiro.test(prezzo[which(tipo == "liscia")])
shapiro.test(prezzo[which(tipo == "marmellata")])

#OK!
k <- 6
alpha <- 0.05
cfr.t <- qt(1-alpha/(2*k),n-1)
# first one:
CI.crema <- c(
  inf = mean(prezzo[which(tipo == "crema")]) - 
)