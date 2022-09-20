#### EXAM: 28 Giugno 2019
#### PROBLEM 1
####

# set wd
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

# Data exploration
terrassa <- read.table("terrassa.txt", header=T)
head(terrassa)

girona <- read.table("girona.txt", header=T)
head(girona)

mean.g <- sapply(girona, mean)
mean.t <- sapply(terrassa, mean)
mean.g
mean.t

boxplot(girona)       #Qui differenza tra T1 e T2
boxplot(terrassa)     #Qui non sembra esserci una differenza

plot(girona)
plot(terrassa)

# a) paired data: let's perform a test on the difference.
delta1 <- girona[,1]-terrassa[,1]
delta2 <- girona[,2]-terrassa[,2]

delta = cbind(delta1,delta2)
plot(delta)

# assumption: Gaussianity.
mcshapiro.test(delta)
# high p-value: verified!

# multivariate test:
n <- dim(delta)[1]
p <- dim(delta)[2]
alpha <- 0.05       # Set the value of the analysis
mu0 <- c(0,0)   # If there's no difference between the two the mean of delta should be zero


x.mean = c(mean(delta[,1]), mean(delta[,2]))
x.mean
S = cov(delta)
S
# Compute S^-1
x.invcov = solve(S)


# T^2 Statistics [The test statistics is the T^2 of Hotelling]
x.T2  <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0)    

# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)  #The treshold linked to the Fisher Quantile

# Actual Test: 
x.T2 < cfr.fisher   # Rejection region: R_alfa = {x.T2>cfr.fisher}    
#     Reject if:  T^2 {Under H0} > (n-1)p/(n-p) Fisher_{1-alpha}(p,n-p)
#     (we reject for large values of the T2 statistics, when we are outside the ellipse)


## Understand better the test:    P-Value
# Compute the p-value 
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)    # We except low value of the pvalue, becuse we are rejecting
P #0

# b) Bonferroni intervals:
alpha = 0.05
k <- dim(delta)[2]
n <- dim(delta)[1]
# Estimators for our delta
S <- cov(delta)
mean.delta <- x.mean
mean.delta
# Build the test:
q.T <- qt(1-alpha/(2*k),n-1)
Infer=Super=Media=NULL
for (ii in 1:k){
  e=rep(0,k)
  e[ii]=1
  Infer[ii] = t(e) %*% mean.delta - sqrt( t(e)%*%S%*%e / n) * q.T;
  Media[ii] = t(e) %*% mean.delta;
  Super[ii] = t(e) %*% mean.delta + sqrt( t(e)%*%S%*%e / n) * q.T;
}
IC = cbind(Infer, Media, Super)
colnames(IC) <- c("Inf", "Mean", "Sup")
rownames(IC) <- colnames(delta)
IC

# 3) test on the average value
avg.G <- (girona[,1]+girona[,2])/2
avg.T <- (terrassa[,1]+terrassa[,2])/2

delta.avg = avg.G-avg.T

shapiro.test(delta.avg)
plot(delta.avg)

media1 <- mean(delta.avg)
sigma1 <- var(delta.avg)
alpha = 0.05

# we use the 1 dimensional test, i.e. t-test
t.test(delta.avg, mu = 0, alternative = 'greater', conf.level = 1-alpha)    ## !!!
# we reject H0: mean of Girona is higher than mean of Terrassa.