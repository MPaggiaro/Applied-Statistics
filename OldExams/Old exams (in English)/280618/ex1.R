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

# data exploration:
morning <- read.table("morning.txt", header = T)
head(morning)

evening <- read.table("evening.txt", header = T)
head(evening)

boxplot(morning)
boxplot(evening)

plot(morning)
plot(evening)

# a) test on the difference of the means:
diff <- data.frame(MEX.OAX = morning[,1]-evening[,1], OAX.MEX = morning[,2]-evening[,2])
head(diff)
plot(diff)

# assumption: Gaussianity of diff.
mcshapiro.test(diff)
# the data is gaussian!

# multivariate test:
n <- dim(diff)[1]
p <- dim(diff)[2]
alpha <- 0.01       # Set the value of the analysis
mu0 <- c(0,0)   # If there's no difference between the two the mean of delta should be zero


x.mean = c(mean(diff[,1]), mean(diff[,2]))
x.mean
S = cov(diff)
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

# Ellipsoidal confidence region with confidence level 95%
plot(diff, asp=1, pch=1, main='Dataset of the Differences')
ellipse(center=x.mean, shape=S/n, radius=sqrt(cfr.fisher), lwd=2)

points(mu0[1], mu0[2], pch=16, col='grey35', cex=1.5)
abline(h=mu0[1], v=mu0[2], col='grey35')

# What if we set the radius as the quantile of order 1-pval?
ellipse(center=x.mean, shape=S/n, radius=sqrt((n-1)*p/(n-p)*qf(1-as.numeric(P),p,n-p)),lty=1,col='dark grey',lwd=2)

# Result: there is a significant mean in the delay. In the morning we have 
# smaller delay.

# b) 4 Bonferroni intervals on the mean delay in morning flights and in
# evening flights.

data <- cbind(morning,evening)
head(data)
colnames(data) = c("Mex-Oax morning","Oax-Mex morning",
                   "Mex-Oax evening","Oax-Mex evening")

# verifying the Gaussian assumption:
mcshapiro.test(data)
# good p-value: Gaussian!

## BONFERRONI FOR THE MEAN COMPONENT OF A GAUSSIAN VECTOR
alpha = 0.01
k <- dim(data)[2]
n <- dim(data)[1]
# Estimators for our dataset
S <- cov(data)
mean.dataset <- sapply(data,mean)
mean.dataset
# Build the test:
q.T <- qt(1-alpha/(2*k),n-1)
Infer=Super=Media=NULL
for (i in 1:k){
  e=rep(0,k)
  e[i]=1
  Infer[i] = t(e) %*% mean.dataset - sqrt( t(e)%*%S%*%e / n) * q.T;
  Media[i] = t(e) %*% mean.dataset;
  Super[i] = t(e) %*% mean.dataset + sqrt( t(e)%*%S%*%e / n) * q.T;
}
IC = cbind(Infer, Media, Super)
colnames(IC) <- c("Inf", "Mean", "Sup")
rownames(IC) <- colnames(data)
IC

# c) Yes! Expected worst delay is 52 minutes, so in 99% of the cases I will
# arrive before 11:15.

# BUT it would be better to do a single interval test here!
# OM.morning <- data[,2]: not needed!
# we provide a one-at-the-time CI:
q.T <- qt(1-alpha,n-1)
e=rep(0,k)
e[2]=1
Infer = t(e) %*% mean.dataset - sqrt( t(e)%*%S%*%e / n) * q.T;
Media = t(e) %*% mean.dataset;
Super = t(e) %*% mean.dataset + sqrt( t(e)%*%S%*%e / n) * q.T;

IC_prevision <- cbind(Infer,Media,Super)
IC_prevision
