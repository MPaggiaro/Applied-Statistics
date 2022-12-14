###--------------------###
### LAB 4 (03/04/2020) ###
###--------------------###

### TOPIC:
### Testing for multivariate normality

setwd("D:/RTDA/Didattica/Applied Statistics MATE 19-20/Lab 4 - 03042020")

library(mvtnorm)
library(mvnormtest)

#_______________________________________________________________________________
##### Testing the hypothesis of multivariate normality

# We generate a sample of n=150 observation from bivariate Gaussian (as in LAB_3.R)

mu <- c(1,2)
mu
sig <- matrix(c(1,1,1,2), 2)
sig

n   <-  150

set.seed(20200403)
X <- rmvnorm(n, mu, sig)

x.1 <- seq(-4,6,0.15); x.2 <- seq(-4,8,0.15)
w <- matrix(NA, length(x.1), length(x.2))
for(i in 1:length(x.1)){  
  for(j in 1:length(x.2)){
    w[i,j] <- dmvnorm(c(x.1[i],x.2[j]),mu,sig)
  }
}

x11()
image(x.1, x.2, w,asp=1, ylim=c(-4,8), main="Sample points")
points(X[,1],X[,2],pch=20,cex=.75)

dev.off()

### Test of normality
#_______________________________________________________________________________
### Approach 1: look at some linear combinations of the original variables
### Example: original variables, principal components

x11()
par(mfrow=c(2,2))

hist(X[,1], prob=T, ylab='density', xlab='X.1', main='Histogram of X.1',ylim=c(0,0.45))
lines((-1000):1000 /100, dnorm((-1000):1000 /100,mean(X[,1]),sd(X[,1])), col='blue', lty=2)

hist(X[,2], prob=T, ylab='density', xlab='X.2', main='Histogram of X.2',ylim=c(0,0.45))
lines((-1000):1000 /100, dnorm((-1000):1000 /100,mean(X[,2]),sd(X[,2])), col='blue', lty=2)

qqnorm(X[,1], main='QQplot of X.1',xlab='theoretical quantiles', ylab='sample quantiles')
qqline(X[,1])

qqnorm(X[,2], main='QQplot of X.2',xlab='theoretical quantiles', ylab='sample quantiles')
qqline(X[,2])

# we perform univariate tests of normality on the two components
shapiro.test(X[,1])
shapiro.test(X[,2])

# Recall: Shapiro-Wilk test
# H0: X~N vs H1: H0^c
# test statistics: W=(angular coeff. of the qqline)^2/sample variance
# One can prove that:
# - w<1
# - the empirical distribution under H0 is concentrated on values near 1 
#   (if n=50 more than 90% of the obs. is between 0.95 and 1)
#   (if n=5 more than 90% of the obs. is between 0.81 and 1).
# 
# If the data do NOT come from a Gaussian distribution, the distribution of 
# the test statistics moves toward smaller values:
# Small values of the statistics W give evidence against H0
# => reject H0 for small values of W

# we look at the directions of the PCs
pc.X <- princomp(X,scores=T)
#pc.X$scores

x11()
par(mfrow=c(2,2))
hist(pc.X$scores[,1], prob=T, ylab='density', xlab='comp.1', main='Histogram of PC1',ylim=c(0,0.41))
lines((-1000):1000 /100, dnorm((-1000):1000 /100,mean(pc.X$scores[,1]),sd(pc.X$scores[,1])), col='blue', lty=2)
hist(pc.X$scores[,2], prob=T, ylab='density', xlab='comp.2', main='Histogram of PC2',ylim=c(0,0.7))
lines((-1000):1000 /100, dnorm((-1000):1000 /100,mean(pc.X$scores[,2]),sd(pc.X$scores[,2])), col='blue', lty=2)
qqnorm(pc.X$scores[,1], main='QQplot of PC1',xlab='theoretical quantiles', ylab='sample quantiles')
qqline(pc.X$scores[,1])
qqnorm(pc.X$scores[,2], main='QQplot of PC2',xlab='theoretical quantiles', ylab='sample quantiles')
qqline(pc.X$scores[,2])

shapiro.test(pc.X$scores[,1])
shapiro.test(pc.X$scores[,2])

# Problem:
# Which level should I use in each test, to get a global level of alpha?

#_______________________________________________________________________________
### Approach 2
### Consider the squared Mahalanobis distances of the data from the (sample) mean
### and test if they are a sample from a chi-square distribution

# Recall:
# Theorem: if X~N(mu,Sigma) r.v. in R^p, det(Sigma)>0
#          then d2(X,mu)=(X-mu)'Sigma^-1(X-mu) ~ Chi-sq(p)

x11()
plot(X, asp=1,xlab='X.1',ylab='X.2')

library(car)
for(prob in (1:14)/15)
  dataEllipse(X, levels = prob , add=T)

d2 <- mahalanobis(X, colMeans(X), cov(X))   

x11(width=13)
par(mfrow=c(1,2))
hist(d2, prob=T, main='Histogram of the Mahalanobis dist.',xlab='d2',ylab='density', col='grey84')
lines(0:2000/100, dchisq(0:2000/100,2), col='blue', lty=2, lwd=2)
qqplot(qchisq((1:n - 0.5)/n, df = 2), d2, main='QQplot of (sample) d2',xlab='theoretical quantiles Chi-sq(2)',
       ylab='sample quantiles')
abline(0, 1)

### we can perform a chi.sq goodness-of-fit test

d2.class <- cut(d2, qchisq((0:10)/10, df = 2))
d2.freq  <- table(d2.class)

chisq.test(x = d2.freq, p = rep(1/10, 10), simulate.p.value = T)
# Test: does the population probabilities (given in x) equal those in p?

# Remark: since the mean and covariance matrix are unknown, we can only
#     have approximate solutions:
#     the Mahalanobis distance is computed with estimates of the mean vector
#     and of the covariance matrix; the sample of distances is not iid

graphics.off()

#_______________________________________________________________________________
### Approach 3: test of all the directions simultaneously, by looking at the min
### of the Shapiro-Wilk statistics

# We reject H0: X ~ N if we observe a "low" value of W along at least 
# one direction, i.e., if the minimum of W along the direction is "low" 

# Looking at all directions, is equivalent to looking at min(W) along
# the directions (test statistic)

# Example with our simulated data: we compute the W statistics for all
# the directions.
theta   <- seq(0, pi - pi/180, by = pi/180)
W       <- NULL
P       <- NULL
for(i in 1:length(theta))
{
  a   <- c(cos(theta[i]), sin(theta[i]))
  w   <- shapiro.test(X %*% a)$statistic
  p   <- shapiro.test(X %*% a)$p.value
  W   <- c(W, w)
  P   <- c(P, p)
}

x11()
par(mfrow = c(2,1))
plot(theta, W, main = 'W statistics', ylim = c(0.95,1), type='l')
abline(v=c(0, pi/2), col = 'blue')
abline(v= atan(princomp(X)$loadings[2,]/princomp(X)$loadings[1,]), col='red')
abline(v= atan(princomp(X)$loadings[2,]/princomp(X)$loadings[1,]) + pi, col='red')

plot(theta, P, main = 'P-values', ylim = c(0,1), type='l')
abline(v=c(0, pi/2), col = 'blue')
abline(h=0.10, col = 'blue', lty = 2) # set alpha=10%
abline(v= atan(princomp(X)$loadings[2,]/princomp(X)$loadings[1,]), col='red')
abline(v= atan(princomp(X)$loadings[2,]/princomp(X)$loadings[1,]) + pi, col='red')

# Note: to set the rejection region, we should look at the distribution of 
# of min(W) under H0 [not the distribution of W.a for all the directions a]
# --> To see how much we reject globally if we set a threshold
#     alpha=10% at the univariate tests based on W.a, see Experiment.R

# Hence, to build the rejection region for the test, 
# we just need to set the threshold as the quantile of order alpha
# of the distribution of min(W) under H0

# Formally: 
# H0: X ~ N vs H1: H0^c
# test statistics: min(W) ~ F under H0
# Reject H0 if min(W)<qF(alpha), 
# with qF(alpha) s.t. P(min(W) < qF(alpha)|H0) = alpha

# The distribution F of min(W) is not known, but it can be approximated with
# a Monte Carlo method. That is, we approximate the distribution of min(W) with a 
# histogram generated by simulating Gaussian samples. The quantile qF(alpha) is
# estimated with the sample quantile of order alpha from the samples.

# Note: an explicit expression is available for min(W). It is computed 
# by the function mshapiro.test 

# Example: for 200 sample we can compute min(W) and look at its distribution
min.W=NULL
for(i in 1:200)
{
  Y <- rmvnorm(n, mu, sig)
  min.W <- c(min.W, mshapiro.test(t(Y))$stat)
}
x11()
hist(min.W, prob=T, col='grey81', main='Histogram of min(W)'); box()
abline(v=quantile(min.W, probs = .1), col=2)
text(quantile(min.W, probs = .1), 85, labels = expression(q[F](1-alpha)),
     pos=2)

# => I'll reject H0 if min(W)<qF(alpha), with qF(alpha)~.98
quantile(min.W, probs = .1)

# Actual observation of min(W)
min.W0 = mshapiro.test(t(X))$stat
min.W0 # accept H0

abline(v=min.W0, col='blue')

# Approximate the p-value with Monte Carlo: count how many realizations under H0
# are associated with a min(W) lower than the actual observation

# Proportion of the realization that has min(W) lower than min.W0
sum(min.W < min.W0)/200

# Very high p-value => accept H0

### The function mcshapiro.test implements a procedure that:
### 1- approximates the distribution of the statistics min(W) via MC
### 2- performs a test of normality based on the (approximate) distribution of min(W)
### 3- returns an approximate p-value of the test at point 2-
mcshapiro.test <- function(X, devstmax = 0.01, sim = ceiling(1/(4*devstmax^2)))
{
  library(mvnormtest)
  n   <- dim(X)[1]
  p   <- dim(X)[2]
  mu  <- rep(0,p)
  sig <- diag(p)
  W   <- NULL
  for(i in 1:sim)
  {
    Xsim <- rmvnorm(n, mu, sig)
    W   <- c(W, mshapiro.test(t(Xsim))$stat)
    # mshapiro.test(X): compute the statistics min(W) for the sample X
  }
  Wmin   <- mshapiro.test(t(X))$stat   # min(W) for the given sample
  pvalue <- sum(W < Wmin)/sim          # proportion of min(W) more extreme than the observed Wmin
  devst  <- sqrt(pvalue*(1-pvalue)/sim)
  list(Wmin = as.vector(Wmin), pvalue = pvalue, devst = devst, sim = sim)
}

mcshapiro.test(X)

graphics.off()

#_______________________________________________________________________________
# Example: test of normality for the dataset stiff
# Dataset: each sample unit is a board. For each board, four measures of 
# stiffness are taken (X1: sending a shock wave, X2: while vibrating the board, 
# X3 and X4: static tests)

stiff <- read.table('stiff.dat')
stiff

x11()
plot(stiff, asp=1, pch=19)
# we can already observe that we have a couple of outliers, maybe not
# compatible with gaussianity assumption

# Normality of the components
x11(width=12)
par(mfcol=c(2,4))

for(i in 1:4)
{
  hist(stiff[,i], prob=T, main=paste('Histogram of V', i, sep=''), xlab=paste('V', i, sep=''))
  lines(900:2800, dnorm(900:2800,mean(stiff[,i]),sd(stiff[,i])), col='blue', lty=2)
  qqnorm(stiff[,i], main=paste('QQplot of V', i, sep=''))
  qqline(stiff[,i])
  print(shapiro.test(stiff[,i])$p)
}
# we see some points moving away from gaussian values.

# Normality of the principal components
PCs <- data.frame(princomp(stiff)$scores)

x11()
plot(PCs, asp=1, pch=19)

x11(width=13)
par(mfcol=c(2,4))
for(i in 1:4)
{
  hist(PCs[,i], prob=T, main=paste('Histogram of PC', i, sep=''))
  lines(seq(min(PCs[,i]), max(PCs[,i]), length=2000), dnorm(seq(min(PCs[,i]), max(PCs[,i]), length=2000),mean(PCs[,i]),sd(PCs[,i])), col='blue', lty=2)
  qqnorm(PCs[,i], main=paste('QQplot of PC', i, sep=''))
  qqline(PCs[,i])
  print(shapiro.test(PCs[,i])$p)
}
 # for the second component, Shapiro test is very small -> not gaussian!


# Mahalanobis distances of the data from the sample mean
M <- colMeans(stiff)
S <- cov(stiff)

d2 <- matrix(mahalanobis(stiff, M, S))

windows()
par(mfrow=c(1,2))

hist(d2, prob=T)
lines(0:2000/100, dchisq(0:2000/100,4), col='blue', lty=2)

qqplot(qchisq(seq(0.5/30, 1 - 0.5/30 ,by = 1/30), df = 4), d2,  main='QQplot di d2')
abline(0, 1)
# qqplot gives us some doubts, in the last points.

# what does it do???
d2.class <- cut(d2, qchisq((0:6)/6, df = 3))
d2.freq  <- table(d2.class)

chisq.test(x = d2.freq, p = rep(1/6, 6), simulate.p.value = T)
# p-value very high: we do not have enough evidence to reject the gaussian
# hypothesis

# test of all the directions simultaneously
mcshapiro.test(stiff)

 ### The data don't seem Gaussian. What can we do?
### Identify clusters 
### Identify (and possibly remove) outliers
### Transform the data (e.g., Box-Cox transformations, see Johnson-Wichern Chap.4.8,
###                     R functions powerTransform(); bcPower())
### Work without the Gaussian assumption (e.g., permutation tests)

### Let's try to identify and remove outliers:
# We remove the data too far (in the sense of the Mahalanobis distance) 
# from the center of the distribution

x11()
plot(d2, pch=ifelse(d2<7.5,1,19))
x11()
plot(stiff, pch=ifelse(d2<7.5,1,19))

stiff.noout <- stiff[which(d2<7.5),]

mcshapiro.test(stiff.noout)
# now we don't have evidence against gaussianity assumption!

# In this case removing the outliers solves the problem of non-gaussianity

