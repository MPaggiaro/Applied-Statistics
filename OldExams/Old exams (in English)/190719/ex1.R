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

# data import:
pines <- read.table("pines.txt", header = T)
head(pines)
plot(pines)

# a) Testing:
# assumption: Gaussianity.
mcshapiro.test(pines)
# OKay! p value 10%
alpha <- 0.01
mu0 <- c(14.2350,42.4520)

x.mean <- sapply(pines, mean)
x.cov <- cov(pines)
x.invcov <- solve(x.cov)
n <- dim(pines)[1]
p <- dim(pines)[2]
x.T2       <- n * (x.mean-mu0) %*% x.invcov %*% (x.mean-mu0) 
# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
# Test: 
x.T2 < cfr.fisher   # no statistical evidence to reject H0 at level alpha
# Rejection region: {x.T2>cfr.fisher}
# (we reject for large values of the T2 statistics)

# Compute the p-value 
P <- 1-pf(x.T2*(n-p)/((n-1)*p), p, n-p)
P

# b) Elliptical region:
plot(pines, asp = 1)
ellipse(mu0, shape=x.cov/n, sqrt(cfr.fisher), col = 'blue', lty = 2, center.pch = 16)
help("dataEllipse")
dataEllipse(pines$Long,pines$Lat, levels = 0.99)

# directions of the axes:
eigen(x.invcov)
eigen(x.cov)
