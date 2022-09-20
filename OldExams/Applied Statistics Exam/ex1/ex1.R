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
pollution <- read.table("pollution.txt", header = T)
head(pollution)
plot(pollution)

# a) Testing
alpha <- 0.05

# assumption: Gaussianity:
mcshapiro.test(pollution)
# p-value 22% ok

n <- dim(pollution)[1]  # 11
p <- dim(pollution)[2]  #  2

x.mean   <- sapply(pollution,mean)
x.cov    <- cov(pollution)
x.invcov <- solve(x.cov)

mu.0 <- c(50,50)

x.T2 <- n * (x.mean-mu.0) %*% x.invcov %*% (x.mean-mu.0)
x.T2

cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)
cfr.fisher

D.T2 < cfr.fisher # FALSE: we reject H0 at level 5%

# we compute the p-value
P <- 1-pf(D.T2*(n-p)/(p*(n-1)), p, n-p)
P
# circa zero

# b) ellipse:
plot(pollution, asp=1, pch=1)
ellipse(center=x.mean, shape=x.cov/n, radius=sqrt(cfr.fisher), lwd=2)
points(mu.0[1], mu.0[2], pch=16, col='grey35', cex=1.5)
abline(h=mu.0[1], v=mu.0[2], col='grey35')

# directions:
eigen(x.cov/n)$vectors

# lengths: 
sqrt(eigen(x.cov/n)$values*cfr.fisher)

# c) NO! see figure

# d)
IC.T2.PM2.5 <- c( x.mean[1]-sqrt(cfr.fisher*x.cov[1,1]/n) , x.mean[1], x.mean[1]+sqrt(cfr.fisher*x.cov[1,1]/n) )
IC.T2.PM10  <- c( x.mean[2]-sqrt(cfr.fisher*x.cov[2,2]/n) , x.mean[2], x.mean[2]+sqrt(cfr.fisher*x.cov[2,2]/n) )

T2 <- rbind(IC.T2.PM2.5, IC.T2.PM10)
dimnames(T2)[[2]] <- c('inf','center','sup')
T2
