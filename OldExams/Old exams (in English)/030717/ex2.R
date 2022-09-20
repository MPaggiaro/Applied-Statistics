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

data <- read.table("bento.txt", header = T)
head(data)
attach(data)
# build the dataset on the differences:
diffdata <- data.frame (rice = rice_hanami - rice_nohanami,
                        sashimi = sashimi_hanami-sashimi_nohanami,
                        vegetables = vegetables_hanami - vegetables_nohanami,
                        okashi = okashi_hanami - okashi_nohanami)
plot(diffdata)

# a) statistical test:

# assumption: gaussianity.
mcshapiro.test(diffdata)
# OK!

n <- dim(diffdata)[1]
p <- dim(diffdata)[2]
alpha <- 0.05       # Set the value of the analysis
mu0 <- c(0,0,0,0)   # If there's no difference between the two the mean of delta should be zero

T.mean <- sapply(diffdata, mean)
T.mean
S <- cov(diffdata)
T.invcov <- solve(S)

T2  <- n * (T.mean-mu0) %*% T.invcov %*% (T.mean-mu0)    

# Radius of the ellipsoid
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha,p,n-p)  #The treshold linked to the Fisher Quantile

# Actual Test: 
T2 < cfr.fisher   # Rejection region: R_alfa = {x.T2>cfr.fisher}    
#     Reject if:  T^2 {Under H0} > (n-1)p/(n-p) Fisher_{1-alpha}(p,n-p)
#     (we reject for large values of the T2 statistics, when we are outside the ellipse)


## Understand better the test:    P-Value
# Compute the p-value 
P <- 1-pf(T2*(n-p)/((n-1)*p), p, n-p)    # We except low value of the pvalue, becuse we are rejecting
P #0

# c) Bonferroni intervals:
alpha = 0.05
k <- dim(diffdata)[2]
# Estimators for our delta

# Build the test:
q.T <- qt(1-alpha/(2*k),n-1)
Infer=Super=Media=NULL
for (ii in 1:k){
  e=rep(0,k)
  e[ii]=1
  Infer[ii] = t(e) %*% T.mean - sqrt( t(e)%*%S%*%e / n) * q.T;
  Media[ii] = t(e) %*% T.mean;
  Super[ii] = t(e) %*% T.mean + sqrt( t(e)%*%S%*%e / n) * q.T;
}
IC = cbind(Infer, Media, Super)
colnames(IC) <- c("Inf", "Mean", "Sup")
rownames(IC) <- colnames(diffdata)
IC

