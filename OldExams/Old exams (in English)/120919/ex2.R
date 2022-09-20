####

# set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# clean workspace:
graphics.off()
rm(list=ls())

# load the function for m shapiro test:
load("C:/Users/Marco Paggiaro/OneDrive/Desktop/mieidati/Applied Statistics/mcshapiro.test.RData")


# Import all the libraries:
library(class)
library(MASS)
library(car)

# data exploration:
stress<- read.table('stress.txt', header = T)
head(stress)

x1<-stress[1:10, ]
x2<-stress[11:15,]

#for each indicator
set.seed(123)

n1 <-10
n2<-5
n <- n1 + n2
T0 <- sapply(x1, median) - sapply(x2,median)
pval<-rep(0,8)

for (i in 1:8){
  
  # Test statistic: absolute difference between the two meedians
  x_pooled <- c(x1[, i],x2[, i])
  # CMC to estimate the p-value
  B <- 10000 # Number of permutations
  T_stat <- numeric(B) # Vector where we will store the values of T*
  
  for(perm in 1:B){
    # permutation:
    permutation <- sample(n)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- median(x1_perm) - median(x2_perm)
  }
  
  # p-value
  pval[i] <- sum(T_stat>=T0[i])/B
}

T0
pval

# b) Correct the p-values with 'fdr'. Which values have improved?
p.fdr <- p.adjust(pval, 'fdr')
which(p.fdr < 0.25)

