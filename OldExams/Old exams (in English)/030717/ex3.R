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


library(mvtnorm)
library(rgl)
library(car)


data <- read.table("geisha.txt", header = T)
head(data)
plot(data)

# a) Euclidean, single linkage.
# dissimilarity matrix:
distance <- dist(data, method = 'euclidean')
image(as.matrix(distance))

# hierarchical clustering:
data.cl <- hclust(distance, method = 'single')
summary(data.cl)      

plot(data.cl, hang=-0.1, labels=FALSE, xlab='', sub='')
clus.info <- cutree(data.cl, k = 2)

# size of the clusters:
table(clus.info)
# centers of the cluster:
cluster_center = aggregate(data,list(cluster=clus.info),mean)
cluster_center

# data representation:
plot(data, col = clus.info+1)
points(cluster_center$duration, cluster_center$time, col = cluster_center$cluster+1,pch= 19,asp = 1)

# cophenetic coefficient:
coph_matr <- cophenetic(data.cl)
coph_coeff <- cor(distance,coph_matr)
coph_coeff
# not bad

# b) result unsatisfactory.
# we try with multiple and average linkage:
ave.link <- hclust(distance, method = 'average')
comple.link <- hclust(distance, method = 'complete')


plot(ave.link, hang=-0.1, labels=FALSE, xlab='', sub='')
plot(comple.link, hang=-0.1, labels=FALSE, xlab='', sub='')

# how are groups subdivided?
clus.a <- cutree(ave.link, k = 2)
clus.c <- cutree(comple.link, k = 2)

plot(data, col = clus.a+1)
plot(data, col= clus.c+1)
#points(cluster_center$duration, cluster_center$time, col = cluster_center$cluster+1,pch= 19,asp = 1)

# same result! we pick average linkage

table(clus.a)
table(clus.c)

# c) Bonferroni intervals:
succ<- data[which(clus.a == '1'),]
plot(succ) 
unsucc <- data[which(clus.a == '2'),]

# covariances:
S1 <- cov(succ)
S2 <- cov(unsucc)
n1 <- dim(succ)[1]
n2 <- dim(unsucc)[1]

n <- dim(data)[1]
alpha <- 0.01
p <- 2
k <- 4

# pooled covariance:
Sp <- ((n1-1)*S1+(n2-1)*S2)/(n-2)
# quantile for Bonferroni's:
cfr.fisher <- ((n-1)*p/(n-p))*qf(1-alpha/(2*k),p,n-p)

# building the CIs:
# 1) difference
meanDiff <- colMeans(succ)-colMeans(unsucc)
bonf.diff <- rbind(meanDiff - sqrt((1/n1+1/n2)*cfr.fisher*diag(Sp)),
                   meanDiff,
                   meanDiff + sqrt((1/n1+1/n2)*cfr.fisher*diag(Sp)))
row.names(bonf.diff)=c("inf","mean","sup")
bonf.diff

# 2) successful:
bonf.succ <- rbind(colMeans(succ) - sqrt(cfr.fisher/n1*diag(S1)),
                   colMeans(succ),
                   colMeans(succ) + sqrt(cfr.fisher/n1*diag(S1)))
row.names(bonf.succ)=c("inf","mean","sup")
bonf.succ
colMeans(succ)

# d) strategy:
# - go out at 16:45
# - search for a 1.30 hours
