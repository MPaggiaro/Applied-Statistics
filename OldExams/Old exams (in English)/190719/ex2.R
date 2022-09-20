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
data <- read.table("buoys.txt", header = T)
head(data)
plot(data)

# a) Euclidean, single linkage.
# dissimilarity matrix:
distance <- dist(data[,c(1,2)], method = 'euclidean')
image(as.matrix(distance))

# hierarchical clustering:
data.cl <- hclust(distance, method = 'ward')

plot(data.cl, hang=-0.1, labels=FALSE, xlab='', sub='')
clus.info <- cutree(data.cl, k = 3)

# size of the clusters:
table(clus.info)
# centers of the cluster:
cluster_center = aggregate(data,list(cluster=clus.info),mean)
cluster_center

# data representation:
plot(data, col = clus.info+1)
points(cluster_center$duration, cluster_center$time, col = cluster_center$cluster+1,pch= 19,asp = 1)

# b) ANOVA test:
clus.info <- as.factor(clus.info)
# assumptions:
# 1. Gaussianity of the three groups
shapiro.test(data$DO[which(clus.info=='1')])
shapiro.test(data$DO[which(clus.info=='2')])
shapiro.test(data$DO[which(clus.info=='3')])

# 2. Same variance: Bartlett test:
bartlett.test(data$DO ~ clus.info)

# ready to perform the test!
help(aov)

fit <- aov(data$DO ~ clus.info)
summary(fit)
boxplot(data$DO~ clus.info)
# p-value 22% ! we don't have difference in means.