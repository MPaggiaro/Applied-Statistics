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

sequoia <- read.table("sequoia.txt", header = T)
head(sequoia)
plot(sequoia)

# a) cluster analysis:
d = dist(sequoia)
clusts <- hclust(d, method='ward.D2')
plot(clusts, hang=0, labels=FALSE, main='Ward', xlab='', sub='')

clusters <- cutree(clusts, k=5)
plot(sequoia, col=clusters+2)
table(clusters)
centroids = aggregate(sequoia, list(cluster=clusters), median)
centroids
points(centroids[,2:3])

# b) Bonferroni CI's on mean and variance of clusters:
alpha <- 0.1
k <- 10 #ten CI's.

# mean and variance of the diameter of trees:
i1 = which(clusters == 1)
i2 = which(clusters == 2)
i3 = which(clusters == 3)
i4 = which(clusters == 4)
i5 = which(clusters == 5)

# assumption: Gaussianity of the groups.
n <- table(clusters)

# Shapiro test on the variables:
shapiro.test(sequoia[i1,2])
shapiro.test(sequoia[i2,2])
shapiro.test(sequoia[i3,2])
shapiro.test(sequoia[i4,2])
shapiro.test(sequoia[i5,2])
# OK!

# mean: t-test
ICmean<-data.frame(0, 5, 3)
ICvar<-matrix(0, 5, 3)

for (i in 1:5){
  ICmean[i, ] <- cbind(inf=mean(sequoia[which(clusters==i),2]) - sqrt(var(sequoia[which(clusters == i), 2])/n[i]) * qt(1 - alpha/(2*k), n[i]-1),
                       center= mean(sequoia[which(clusters==i),2]),
                       sup=mean(sequoia[which(clusters==i),2]) + sqrt(var(sequoia[which(clusters == i), 2])/n[i]) * qt(1 - alpha/(2*k), n[i]-1))
  
  ICvar[i, ] <- cbind(inf=var(sequoia[which(clusters == i), 2])*(n[i]-1) / qchisq(1 - alpha/(2*k), n[i]-1),
                      center=var(sequoia[which(clusters == i), 2]),
                      sup=var(sequoia[which(clusters == i), 2])*(n[i]-1) / qchisq(alpha/(2*k), n[i]-1))
  
}

ICmean
ICvar
