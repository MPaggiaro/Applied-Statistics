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

# a) classifier based on types of coordinates:
debris <- read.table("debris.txt", header = T)
head(debris)
attach(debris)
plot(x,y, col='white')
points(debris[which(risk=="H"),1:2], col='red' )
points(debris[which(risk=="L"),1:2], col='green4')

# assumptions:
# 1) Gaussianity:
mcshapiro.test(debris[which(risk=="H"),1:2])
mcshapiro.test(debris[which(risk=="L"),1:2])
# the second group is not gaussian!
# idgaf, I use qda anyway.
coordinates <- debris[,1:2]
group.qda <- qda(coordinates, risk)
group.qda

points(group.qda$means, pch=4 , lwd=2, cex=1.5)

x  <- seq(min(debris[,1]), max(debris[,1]), length=200)
y  <- seq(min(debris[,2]), max(debris[,2]), length=200)
xy <- expand.grid(x=x, y=y)

z  <- predict(group.qda, xy)$post  
z1 <- z[,1] - z[,2] 
z2 <- z[,2] - z[,1]  


contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)

# b) Weaknesses of the model, AER
qdaCV <- qda(coordinates, risk, CV=TRUE)  # specify the argument CV

qdaCV$class
risk
table(class.true=risk, class.assignedCV=qdaCV$class)

errorsCV <- (qdaCV$class != risk)
errorsCV
sum(errorsCV)
AERCV   <- sum(errorsCV)/length(risk)
AERCV

# c)

set.seed(321)
errorKNN=rep(0,21)

for (k in 10:30){
  
  group.knn.cv <- knn.cv(train = coordinates, cl = risk, k = k)
  errors <- (group.knn.cv != risk)
  AERCV <- sum(errors)/length(risk)
  errorKNN[k-9] <- AERCV
  
}

plot(errorKNN)

# Plot the partition induced by knn  - case bivariate ì, 3 groups
k.best <- which.min(errorKNN) + 9
min(errorKNN)
k.best

plot(coordinates, main='knn')
points(debris[which(risk=="H"),1:2], col='red' )
points(debris[which(risk=="L"),1:2], col='green4')
legend("topright", legend=levels(risk), fill=c(2,3,4))

iris.knn <- knn(train = coordinates, test = xy, cl = risk, k = k.best)
iris.knn
z  <- as.numeric(iris.knn)

contour(x, y, matrix(z, 200), levels=c(1.5, 2.5), drawlabels=F, add=T)

# d) prediction:

new.pred <- knn(train = coordinates, test = c(1, -4), cl = risk, k = k.best)
new.pred
