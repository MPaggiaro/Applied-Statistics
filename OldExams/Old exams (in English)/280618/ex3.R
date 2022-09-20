#### EXAM: 28 Giugno 2018
#### PROBLEM 3
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

# dataset exploration:
data <- read.table("precolombian.txt", header = T)
head(data)
attach(data)
plot(Year, Aspect.Ratio, col=Civilization)

data1 <- data[,1:2]

# a) build a classifier:
levels(Civilization)
maya <-which(Civilization =='Maya')
aztec <- which(Civilization =='Aztec')
toltec <-which(Civilization =='Toltec')

# assumptions:
# 1) Gaussianity.
mcshapiro.test(data[maya,1:2])
mcshapiro.test(data[aztec,1:2])
mcshapiro.test(data[toltec,1:2])
# OK!

# 2) covariance structure:
S1 <- cov(data[maya,1:2])
S2 <- cov(data[aztec,1:2])
S3 <- cov(data[toltec,1:2])

x11()
par(mfrow=c(1,3))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. S3', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
# different variances -> use QDA!

qda.data <- qda(data1, Civilization, priors = c(0.2,0.1,0.7))
lda.data <- lda(data1, Civilization, priors = c(0.2,0.1,0.7))

qda.data
# Group means inside here!

# classification regions plot:
x11()
plot(data1, main='Civilizations', xlab='Year', ylab='Aspect.Ratio', pch=20)
points(data1[aztec,], col='red', pch=20)
points(data1[maya,], col='green', pch=20)
points(data1[toltec,], col='blue', pch=20)
legend("topright", legend=levels(Civilization), fill=c('red','green','blue'))

points(qda.data$means, col=c('red','green','blue'), pch=4, lwd=2, cex=1.5)

x  <- seq(min(Year), max(Year), length=200)
y  <- seq(min(Aspect.Ratio), max(Aspect.Ratio), length=200)
xy <- expand.grid(Year=x, Aspect.Ratio=y)

z  <- predict(qda.data, xy)$post  
z1 <- z[,1] - pmax(z[,2], z[,3])    
z2 <- z[,2] - pmax(z[,1], z[,3])    
z3 <- z[,3] - pmax(z[,1], z[,2])

contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
contour(x, y, matrix(z3, 200), levels=0, drawlabels=F, add=T)
# soooo coooollll!!!!

# c) compute the APER of the classifier:
prediction <- predict(lda.data)
# confusion matrix:
misc <-table(true.class = Civilization, assigned.class = prediction$class)
# why do we have to compute the modified APER?
prior <- c(0.2,0.1,0.7)
G <- length(prior)
APER <- 0
for(g in 1:G)
  APER <- APER + sum(misc[g,-g])/sum(misc[g,]) * prior[g]
APER

WrongAPER <- (sum(misc)-sum(diag(misc)))/sum(misc)
WrongAPER # very different, be careful!!!!

# c) Classification of a new datum:
x0 <- data.frame(Year = 986, Aspect.Ratio = 1.4)
predict(qda.data,x0)
# Maya
# Let's plot it to see if it fits:
points(x0, col='black', pch=20)
# perfect!

detach(data)
