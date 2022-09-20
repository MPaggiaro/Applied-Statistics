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
library(fda)

# data import:
data <- read.table("watertemp.txt", header = T)
head(data)

matplot(t(data[,1:365]),type = "l")

# a) Fourier basis smoothing:

basis <- create.fourier.basis(rangeval = c(1,365),nbasis=45)
basis1 <- create.bspline.basis(rangeval = c(1,365),nbasis=10)
plot(basis)
plot(basis1)
Xsp <- smooth.basis(argvals = 1:365, y = t(data[,1:365]), fdParobj = basis)
# coefficients here:
head(Xsp)
head(Xsp$fd$coefs)

smoothed <- eval.fd(1:365, Xsp$fd)
head(smoothed)

matplot(smoothed, type="l")

# b) PCA

pca.wt <- pca.fd(Xsp$fd, nharm = 5, centerfns = T)
# variance explained:
sum(pca.wt$varprop)

# plot of first three eigenfnctns:

plot(pca.wt)

# other possibility:
mean.F <- mean(Xsp$fd)
mean.F <- eval.fd(1:365, mean.F)
x11()
par(mfrow = c(1,3))
plot(mean.F,type = 'l' )
harm1 <- mean.F + sd(pca.wt$scores[,1])*eval.fd(1:365,pca.wt$harmonics[1,])
harm2 <- mean.F - sd(pca.wt$scores[,1])*eval.fd(1:365,pca.wt$harmonics[1,])
lines(1:365, harm1, col="palegreen1")
lines(1:365, harm2, col="indianred1")

plot(mean.F,type = 'l' )
harm1 <- mean.F + sd(pca.wt$scores[,2])*eval.fd(1:365,pca.wt$harmonics[2,])
harm2 <- mean.F - sd(pca.wt$scores[,2])*eval.fd(1:365,pca.wt$harmonics[2,])
lines(1:365, harm1, col="palegreen1")
lines(1:365, harm2, col="indianred1")

plot(mean.F,type = 'l' )
harm1 <- mean.F + sd(pca.wt$scores[,3])*eval.fd(1:365,pca.wt$harmonics[3,])
harm2 <- mean.F - sd(pca.wt$scores[,3])*eval.fd(1:365,pca.wt$harmonics[3,])
lines(1:365, harm1, col="palegreen1")
lines(1:365, harm2, col="indianred1")

# first two components:
layout(cbind(1,2))
plot(pca.wt$harmonics[1,], col = 1, ylab = 'FPC1')
abline(h=0, lty=2)
plot(pca.wt$harmonics[2,], col = 2, ylab = 'FPC2')

# scree plot:
par(mfrow=c(1,1))
plot(cumsum(pca.wt$varprop)/sum(pca.wt$varprop), type='b', ylim = c(0,1))

# c) Scores along first 2 PCs:
# which is which:
i1 <- which(data$Zone == "Surface")
i2 <- which(data$Zone == "Medium")
i3 <- which(data$Zone == "Deep")

col.stat <- rep(NA, 132)
col.stat[i1] <- "lightblue"
col.stat[i2] <- "steelblue1"
col.stat[i3] <- "navy"

mat.scores1 <- matrix(0,365,132)
for (i in 1:132){
  mat.scores1[,i]=pca.wt$scores[i,1]*eval.fd(1:365, pca.wt$harmonics[1,])
}

matplot(1:365, mat.scores1, col = col.stat, type = "l")
# the water is hotter in the surface and colder in deep water.

mat.scores2 <- matrix(0,365,132)
for (i in 1:132){
  mat.scores2[,i]=pca.wt$scores[i,2]*eval.fd(1:365, pca.wt$harmonics[2,])
}

matplot(1:365, mat.scores2, col = col.stat, type = "l")

mat.scores3 <- matrix(0,365,132)
for (i in 1:132){
  mat.scores3[,i]=mean.F + pca.wt$scores[i,1]*eval.fd(1:365, pca.wt$harmonics[1,])+pca.wt$scores[i,2]*eval.fd(1:365, pca.wt$harmonics[2,])
}

matplot(1:365, mat.scores3, col = col.stat, type = "l")
# difference in first PCA!

# we can also plot the PCs classically:
plot(pca.wt$scores[,1],pca.wt$scores[,2],col = col.stat,xlab="Scores FPC1",ylab="Scores FPC2",lwd=2)
