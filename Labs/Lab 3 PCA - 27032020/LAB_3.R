###--------------------###
### LAB 3 (27/03/2020) ###
###--------------------###

### TOPIC:
### Principal Component Analysis

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

#_______________________________________________________________________________
##### Principal component analysis of a simulated dataset 

# Generate the data
# multivariate gaussian data: mvtnorm
library(mvtnorm)

mu  <-  c(1,2)
sig <-  cbind(c(1,1), c(1,4))
n   <-  100

set.seed(27032020)
X <- rmvnorm(n, mu, sig)
# random numbers from a normal distribution.

# we plot the data
x11()
plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=19, xlim=c(-6,6), ylim=c(-5,10))

# we plot the sample mean
points(colMeans(X)[1], colMeans(X)[2], col='red', pch=19,lwd=3)

# we plot the projection on the x- an y-axis and compute their variance
abline(h=colMeans(X)[2], lty=2, col='grey')
points(X[,1], rep(colMeans(X)[2], n), col='red')
var(X[,1])

abline(v=colMeans(X)[1], lty=2, col='grey')
points(rep(colMeans(X)[1], n), X[,2], col='red')
var(X[,2])

# let's compute the variance along all the directions
theta   <- seq(0, pi, by = 2*pi/360)
# for now, Var is empty.
Var     <- NULL


# Example: along the direction with angle theta:
theta[30]
abline(a = colMeans(X)[2] - tan(theta[30])*colMeans(X)[1], b = tan(theta[30]), lty=2)
a <- c(cos(theta[30]), sin(theta[30]))
proj30=(a)%*%(t(X)-colMeans(X))
points(colMeans(X)[1]+cos(theta[30])*proj30, 
       colMeans(X)[2]+sin(theta[30])*proj30, col='red')
var(X %*% a)

# For cycle
for(i in 1:length(theta))  # for i between 1 and length(theta) ripeat:
{
  a   <- c(cos(theta[i]), sin(theta[i])) # unit vector in direction theta[i]
  v   <- var(X %*% a)                    # sample variance of the projection of X along the direction identified by vector a
  Var <- c(Var, v)
}

# we can compute the min and max of the variance
max.var   <- max(Var)
max.theta <- theta[which.max(Var)]
max.a     <- c(cos(max.theta), sin(max.theta))

min.var   <- min(Var)
min.theta <- theta[which.min(Var)]
min.a     <- c(cos(min.theta), sin(min.theta))


# Graphically:
x11(width=10, height=7)
par(mfrow=c(1,2))
plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=20)
abline(a = colMeans(X)[2] - tan(max.theta)*colMeans(X)[1], b = tan(max.theta), lty = 4, col = 'navyblue', lwd = 2)
abline(a = colMeans(X)[2] - tan(min.theta)*colMeans(X)[1], b = tan(min.theta), lty = 4, col = 'blue', lwd = 2)

plot(theta, Var, type = 'l', col='dark grey', lwd = 2,ylab='Variance')
points(max.theta, max.var, pch=16, col='navyblue')
points(min.theta, min.var, pch=16, col='blue')

### Let's verify the theory
# we compute the sample covariance matrix 

M <- colMeans(X)
S <- cov(X)

# we compute the eigenvectors and eigenvalues
eigen(S)
# Note. eigen(S)$vectors returns a matrix whose 
#     columns are the eigenvectors of S

# we compare with the values and directions of max/min variability 
# we found empirically
max.var
min.var
max.a
min.a

# let's plot the directions of max/min variability
par(mfrow=c(1,2))
plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=20)
ellipse(M, S, 1, add=T,lwd=3)

abline(a = M[2] - eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1]*M[1], b = eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1], lty = 2, col = 'dark red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2]*M[1], b = eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2], lty = 2, col = 'red', lwd = 2)
abline(a = M[2] - tan(max.theta)*M[1], b = tan(max.theta), lty = 4, col = 'navyblue', lwd = 2)
abline(a = M[2] - tan(min.theta)*M[1], b = tan(min.theta), lty = 4, col = 'blue', lwd = 2)

plot(theta, Var, type = 'l', col='dark grey', lwd = 2,ylab='Varianza')
points(max.theta, max.var, pch=20, col='navyblue')
points(min.theta, min.var, pch=20, col='blue')
points(atan(eigen(S)$vector[2,1]/eigen(S)$vector[1,1]), max.var, pch=3, col='dark red')
points(atan(eigen(S)$vector[2,2]/eigen(S)$vector[1,2])+pi, min.var, pch=3, col='red')

graphics.off()

#_______________________________________________________________________________
##### Principal component analysis of the dataset 'tourists'

# The dataset "tourists.txt" collects the data on the flow of Italian tourism
# from outside Lombardy to Milan for the year 2015. Each statistical unit 
# corresponds to a Region of origin and a month of observation. For each unit,
# the tourists' flow is quantified through the number of nights spent by clients
# in: `5 stars hotels', `4 stars hotels', `3 stars hotels', `2 stars hotels', 
# `1 star hotels', `residences', `B&B' and `rented flats'.

tourists <- read.table('tourists.txt', header=T)
head(tourists)

# we keep the first two columns in the variable tourist.label
tourists.label <- tourists[,1:2]
# then we remove it from the data set.
tourists <- tourists[,-(1:2)]

n <- dim(tourists)[1]
p <- dim(tourists)[2]

# Boxplot
x11()
par(mar=rep(8,4))
boxplot(tourists, las=2, col='gold')

# We observe that the variability of the number of nights in 3,4 stars hotels and residences
# is higher than that of the others. This may influence the PCA

# Note: PCA is not about the mean, it is about the variability
x11()
par(mar=rep(8,4))
boxplot(scale(x=tourists,center = T, scale=F), las=2, col='gold')

# We perform the PCA on original data
pc.tourists <- princomp(tourists, scores=T)
pc.tourists
summary(pc.tourists)

# To obtain the rows of the summary:
# standard deviation of the components
pc.tourists$sd
# proportion of variance explained by each PC
pc.tourists$sd^2/sum(pc.tourists$sd^2)
# cumulative proportion of explained variance
cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2)

# loadings (recall: coefficients of the linear combination of the original 
#           variables that defines each principal component)

load.tour <- pc.tourists$loadings
load.tour

load.tour[,1:8]

# graphical representation of the loadings of the first six principal components
x11()
par(mfcol = c(4,2))
for(i in 1:8) barplot(load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))

x11()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load.tour[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First PCs: weighted average of the number of nights in 3,4 stars hotel and residences
# Second PCs: contrast between the number of nights in 3 and 4 stars hotel
# Third PC: residences

# The loadings reflect the previous observation: the first 3 PCs are 
# driven by the variables displaying the highest variability

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.tourists, las=2, main='Principal components', ylim=c(0,4.5e7))
barplot(sapply(tourists,sd)^2, las=2, main='Original Variables', ylim=c(0,4.5e7), ylab='Variances')
plot(cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(tourists),labels=1:ncol(tourists),las=2)

# The first PC explains more than 98% of the total variability. 
# This is due to the masking effect of those 3 variables over the others

##### Principal component analysis of the dataset 'tourists',
##### but on the standardized variables

# We compute the standardized variables
tourists.sd <- scale(tourists)
tourists.sd <- data.frame(tourists.sd)

head(tourists.sd)

pc.tourists <- princomp(tourists.sd, scores=T)
pc.tourists
summary(pc.tourists)

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.tourists, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(tourists.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.tourists$sde^2)/sum(pc.tourists$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(tourists.sd),labels=1:ncol(tourists.sd),las=2)

# If we wanted to perform dimensionality reduction, we could keep
# 1 or 2 PCs

# loadings
load.tour <- pc.tourists$loadings
load.tour

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.tour[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

# Interpretation of the loadings:
# In this case, the first PC represents an average of the number of nights spent in 
# all the types of hotels and residences, taken with very similar weights.
# The sencond PC contrasts the more expensive solutions (4,5 stars hotels and residences)
# against the cheap solutions (1,2 stars hotels and B&B)

# High PC1: general high flow of tourists
# Low PC1: general low flow of tourists 
# High PC2: high flow for expensive solutions, low flow for cheap solutions
# Low PC2: low flow for expensive solutions, high flow for cheap solutions

# scores
scores.tourists <- pc.tourists$scores
scores.tourists

x11()
plot(scores.tourists[,1:2])
abline(h=0, v=0, lty=2, col='grey')

x11()
layout(matrix(c(1,2),2))
boxplot(tourists.sd, las=2, col='gold', main='Standardized variables')
scores.tourists <- data.frame(scores.tourists)
boxplot(scores.tourists, las=2, col='gold', main='Principal components')

x11()
biplot(pc.tourists)
# the arrows represent the loadings of the original variables on
# the 2 first PCs.

# Let's use the categorical variables to further interpret the results
head(tourists.label)

# Color according to Month
tourists.label[,1]
# We order the labels according to time order
tourists.label[,1] <- factor(tourists.label[,1], levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))
col.ramp <- rainbow(12)
col.lab1 <- rep(NA, n)
for(i in 1:n)
  col.lab1[i] = col.ramp[which(tourists.label[i,1] == levels(tourists.label[,1]))]

x11()
plot(scores.tourists[,1:2], col=col.lab1, pch=19, xlim=c(-8,25), ylim=c(-3,3.2))
abline(h=-3, v=-8, col=1)
points(scores.tourists[,1], rep(-3, n), col=col.lab1, pch=19)
points(rep(-8, n),scores.tourists[,2], col=col.lab1, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(tourists.label[,1]),fill=rainbow(12),bty='n')

# Months of Expo 2015: May to Oct
expo.label=factor(ifelse(tourists.label[,1] %in% c("May","Jun","Jul","Aug","Sept","Oct"), 'Expo', 'Non Expo'))
col.expo = ifelse(tourists.label[,1] %in% c("May","Jun","Jul","Aug","Sept","Oct"), 'red', 'blue')

x11(width=10, height = 10)
layout(cbind(c(2,4), c(1,3)), widths = c(1,4), heights = c(4, 1))
par(mar=rep(3,4))
plot(scores.tourists[,1:2], col=col.expo, pch=19, xlim=c(-8,20), ylim=c(-3,3.2), las=2)
abline(h=-3, v=-8, col=1)
points(scores.tourists[,1], rep(-3, n), col=col.expo, pch=19)
points(rep(-8, n),scores.tourists[,2], col=col.expo, pch=19)
abline(h=0, v=0, lty=2, col='grey')
boxplot(scores.tourists[,2]~expo.label, col=c('red', 'blue'), ylim=c(-3,3.2), las=2)
boxplot(scores.tourists[,1]~expo.label, col=c('red', 'blue'), ylim=c(-8,20), horizontal=T, las=2)

# Color according to Region of Origin
tourists.label[,2]
col.ramp <- rainbow(20)
col.lab2 <- rep(NA, n)
for(i in 1:n)
  col.lab2[i] = col.ramp[which(tourists.label[i,2] == levels(tourists.label[,2]))]

x11()
plot(scores.tourists[,1:2], col=col.lab2, pch=19, xlim=c(-8,30), ylim=c(-3,3.2))
abline(h=-3, v=-8, col=1)
points(scores.tourists[,1], rep(-3, n), col=col.lab2, pch=19)
points(rep(-8, n),scores.tourists[,2], col=col.lab2, pch=19)
abline(h=0, v=0, lty=2, col='grey')
legend('topright',levels(tourists.label[,2]),fill=rainbow(20),bty='n')


graphics.off()

##### Homework: try to perform the PCA on the log-transformed data
tourists.mod = tourists
for(i in 1:8) tourists.mod[which(tourists[,i]==0),i] = 1
tourists.log = log(tourists.mod)
head(tourists.log)

#_______________________________________________________________________________
##### Principal component analysis of the dataset 'food'

# upload the data
food <- read.table('Food.txt', header=T)
head(food)
dim(food)

n <- dim(food)[1]
p <- dim(food)[2]

# exploration
x11()
boxplot(food,col='gold')

x11()
boxplot(scale(x=food, center = T, scale=F), col='gold')

S <- cov(food)
round(S,digits = 2)
R <- cor(food)
round(R,digits = 2)

var.gen <- det(S)
var.gen
var.tot <- sum( diag(S) )
var.tot

##### Principal component analysis of the dataset 'food', 
##### based on the correlation matrix
food.sd <- scale(food)
food.sd <- data.frame(food.sd)

head(food.sd)
sapply(food.sd,mean)
sapply(food.sd,sd)
cov(food.sd)

# PC on correlation matrix
pc.food <- princomp(food.sd, scores=T)
pc.food
summary(pc.food)

# explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pc.food$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(food.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(pc.food$sdev^2)/sum(pc.food$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(food.sd),labels=1:ncol(food.sd),las=2)

# scores
scores.food <- pc.food$scores
scores.food

# variability of the original variables / scores
x11()
layout(matrix(c(1,2),2))
boxplot(food.sd, las=2, col='gold', main='Original variables')
scores.food <- data.frame(scores.food)
boxplot(scores.food, las=2, col='gold', main='Principal components')

# loadings
load.food    <- pc.food$loadings
load.food

x11()
par(mar = c(1,4,0,2), mfrow = c(4,1))
for(i in 1:4)barplot(load.food[,i], ylim = c(-1, 1))

# let's plot only the most significant loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(4,1))
for(i in 1:4) barplot(ifelse(abs(load.food[,i]) < 0.3, 0, load.food[,i]) , ylim = c(-1, 1));abline(h=0)


# Projection on the space generated by the k-th principal component
x11(width=21, height=7)
par(mfrow=c(2,5))
matplot(t(food.sd), type='l', main = 'Data', ylim=range(food.sd))

meanF <- colMeans(food.sd)
matplot(meanF, type='l', main = '0 PC', lwd=2, ylim=range(food.sd))
for(i in 1:8)
{
  projection <- matrix(meanF, dim(food.sd)[[1]], dim(food.sd)[[2]], byrow=T) + scores.food[,i] %*% t(load.food[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(food.sd))
  matplot(meanF, type='l', lwd=2, add=T)
}

# Projection on the space generated by the first k principal components
x11(width=21, height=7)
par(mfrow=c(2,5))
matplot(t(food.sd), type='l', main = 'Data', ylim=range(food.sd))
meanF <- colMeans(food.sd)
matplot(meanF, type='l', main = 'First 0 PCs', lwd=2, ylim=range(food.sd))
projection <- matrix(meanF, dim(food.sd)[[1]], dim(food.sd)[[2]], byrow=T)
for(i in 1:8)
{
  projection <- projection + scores.food[,i] %*% t(load.food[,i])
  matplot(t(projection), type='l', main = paste('First', i, 'PCs'), ylim=range(food.sd))
  matplot(meanF, type='l', lwd=2, add=T)
}

# Scores
x11()
par(mfrow=c(1,1))
plot(scores.food[,1],scores.food[,2],type="n",xlab="pc1",ylab="pc2", asp=1, xlim=c(-4,3))
text(scores.food[,1],scores.food[,2],dimnames(food)[[1]], cex=0.7)

x11()
biplot(pc.food)

x11()
stars(food.sd, draw.segments=T)

graphics.off()

#_______________________________________________________________________________
### p-dimensional geometrical interpretation of the principal components
library(rgl)

# theoretical mean and covariance matrix of the model
mu  <- c(0, 2, 3)
mu
sig <- rbind(c(9, 1, 1), c(1, 4, 1), c(1, 1, 1))
sig
nobs <- 100

X <- rmvnorm(nobs, mu, sig)

# sample mean and covariance matrix
M <- colMeans(X)
M
S <- cov(X)
S

open3d()                    # open a new device
points3d(X, asp=1, size=4)  # plot the points
axes3d()                    # add the axes
plot3d(ellipse3d(S, centre=M, level= 9/10), alpha=0.15, add = TRUE) # add the ellipsoid

# principal components
PC <- princomp(X)
summary(PC)

# "0" principal component: the best approximation of dimension 0 (a point)
open3d()
points3d(X, asp=1, size=4)
axes3d()

points3d(t(M), col='red', size=6)

for(i in 1:100)
  lines3d(rbind(X[i,], M))

# I principal component: the best approximation of dimension 1 (a line)
open3d()
points3d(X, asp=1, size=4)
axes3d()

PC1 <- NULL
for(i in 1:nobs) PC1 <- rbind(PC1, PC$loadings[,1]*PC$scores[i,1] + M)
points3d(PC1, col='red', size=6)

for(i in 1:nobs)lines3d(rbind(X[i,], PC1[i,]),col='blue')

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]), col='forestgreen',lwd=2)

# I and II principal components: the best approximation of dimension 2 (a plane)
open3d()
points3d(X, asp=1, size=4)
axes3d()

PC12 <- NULL
for(i in 1:nobs)PC12 <- rbind(PC12, PC$loadings[,1]*PC$scores[i,1] + PC$loadings[,2]*PC$scores[i,2] + M)
points3d(PC12, col='red', size=6)

for(i in 1:nobs)lines3d(rbind(X[i,], PC12[i,]),col='blue')

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]), col='forestgreen',lwd=2) 
lines3d(rbind(M + 2*PC$sdev[2] * PC$loadings[,2], M - 2*PC$sdev[2] * PC$loadings[,2]), col='forestgreen',lwd=2) 

# I, II and III principal components: the best approximation of dimension 3 (the entire space)

open3d()
points3d(X, asp=1, size=4)
axes3d()

PC123 <- NULL
for(i in 1:nobs)PC123 <- rbind(PC123, PC$loadings[,1]*PC$scores[i,1] + PC$loadings[,2]*PC$scores[i,2] + PC$loadings[,3]*PC$scores[i,3] + M)
points3d(PC123, col='red', size=6)

for(i in 1:nobs)lines3d(rbind(X[i,], PC123[i,]),col='blue')

lines3d(rbind(M + 2*PC$sdev[1] * PC$loadings[,1], M - 2*PC$sdev[1] * PC$loadings[,1]), col='forestgreen',lwd=2) 
lines3d(rbind(M + 2*PC$sdev[2] * PC$loadings[,2], M - 2*PC$sdev[2] * PC$loadings[,2]), col='forestgreen',lwd=2) 
lines3d(rbind(M + 2*PC$sdev[3] * PC$loadings[,3], M - 2*PC$sdev[3] * PC$loadings[,3]), col='forestgreen',lwd=2)

#_______________________________________________________________________________
##### Example: 
##### Question (c) of Problem 3 of the 29/06/2010 exam
## The file scotland.txt collects the number of residents in Scotland, according
## to the last census of 2001, divided by age and county. Assume the data 
## associated with different counties to be independent and identically distributed,
## and assume the data corresponding to different age ranges to be dependent.
## Perform a dimensionality reduction of the dataset through a principal component
## analysis and interpret the obtained components

age <- read.table('scotland.txt', header=T)
head(age)
dim(age)

x11()
pairs(age, pch=19)
# scatterplots of two consecutive age ranges are very correlated.
matplot(t(age), type='l', xlab='Age', ylab='Number of Residents', lty=1, col=rainbow(33), las=1)
# continuous lines representing the units. Useful in this context:
# each statistical unit could be approximated by a straight line.
# it could be enough to describe the unit with intersect and slope.
# we expect a dimension 2.

S <- cov(age)
image(S, asp=1)
# graphical representation through the function image. Be careful about
# the ordering of the matrix: first element is 0,0!

var.gen <- det(S)
var.tot <- sum( diag(S) )

# PCA (on the covariance matrix)
pc.age <- princomp(age, scores=T)
pc.age
summary(pc.age)
# we see that first two components account for the 99% of the variability
# of the dataset.

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(pc.age$sdev^2, las=2, main='Principal Components', ylab='Variances')
barplot(sapply(age,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(pc.age$sdev^2)/sum(pc.age$sde^2), type='b', axes=F, xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(age),labels=1:ncol(age),las=2)

# Scores
scores.age <- pc.age$scores
scores.age

layout(matrix(c(1,2),2))
boxplot(age, las=2, col='gold', main='Original variables')
scores.age <- data.frame(scores.age)
boxplot(scores.age, las=2, col='gold', main='Principal components')


# loadings are useful to interpret the PCs:
load.age    <- pc.age$loadings
load.age
# first principal component: all positive equal weights
# -> constant line, overall mean (size) of the county.
# second pc: inclined line -> contrast between young and old people
# (wrt the mean)

x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3)barplot(load.age[,i], ylim = c(-1, 1))

x11()
plot(scores.age[,1],scores.age[,2],type="n",xlab="pc1",ylab="pc2", asp=1)
text(scores.age[,1],scores.age[,2],dimnames(age)[[1]], cex=0.7)

biplot(pc.age)

# another possible visualization is the following:
# Projection on the space generated by the k-th principal component
x11(width=18, height=7)
par(mfrow=c(2,3))
#matplot(t(age), type='l', main = 'Data', ylim=range(age))
meanA <- colMeans(age)
matplot(meanA, type='l', main = '0 PC', lwd=2, ylim=range(age))
for(i in 1:5)
{
  projection <- matrix(meanA, dim(age)[[1]], dim(age)[[2]], byrow=T) + scores.age[,i] %*% t(load.age[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(age))
  matplot(meanA, type='l', lwd=2, add=T)
}

# Projection on the space generated by the first k principal components
x11(width=18, height=7)
par(mfrow=c(2,3))
#matplot(t(age), type='l', main = 'Data', ylim=range(age))
meanA <- colMeans(age)
matplot(meanA, type='l', main = '0 PC', lwd=2, ylim=range(age))
projection <- matrix(meanA, dim(age)[[1]], dim(age)[[2]], byrow=T)
for(i in 1:5)
{
  projection <- projection + scores.age[,i] %*% t(load.age[,i])
  matplot(t(projection), type='l', main = paste('First', i, 'PCs'), ylim=range(age))
  matplot(meanA, type='l', lwd=2, add=T)
}

#_______________________________________________________________________________
##### Additional execises: 

# Problem 1.
# Along the ringroads of Milan four control units measure the concentration of the 
# pollutant NO in the air.
# The measures collected during the last year are reported in the file NO.txt.
# Perform a principal component analysis of the available data. In particular:
# (a) Compute the loadings
# (b) Compute the variances along the PCs
# (c) Comment and interpret the results at points (a) and (b)
# (d) On the 3rd July the control unites registered the values (13, 10, 11, 13).
#     Compute the corresponding scores

#_______________________________________________________________________________
##### Additional material: Theoretical & sample PCA

# Let's generate a sample of size 100 from a Gaussian
set.seed(27032020)
mu  <-  c(1,2)
sig <-  cbind(c(1,1), c(1,4))
n   <-  100
X <- rmvnorm(n, mu, sig)

M <- colMeans(X)
S <- cov(X)

x11(width=14, height=7)
par(mfrow=c(1,3))
plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=1)
ellipse(M, S, 1, add=T,lwd=3, col = 'red')
ellipse(mu, sig, 1, add=T,lwd=3, col='springgreen')

abline(a = M[2] - eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1]*M[1], b = eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1], lty = 2, col = 'red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2]*M[1], b = eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2], lty = 2, col = 'red', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1]*mu[1], b = eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1], lty = 2, col = 'springgreen', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2]*mu[1], b = eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2], lty = 2, col = 'springgreen', lwd = 2)

legend('topleft',c('True','Estimated'),col=c('springgreen','red'),lty=c(1,1),lwd=2)

### let's now increase n...
n <- 1000

X <- rmvnorm(n, mu, sig)
M <- colMeans(X)
S <- cov(X)

plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=1)
ellipse(M, S, 1, add=T,lwd=3, col = 'red')
ellipse(mu, sig, 1, add=T,lwd=3, col='springgreen')

abline(a = M[2] - eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1]*M[1], b = eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1], lty = 2, col = 'red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2]*M[1], b = eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2], lty = 2, col = 'red', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1]*mu[1], b = eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1], lty = 2, col = 'springgreen', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2]*mu[1], b = eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2], lty = 2, col = 'springgreen', lwd = 2)

legend('topleft',c('True','Estimated'),col=c('springgreen','red'),lty=c(1,1),lwd=2)

###
n <- 5000

X <- rmvnorm(n, mu, sig)
M <- colMeans(X)
S <- cov(X)

plot(X, asp=1, xlab='Var 1', ylab='Var 2',pch=1)
ellipse(M, S, 1, add=T,lwd=3, col = 'red')
ellipse(mu, sig, 1, add=T,lwd=3, col='springgreen')

abline(a = M[2] - eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1]*M[1], b = eigen(S)$vectors[2,1]/eigen(S)$vectors[1,1], lty = 2, col = 'red', lwd = 2)
abline(a = M[2] - eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2]*M[1], b = eigen(S)$vectors[2,2]/eigen(S)$vectors[1,2], lty = 2, col = 'red', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1]*mu[1], b = eigen(sig)$vectors[2,1]/eigen(sig)$vectors[1,1], lty = 2, col = 'springgreen', lwd = 2)
abline(a = mu[2] - eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2]*mu[1], b = eigen(sig)$vectors[2,2]/eigen(sig)$vectors[1,2], lty = 2, col = 'springgreen', lwd = 2)

legend('topleft',c('True','Estimated'),col=c('springgreen','red'),lty=c(1,1),lwd=2)

