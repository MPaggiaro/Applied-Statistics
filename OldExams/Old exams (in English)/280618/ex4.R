#### EXAM: 28 Giugno 2018
#### PROBLEM 4
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
data <- read.table("Hotels.txt", header = T)
head(data)
attach(data)

# a) estimate model parameters:
n <- dim(data)[1]
isSeafront <- which(Position == "Seafront")
isSeafront
isWetseason <- which(Season == "WetSeason")
isWetseason

dSeafront <- dWetseason <- rep(0,n)
dSeafront[isSeafront] <- 1
dWetseason [isWetseason] <-1

sinusoid <- 1 + cos(4*pi/365*t)
fm <- lm(price ~ sinusoid + dSeafront + dWetseason + 
           sinusoid:dSeafront + sinusoid:dWetseason)
summary(fm)

# estimate of sigma^2
sum(fm$residuals^2)/fm$df

# graphical representation of the regression:
plot(t, price, pch = 20, col = "white")
points(t[which(dWetseason==1 & dSeafront==1)], price[which(dWetseason==1 & dSeafront==1)], col = "blue", pch = 20)
points(t[which(dWetseason==1 & dSeafront==0)], price[which(dWetseason==1 & dSeafront==0)], col = "blue")
points(t[which(dWetseason==0 & dSeafront==1)], price[which(dWetseason==0 & dSeafront==1)], col = "green", pch = 20)
points(t[which(dWetseason==0 & dSeafront==0)], price[which(dWetseason==0 & dSeafront==0)], col = "green")

tt = seq(t[1],t[n], 0.5)
sin_tt <- 1 + cos(4*pi/365*tt)
b <- coef(fm)
# wet season + seafront
lines(tt, b[1]+b[3]+b[4]+(b[2]+b[5]+b[6])*sin_tt, col = "blue")
# wet season no seafront
lines(tt, b[1]+b[4]+(b[2]+b[6])*sin_tt, col = "blue", lty = "dashed")
lines(tt, b[1]+b[3]+(b[2]+b[5])*sin_tt, col = "green")
# non wet season + non-seafront
lines(tt, b[1]+(b[2])*sin_tt, col = "green",lty = "dashed")
# how to represent a two-way linear regression???

# model assumptions:
par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))
# 9% p-value, not bad.

# b) 

# 1. dependence rates vs. seasonality.
A <- rbind(c(0,0,0,1,0,0), c(0,0,0,0,0,1))
b <- c(0,0)
linearHypothesis(fm, A, b)

# 2. rates vs position
A <- rbind(c(0,0,1,0,0,0), c(0,0,0,0,1,0))
b <- c(0,0)
linearHypothesis(fm, A, b)

# 3. temporal rates vs. season/position:
A <- rbind(c(0,0,0,0,1,0), c(0,0,0,0,0,1))
b <- c(0,0)
linearHypothesis(fm, A, b)
# sparecchio!

# c) model reduction:
fm <- lm(price ~ sinusoid + dSeafront + dWetseason)
summary(fm)

graphics.off()
# graphical representation of the regression:
plot(t, price, pch = 20, col = "white")
points(t[which(dWetseason==1 & dSeafront==1)], price[which(dWetseason==1 & dSeafront==1)], col = "blue", pch = 20)
points(t[which(dWetseason==1 & dSeafront==0)], price[which(dWetseason==1 & dSeafront==0)], col = "blue")
points(t[which(dWetseason==0 & dSeafront==1)], price[which(dWetseason==0 & dSeafront==1)], col = "green", pch = 20)
points(t[which(dWetseason==0 & dSeafront==0)], price[which(dWetseason==0 & dSeafront==0)], col = "green")

tt = seq(t[1],t[n], 0.5)
sin_tt <- 1 + cos(4*pi/365*tt)
b <- coef(fm)
# wet season + seafront
lines(tt, b[1]+b[3]+b[4]+b[2]*sin_tt, col = "blue")
# wet season no seafront
lines(tt, b[1]+b[4]+b[2]*sin_tt, col = "blue", lty = "dashed")
# dry season + seafront!
lines(tt, b[1]+b[3]+b[2]*sin_tt, col = "green")
# non wet season + non-seafront
lines(tt, b[1]+b[2]*sin_tt, col = "green",lty = "dashed")
# how to represent a two-way linear regression??? Like this!

par(mfrow=c(2,2))
plot(fm)

shapiro.test(residuals(fm))
# 9% p-value, not bad.

# d) Confidence interval of the maximum:

# maximum reached in ...
# sinusoid = 2, dry season, seafront!
# 
# mean rates -> CI on the expected value.

z0 <- data.frame(sinusoid = 2, dSeafront = 1, dWetseason = 0)
Conf <- predict(fm, z0, interval='confidence', level=1-0.01)  
Conf

# how can we plot the confidence interval??
