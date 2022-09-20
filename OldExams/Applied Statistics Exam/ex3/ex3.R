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

# 
airfoil <- read.table("airfoil.txt", header = T)
head(airfoil)
attach(airfoil)

# a) 

fm <- lm(sound ~ velocity + frequency + frequency:velocity)
summary(fm)

# parameters estimate
coeffs = coef(fm)
coeffs[1]+coeffs[2]

coeffs[3]+ coeffs[4]

# diagnostics of residuals
par(mfrow=c(2,2))
plot(fm)

shapiro.test(fm$residuals)

# b)


# sound vs. frequency:
C <- rbind(c(0,0,1,0), c(0,0,0,1))
linearHypothesis(fm, C, c(0,0))
# Yes!

# sound vs. velocity:
C <- rbind(c(0,1,0,0), c(0,0,0,1))
linearHypothesis(fm, C, c(0,0))

# Yes!

# influence velocity on beta1:
# just look at p-value velocity:frequency

# c) 
rm <- lm(sound ~ velocity + frequency)
summary(rm)
c_rm <- coef(rm)
c_rm[1 ]+c_rm[2]
# d)

# prediction:
z0 <- data.frame(frequency= 15000, velocity = 'H')

predict(rm, z0, interval = "confidence", level = 0.01)


detach(airfoil)
