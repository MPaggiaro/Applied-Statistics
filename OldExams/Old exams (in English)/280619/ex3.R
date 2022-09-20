#### EXAM: 28 Giugno 2019
#### PROBLEM 2
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

# Data exploration:
airport <- read.table("airport.txt", header=T)
head(airport)
plot(airport)
attach(airport)
plot(distance, duration, col=time.of.the.day)
legend('topleft', legend=c("6-10", "11-15","16-20"), fill=c(1,2,3), cex=.7)

# we need to convert categorical data of groups into binary:
n <-dim(airport)[1]
n

# building of dummy variables:
dS = rep(0,n)
dP = dS

dP[which(airport$time.of.the.day=="11-15")] <- 1
dS[which(airport$time.of.the.day=="16-20")] <- 1
dP
dS

# a) estimate parameters of the model, verify the assumptions:
# Perform the LM
# Model:
# duration = B0[g] + B1[g]*x + eps
# with B0[g]=beta0       if the unit is morning
#      B0[g]=beta0+beta2 if the unit is afternoon
#      B0[g]=beta0+beta3 if the unit is evening
#      B1[g]=beta1       if the unit is m
#      B1[g]=beta1+beta4 if the unit is af
#      B1[g]=beta1+beta5 if the unit is ev
fit <- lm(duration ~ distance+dP+dS+distance:dP+distance:dS, data=airport)
summary(fit)
shapiro.test(residuals(fit))
par(mfrow=c(2,2))
plot(fit)
graphics.off()
# they seem gaussian!

# estimate of sigma^2:
est_sigma = sum(fit$residuals^2)/fit$df
est_sigma
5# plot of the regression:
coeffs <-fit$coefficients
g = 3
B0 = rep(0,g)
B1 = B0

B0[1]=coeffs[1]
B0[2]=coeffs[1]+coeffs[3]
B0[3]=coeffs[1]+coeffs[4]

B1[1]=coeffs[2]
B1[2]=coeffs[2]+coeffs[5]
B1[3]=coeffs[2]+coeffs[6]

# graphical representation:
plot(distance, duration, col=time.of.the.day, xlim = c(40,70))
legend('topleft', legend=c("11-15","16-20","6,10"), fill=c(1,2,3), cex=.7)
x <- seq(40,70, by = 0.1)
lines(x, B0[1] + B1[1]*x)
lines(x, B0[2] + B1[2]*x)
lines(x, B0[3] + B1[3]*x)

# b) statistical tests:

# - dependence mean duration/time of the day:
# we test if beta2 = beta3 = beta4 = beta5 = 0
linearHypothesis(fit,
                 rbind(c(0,0,1,0,0,0),
                       c(0,0,0,1,0,0),
                       c(0,0,0,0,1,0),
                       c(0,0,0,0,0,1)),
                 c(0,0,0,0))
# time of the day influences travel duration.

# - dependence mean duration/distance:
# we test beta1 = beta4 = beta5 = 0
linearHypothesis(fit,
                 rbind(c(0,1,0,0,0,0),
                       c(0,0,0,0,1,0),
                       c(0,0,0,0,0,1)),
                 c(0,0,0))
# distance influences duration.

# c) model reduction:
# - effect of G on the intercept:
linearHypothesis(fit,
                 rbind(c(0,0,1,0,0,0),
                       c(0,0,0,1,0,0)),
                 c(0,0))
# no effect -> we can remove:
fit <- lm(duration ~ distance+distance:dP+distance:dS, data=airport)
summary(fit)
# representation:
B0 = fit$coefficients[1]
B1 = fit$coefficients[2]+c(0,fit$coefficients[3:4])
plot(distance, duration, col=time.of.the.day, xlim = c(40,70))
legend('topleft', legend=c("11-15","16-20","6-10"), fill=c(1,2,3), cex=.7)
x <- seq(40,70, by = 0.1)
lines(x, B0 + B1[1]*x)
lines(x, B0 + B1[2]*x)
lines(x, B0 + B1[3]*x)


# we can try to do a last trick:
dPS = dP+dS
fit <- lm(duration ~ distance+distance:dPS, data=airport)
summary(fit)
# yes! it works maaaannnn!
# representation:
B0 = fit$coefficients[1]
B1 = fit$coefficients[2]+c(0,fit$coefficients[3])
plot(distance, duration, col=time.of.the.day, xlim = c(40,70))
legend('topleft', legend=c("11-15","16-20","6-10"), fill=c(1,2,3), cex=.7)
x <- seq(40,70, by = 0.1)
lines(x, B0 + B1[1]*x)
lines(x, B0 + B1[2]*x)

# d) prediction:
# better to rewrite data in a easier form:
dati <- data.frame(duration, distance, dPS)
fit <- lm(duration ~ distance+distance:dPS, data=dati)
summary(fit)
Z0 <- data.frame(distance = 57, dPS = 0)
# prediction:
IP <- predict(fit, newdata=Z0, interval='prediction', level=1-0.01)
IP

#9.30 - 1.27 -> 8.03!
# I take the bus of 8:00.