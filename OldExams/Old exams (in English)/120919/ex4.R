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

# data exploration:
mickey <- read.table("mickey.txt", header = T)
head(mickey)
attach(mickey)
plot(day, waiting.time)
# a) lm

sinusoidal <- 1 + cos(4*pi*day/365)
fm <- lm(waiting.time ~ day.of.the.week + sinusoidal + sinusoidal:day.of.the.week)
summary(fm)

par(mfrow=c(2,2))
plot(fm)

shapiro.test(fm$residuals)
# b) 
C = rbind(c(0,1,0,0),
          c(0,0,0,1))
linearHypothesis(fm,C,c(0,0))

# c)
fm <- lm(waiting.time ~ day.of.the.week + sinusoidal)
summary(fm)

# d)

 
linearHypothesis(fm, c(1,1,2), 60)
# true!
# or
z0 <- data.frame(sinusoidal=2,day.of.the.week="weekend")
help("predict")
pred.z0 <- predict(fm, z0, interval = 'confidence', level = 0.95) 
pred.z0

# e) 
t0 <- 238
sin_t0 <- 1 + cos(4*pi*t0/365)
z0 <- data.frame(sinusoidal=sin_t0,day.of.the.week="weekdays")
help("predict")
pred.z0 <- predict(fm, z0, interval = 'prediction', level = 0.95) 
pred.z0

