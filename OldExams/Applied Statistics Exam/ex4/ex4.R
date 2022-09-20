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

## Load spatial packages

library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

# data import:
revenues <- read.table("revenues.txt", header = T)
head(revenues)
attach(revenues)

coordinates(revenues) <- c('x','y')

# a)
# estimated variogram
var.est <- variogram(revenue ~ population, revenues)
plot(var.est)

# fitted variogram
v.fit <- fit.variogram(var.est, vgm(psill = 600, model = 'Gau', range = 1000))
v.fit
plot(var.est,v.fit)

g.model <- gstat(formula = revenue ~ population, data = revenues, model = v.fit)

# estimation of a0, a1:
s0 <- data.frame(x = c(514711,514711), y = c(5033903,5033903), population=c(0,1))

coordinates(s0) <- c('x', 'y')
prediction <- predict(g.model, s0, BLUE =T) 

# a0: first one
prediction$var1.pred[1]
# a1: second minus first
prediction$var1.pred[2]- prediction$var1.pred[1]

# deltas:


# b) Linear model:
fm = lm(population ~ distance)
summary(fm)

sB <- data.frame(x =514703.8, y = 5035569.3)
sD <- data.frame(x = 514711.6, y = 5033903)

sdiff <- sB - sD
sdiff
distB <- sqrt(sdiff$x^2+sdiff$y^2)
dB <- dist(c(sB,sD))
d0 <- data.frame(distance = distB)

pB <- predict(fm, d0)
pB


p0 <- data.frame(x = 514703.8,y = 5035569.3 ,population = pB)
coordinates(p0) <- c('x','y')
predict(g.model,p0, BLUE = F)

