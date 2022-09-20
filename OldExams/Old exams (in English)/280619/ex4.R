#### EXAM: 28 Giugno 2019
#### PROBLEM 2
####

# set wd
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

# data loading:
montserrat <- read.table("montserrat.txt", header=T)
head(montserrat)
coordinates(montserrat) <- c('x','y')
bubble(montserrat,'speed', do.log = TRUE, key.space='bottom') # ugly plot! Why?
plot(montserrat$distance,montserrat$speed)

# a) estimation of two variograms:
v2 <- variogram(speed ~ 1, data = montserrat)    
plot(v2)

v <- variogram(speed ~ distance, data = montserrat)
plot(v)

# convergent plot and small semivariance: we pick the second model!

# b) Fit a spherical model:
v.fit1 <- fit.variogram(v, vgm(8, "Sph", 20))   #sugg.isco di fittare su v un modello sferico con sill=1 e range=0.5
plot(v, v.fit1, pch = 3)
v.fit1

# c) GLS estimation of a:
g.tr1 <- gstat(formula = speed ~ distance, data = montserrat, model = v.fit1)
g.tr1   # Da qui leggo i parametri per il variogram epsilon

s0 = c(402476,4605558)
z0.new = data.frame(x=rep(s0[1],2),y=rep(s0[2],2),distance=c(0,1))
coordinates(z0.new) = c('x','y')
res <-predict(g.tr1, z0.new, BLUE = T)
a0 <-res$var1.pred[1]
a1 <-res$var1.pred[2]-a0
# d) Prediction:
pred <- predict(g.tr1, z0.new[1,])
pred
