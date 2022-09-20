
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



# One of the most relevant consequences of the eruption of volcan 
# Eyjafjoll (in Iceland), in 2010, is the contamination by fluoride. 
# The latter is due to the deposit on the ground of the ash released
# in the atmosphere during the eruption.
# The file "fluoruro.txt" reports the coordinates of 50 measurement sites
# s_i, i=1,...,50, the corresponding concentrations of fluoride (ppm) F(s_i)
# and the distance D.s_i of each site s_i from the crater of the volcano.
# Denoting by delta a zero-mean, second-order stationary and isotropic random
# field:
# a) Estimate two empirical variograms, assuming the following models:
#    F(s_i)=beta_0+delta(s_i) and 
#    F(s_i)=beta_0+beta_1*D.s_i+delta(s_i). 
#    Choose the most appropriate model for the observations.
# b) Fit to the empirical variogram at point (a), a Gaussian model
#    without nugget, via weighted least squares. Use as initial parameters: 
#    sill=100, range=0.02. Report the estimates of sill and range.
# c) Fit to the empirical variogram chosen at point (a), a spherical model
#    without nugget, via weighted least squares. Report the estimates of sill 
#    and range.
# d) Compare the variograms estimated at points (b) and (c), with the empirical
#    variogram at point (a). Given that the ash deposition is known to be
#    a very regular phenomenon, which variogram model is the most appropriate?
# e) Based on model (d), estimate the concentration of fluoride due to the eruption
#    in the city of Raufarhofn (s0 = (0.3; 0.24), D.s0 = 0.1970) 
# f) Based on model (d), estimate the concentration of fluoride at the same
#    location, due to a possible new eruption of equivalent intensity, independent 
#    of that of 2010.

fluoruro <- read.table("fluoruro.txt", header = T)
head(fluoruro)
attach(fluoruro)
coordinates(fluoruro) <- c('X','Y')

# a) 
svgm1 <- variogram(Conc.ppm ~ 1, fluoruro)
plot(svgm1)

svgm2 <- variogram(Conc.ppm ~ D, fluoruro)
plot(svgm2)

# we choose 2!

# b)
v.fit2 <- fit.variogram(svgm2, vgm(psill = 100, model = 'Gau', range = 0.02))
plot(svgm2, v.fit2)

# estimates of sill, range:
v.fit2

# c)
v.fit.sphe <-fit.variogram(svgm2, vgm(psill = 100, model = 'Sph', range = 0.02))
v.fit.sphe
plot(svgm2, v.fit.sphe)

# d) 
# we choose the Gaussian because it is more regular.

# e) Estimation:
g.model <- gstat(formula = Conc.ppm ~ D, data = fluoruro, model = v.fit2)

s0 <- data.frame(X =0.3, Y=0.24, D = 0.1970)
s0
coordinates(s0) <- c('X','Y')
predict(g.model,s0, BLUE = T)

# f)
predict(g.model,s0)
detach(fluoruro)
