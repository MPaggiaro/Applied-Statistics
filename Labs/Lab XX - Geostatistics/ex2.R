
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


#___________________________________________________________*_____________________
#
# The file radioville.txt reports the information on 158 control units
# in the area around the nuclear power plant of Radioville.
# At each site, available data consist of: radioactivity levels [Bq],
# longitude [°N], latitude [°W] and type of soil [urban/vegetation].
# Denoting by s_i the i-th site, by R the radioactivity level,
# by eps a weakly stationary random field and by D a dummy 
# urban/vegetation:

# a) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram without nugget, estimated
#    via weighted least squares;
# b) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram with nugget, estimated
#    via weighted least squares;
# c) choose the best variogram model by comparing the fitted model 
#    with the corresponding empirical variograms (report qualitative
#    plots and the estimated variogram parameters)
# d) on the basis of model (c), predict the radioactivity level at the
#    parking lot of the shopping centre of Radioville (lon = 78.59, 
#    lat = 35.34), and in the park of Radioville (lon = 77.6, 
#    lat = 34.99);
# e) estimate variance of prediction error at the same locations
#    as at point d).

# a)
radioville <- read.table("radioville.txt",header = T)
head(radioville)
attach(radioville)
coordinates(radioville)<-c('Long', 'Lat')

v <- variogram(Bq ~ D, radioville)
plot(v)
v
v.fit <- fit.variogram(v, vgm(psill = 1, model = 'Sph', range = 0.6))
v.fit
plot(v,v.fit)

# estimation of parameters:
g.1 <- gstat(formula = Bq ~ D, data = radioville, model = v.fit)
predict(g.1, radioville[1,], BLUE = T)
predict(g.1, radioville[6,], BLUE = T)
# beta0[u]=3.98
# beta0[v]=7.93

predict(g.1, radioville[2,], BLUE = T)
# same value, cool!

# b) 
v.fit2 <- fit.variogram(v, vgm(psill = 0.9, model = 'Sph', range = 0.6, nugget = 0.1))
v.fit2
plot(v,v.fit2)

# estimation of parameters:
g.2 <- gstat(formula = Bq ~ D, data = radioville, model = v.fit2)
predict(g.2, radioville[1,], BLUE = T)
predict(g.2, radioville[6,], BLUE = T)
# beta0[u]=3.986
# beta0[v]=7.923

predict(g.2, radioville[10,], BLUE = T)

# c) Best model: the first one -> nugget equal to zero!

# d)
s0 <- data.frame(lon = c(78.59,77.6), lat = c(35.34,34.99), D=c('U','V'))
                 
coordinates(s0) <- c('lon', 'lat')
predict(g.1, s0, BLUE = F)

detach(radioville)
