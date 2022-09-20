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

# data visualization:
data <- read.table("garden.txt", header = T)
head(data)
plot(data)

# a) model:
attach(data)
fm <- lm(extension ~ carps + maple + cherry + stones)
summary(fm)

# estimate of betas in summary:
sigma <- summary(fm)$sigma

# assumptions:
par(mfrow=c(2,2))
plot(fm)
shapiro.test(residuals(fm))
# gaussian!

# b)

# - dependence extension : maple / cherries
C <- rbind(c(0,0,1,0,0), c(0,0,0,1,0))
linearHypothesis(fm, C, c(0,0))
# Yes!!

# - dependence extension : stones / carps:
C <- rbind(c(0,1,0,0,0), c(0,0,0,0,1))
linearHypothesis(fm, C, c(0,0))
# Yes!!

# c) Model reduction: it's enough to take maples and stones, we
# have collinearity!
frm <- lm(extension ~ carps + maple)
summary(frm)
