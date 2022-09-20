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
library(glmnet)

# data reading:
piadeina <- read.table("piadeina.txt", header=T)
head(piadeina)
plot(piadeina)
attach(piadeina)
levels(Day.of.Week)

# a) linear regression model:
n <- dim(piadeina)[1]
# - building the categorical variables day of week:
# initialization:
dTue <- dWed <- dThu <- dFri <- rep(0,n)

# fulfillment:
dTue[which(Day.of.Week == "Tue")] <- 1
dWed[which(Day.of.Week == "Wed")] <- 1
dThu[which(Day.of.Week == "Thu")] <- 1
dFri[which(Day.of.Week == "Fri")] <- 1

fm <- lm (Sales ~ dTue + dWed + dThu + dFri + Bread.Sold +
            Wraps.Sold + Sandwich.Sold + Focaccia.Sold +
            Piadina.Sold + Chips.Sold + Juices.Sold +
            Total.Soda.and.Coffee.Sold + Max.Daily.Temperature)
summary(fm)

# assumptions:
par(mfrow= c(2,2))
plot(fm)

shapiro.test(residuals(fm))
# we have two outliers, 2 and 8.

# b) Lasso method:
x <- model.matrix(Sales ~ dTue + dWed + dThu + dFri + Bread.Sold +
                    Wraps.Sold + Sandwich.Sold + Focaccia.Sold +
                    Piadina.Sold + Chips.Sold + Juices.Sold +
                    Total.Soda.and.Coffee.Sold + Max.Daily.Temperature)[,-1]
# Build the vector of response
y <- Sales

fit.lasso <- glmnet(x,y, lambda = 5) # default: alpha=1 -> lasso 
# [note: if alpha=0 -> ridge regression]
coef(fit.lasso)

# c)
# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- seq(0,100,by = 1)
lambda.grid
fit.lasso <- glmnet(x,y, lambda = lambda.grid) 

# cross - validation:
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[,]
coef.lasso 
