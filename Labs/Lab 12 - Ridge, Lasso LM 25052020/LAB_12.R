###---------------------###
### LAB 12 (25/05/2020) ###
###---------------------###

### TOPICS:
### Linear models

library(MASS)
library(car)
library(rgl)

library(glmnet)

setwd("D:/RTDA/Didattica/Applied Statistics MATE 19-20/Lab 12 - 25052020")

#_______________________________________________________________________________
##### Problem of collinearity
#####--------------------------

### Example 1: Multiple linear regression
###----------------------------------------
### Dataset cars: distance taken to stop [ft] as a function of velocity [mph]
### for some cars in the 1920s

cars

x11()
plot(cars, xlab='Speed', ylab='Stopping distance', las=1)

n          <- dim(cars)[[1]]
distance   <- cars$dist
speed1     <- cars$speed
speed2     <- cars$speed^2

### Model:
### distance = beta_0 + beta_1 * speed + beta_2 * speed^2 + Eps
### (linear in the parameters!)

fm <- lm(distance ~ speed1 + speed2)
summary(fm) 
# Note: collinearity

# Variance inflation factor
help(vif)

vif(fm)

# recall vif formula:
1/(1- summary(lm(speed1 ~ speed2))$r.squared )
1/(1- summary(lm(speed2 ~ speed1))$r.squared )

### A possible solution to collinearity: PCA
speed.pc <- princomp(cbind(speed1,speed2), scores=TRUE)
summary(speed.pc)
speed.pc$load

sp1.pc <- speed.pc$scores[,1]
sp2.pc <- speed.pc$scores[,2]

# Now we estimate the model by inserting the PCs instead of the 
# original regressors 
# Model: y = b0 + b1*PC1+ b2*PC2 + eps, eps~N(0,sigma^2)
fm.pc <- lm(distance ~ sp1.pc + sp2.pc)

summary(fm.pc) 

# Note: we could have performed dimensionality reduction before
# estimating the model and then considered only the first PC.

# We can re-write the model as:
# Model: 
# y= b0 + b1*      PC1                 + b2*      PC2                 + eps =
#  = b0 + b1*(e11*(X1-m1)+e21*(X2-m2)) + b2*(e12*(X1-m1)+e22*(X2-m2)) + eps =
#  = b0 - b1*e11*m1 - b2*e12*m1 - b1*e21*m2 - b2*e22*m2 + 
#                           + (b1*e11+b2*e12)*X1 + (b1*e21+b2*e22)*X2 + eps
# where e.ij are the loadings, i=1,2, j=1,2.
# => We can compute the coefficients of the model which used the original 
#    regressors
m1 <- mean(speed1)
m2 <- mean(speed2)
beta0 <- coefficients(fm.pc)[1] - 
  coefficients(fm.pc)[2]*speed.pc$load[1,1]*m1 - 
  coefficients(fm.pc)[3]*speed.pc$load[1,2]*m1 - 
  coefficients(fm.pc)[2]*speed.pc$load[2,1]*m2 - 
  coefficients(fm.pc)[3]*speed.pc$load[2,2]*m2
beta1 <- coefficients(fm.pc)[2]*speed.pc$load[1,1] + 
  coefficients(fm.pc)[3]*speed.pc$load[1,2] 
beta2 <- coefficients(fm.pc)[2]*speed.pc$load[2,1] + 
  coefficients(fm.pc)[3]*speed.pc$load[2,2] 

c(beta0=as.numeric(beta0),beta1=as.numeric(beta1),beta2=as.numeric(beta2))
fm$coefficients

x11()
x <- seq(0, 25, len=100)
plot(cars, xlab='Speed', ylab='Stopping distance', las=1)
lines(x, beta0+beta1*x+beta2*x^2)

# Reduce the model:
fm.pc <- lm(distance ~ sp1.pc)
summary(fm.pc) 

# We can re-write the model as:
# Model: y = b0 + b1*      PC1                 + eps =
#          = b0 + b1*(e11*(X-m1)1+e21*(X2-m2)) + eps =
#          = b0 - b1*e11*m1 - b2*e21*m2 + b1*e11*X1 + b1*e21*X2 + eps
beta0 <- coefficients(fm.pc)[1] - 
  coefficients(fm.pc)[2]*speed.pc$load[1,1]*m1 - 
  coefficients(fm.pc)[2]*speed.pc$load[2,1]*m2 
beta1 <- coefficients(fm.pc)[2]*speed.pc$load[1,1]
beta2 <- coefficients(fm.pc)[2]*speed.pc$load[2,1]

c(beta0=as.numeric(beta0),beta1=as.numeric(beta1),beta2=as.numeric(beta2))
fm$coefficients

x11()
plot(sp1.pc,distance, xlab='PC1', ylab='Stopping distance', las=1, xlim=c(-250,361), ylim=c(-5,130))
x <- seq(-250,361,by=1)
b <- coef(fm.pc)
lines(x, b[1]+b[2]*x)

x11()
plot(speed1,distance, ylab='Stopping distance', las=1, ylim=c(-5,130))
x <- seq(0,25,by=1)
lines(x, beta0 + beta1*x + beta2*x^2)

# diagnostics of the residuals
par(mfrow=c(2,2))
plot(fm.pc)

shapiro.test(residuals(fm.pc))

dev.off()

# The first PC is basically speed2 (centered with respect to the mean),
# hence we could just consider the regressor speed2

fm.2 <- lm(distance ~ speed2)
summary(fm.2) 
# Note: when using as regressors polynomials of the same variable (i.e., in
# the framework of polynomial regression), some authors recommend instead to
# keep all the orders lower than the maximum that one wants to keep (e.g., if
# keeping the maximum order 3, they recommend keeping also all the terms
# of order 2 and 1, for a "hierarchy" principle)

x11()
plot(speed2, distance, xlab='speed2', ylab='Stopping distance', las=1, xlim=c(15,626), ylim=c(-5,130))
x <- seq(0,650,by=1)
b <- coef(fm.2)
lines(x, b[1]+b[2]*x)

x11()
plot(cars, xlab='Speed', ylab='Stopping distance', las=1, xlim=c(0,30), ylim=c(-5,130))
x <- seq(0,30,by=0.1)
b <- coef(fm.2)
lines(x, b[1]+b[2]*x^2)

dev.off()
graphics.off()
### Otherwise, another possible solution to collinearity: ridge regression
help(lm.ridge)

# Fix lambda
lambda <- .5
fit.ridge <- lm.ridge(distance ~ speed1 + speed2, lambda = lambda)
# Note: to fit the model, R automatically centers X and Y 
# with respect to their mean.

coef.ridge <- coef(fit.ridge)
yhat.lm <- cbind(rep(1,n), speed1, speed2)%*%coef(fm)  # LM fitted values
yhat.r <- cbind(rep(1,n), speed1, speed2)%*%coef.ridge # ridge fitted values

x11()
plot(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',xlab='Speed')
points(speed1, distance, pch=1, cex=.8)
matlines(speed1, yhat.r, type='l', lty=1,col=grey.colors(length(lambda)), lwd=2)
legend("topleft",c("lm","ridge"),lty=c(4,1),col=c("black",grey.colors(length(lambda))),lwd=2)


# Repeat for a grid of lambda's
lambda.c <- seq(0,10,0.01)
fit.ridge <- lm.ridge(distance ~ speed1 + speed2, lambda = lambda.c)

x11(width=14, height=5)
par(mfrow=c(1,3))
plot(lambda.c,coef(fit.ridge)[,1], type='l', xlab=expression(lambda),
     ylab=expression(beta[0]))
abline(h=coef(fm)[1], lty=2)
plot(lambda.c,coef(fit.ridge)[,2], type='l', xlab=expression(lambda),
     ylab=expression(beta[1]))
abline(h=coef(fm)[2], lty=2)
plot(lambda.c,coef(fit.ridge)[,3], type='l', xlab=expression(lambda),
     ylab=expression(beta[2]))
abline(h=coef(fm)[3], lty=2)

dev.off()

yhat.lm <- cbind(rep(1,n), speed1, speed2)%*%coef(fm)

x11()
plot(speed1, yhat.lm, type='l', lty=1, lwd=2, ylab='Distance',
     xlab='Speed')
points(speed1, distance, pch=1, cex=.8)
yhat.r <- NULL
for(i in 1:length(lambda.c))
  yhat.r=cbind(yhat.r, cbind(rep(1,n), speed1, speed2)%*%coef(fit.ridge)[i,])
matlines(speed1, yhat.r, type='l', lty=1,
         col=grey.colors(length(lambda.c)))
lines(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',
      xlab='Speed')

# Choice of the optimal lambda, e.g., via cross-validation
select(fit.ridge)

# or
lambda.opt <- lambda.c[which.min(fit.ridge$GCV)]
lambda.opt

x11(width=14, height=5)
par(mfrow=c(1,3))
plot(lambda.c,coef(fit.ridge)[,1], type='l', xlab=expression(lambda),
     ylab=expression(beta[0]))
abline(h=coef(fm)[1], lty=1, col='grey')
abline(v=lambda.opt, col=2, lty=2)
plot(lambda.c,coef(fit.ridge)[,2], type='l', xlab=expression(lambda),
     ylab=expression(beta[1]))
abline(h=coef(fm)[2], lty=1, col='grey')
abline(v=lambda.opt, col=2, lty=2)
plot(lambda.c,coef(fit.ridge)[,3], type='l', xlab=expression(lambda),
     ylab=expression(beta[2]))
abline(h=coef(fm)[3], lty=1, col='grey')
abline(v=lambda.opt, col=2, lty=2)

dev.off()

x11()
plot(speed1, distance, pch=1, cex=.8, ylab='Distance',
     xlab='Speed')
matlines(speed1, yhat.r, type='l', lty=1,
         col=grey.colors(length(lambda.c)))
lines(speed1, yhat.lm, type='l', lty=4, lwd=2, ylab='Distance',
      xlab='Speed')
lines(speed1, yhat.r[,which.min(fit.ridge$GCV)], type='l', lty=1, lwd=2,
      col=2, ylab='Distance', xlab='Speed')
legend("topleft", c('LM', 'Ridge opt.' ), lty=c(4,1), col=c(1,2), lwd=2)

coef.ridge <- coef(fit.ridge)[which.min(fit.ridge$GCV),]
coef.ridge

### Otherwise, another possible solution to collinearity: lasso regression
help(glmnet)

# Build the matrix of predictors
x <- model.matrix(distance~speed1+speed2)[,-1]
# Build the vector of response
y <- distance

# Let's set a grid of candidate lambda's for the estimate
lambda.grid <- 10^seq(5,-3,length=100)
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso 
# [note: if alpha=0 -> ridge regression]

x11()
plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

# Let's set lambda via cross validation
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

x11()
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:3,]
coef.lasso 


#______________________________________________________________________________________________________________________________________________________________
##### Multiple linear regression
#####--------------------------------------------------------

data <- read.table('concrete.txt', header=T)

head(data)
dim(data)
names(data)

pairs(data)

attach(data)

X1 <- Alluminium    
X2 <- Silicate
X3 <- Alluminium_ferrite
X4 <- Silicate_bicalcium
Y <- Hardness_concrete

detach(data)

x11()
par(mfrow=c(2,2))
plot(X1, Y, main='Hardness vs Alluminium', lwd=2,
     xlab='Alluminium', ylab='Hardness concrete')
plot(X2, Y, main='Hardness vs Silicate', lwd=2,
     xlab='Silicate', ylab='Hardness concrete')
plot(X3, Y, main='Hardness vs Alluminium ferrite', lwd=2,
     xlab='Alluminium ferrite', ylab='Hardness concrete')
plot(X4, Y, main='Hardness vs Silicate bicalcium', lwd=2,
     xlab='Silicate bicalcium', ylab='Hardness concrete')

## Multiple linear regression

result <- lm(Y ~ X1 + X2 + X3 + X4)
summary(result)

vif(result)

#### PCA regression
result.pc <- princomp(cbind(X1,X2,X3,X4), scores=TRUE)
summary(result.pc)
result.pc$load

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(result.pc$sdev^2, las=2, main='Principal Components', ylab='Variances')
barplot(c(sd(X1),sd(X2),sd(X3),sd(X4))^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(result.pc$sdev^2)/sum(result.pc$sde^2), type='b', axes=F, xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.9, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:4,labels=1:4,las=2)

# Loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(4,1))
for(i in 1:4)barplot(result.pc$load[,i], ylim = c(-1, 1))

graphics.off()

# Dimensionality reduction: select first two PCs:
pc1 <- result.pc$scores[,1]
pc2 <- result.pc$scores[,2]

# Now we estimate the model using the first two PCs as regressors.
# Model: y = b0 + b1*PC1+ b2*PC2 + eps, eps~N(0,sigma^2)
fm.pc <- lm(Y ~ pc1 + pc2)

summary(fm.pc) 

# We can re-write the model as:
# Model:
# y= b0 + b1*PC1 + b2*PC2 + eps =
#  = b0 + b1*(e11*(X1-m1)+e21*(X2-m2)+e31*(X3-m3)+e41*(X4-m4)) +
#       + b2*(e12*(X1-m1)+e22*(X2-m2)+e32*(X3-m3)+e42*(X4-m4)) + eps =
#  = b0 - b1*e11*m1 - b2*e12*m1 - b1*e21*m2 - b2*e22*m2 +
#       - b1*e31*m3 - b2*e32*m3 - b1*e41*m4 - b2*e42*m4 +
#       + (b1*e11+b2*e12)*X1 + (b1*e21+b2*e22)*X2 +
#       + (b1*e31+b2*e32)*X3 + (b1*e41+b2*e42)*X4 + eps
# where e.ij are the loadings, i=1,2,3,4, j=1,2.
# => We can compute the coefficients of the model which used the original
#    regressors
m1 <- mean(X1)
m2 <- mean(X2)
m3 <- mean(X3)
m4 <- mean(X4)
beta0 <- coefficients(fm.pc)[1] -
  coefficients(fm.pc)[2]*result.pc$load[1,1]*m1 -
  coefficients(fm.pc)[3]*result.pc$load[1,2]*m1 -
  coefficients(fm.pc)[2]*result.pc$load[2,1]*m2 -
  coefficients(fm.pc)[3]*result.pc$load[2,2]*m2 -
  coefficients(fm.pc)[2]*result.pc$load[3,1]*m3 -
  coefficients(fm.pc)[3]*result.pc$load[3,2]*m3 - 
  coefficients(fm.pc)[2]*result.pc$load[4,1]*m4 -
  coefficients(fm.pc)[3]*result.pc$load[4,2]*m4
beta1 <- coefficients(fm.pc)[2]*result.pc$load[1,1] +
  coefficients(fm.pc)[3]*result.pc$load[1,2]
beta2 <- coefficients(fm.pc)[2]*result.pc$load[2,1] +
  coefficients(fm.pc)[3]*result.pc$load[2,2]
beta3 <- coefficients(fm.pc)[2]*result.pc$load[3,1] +
  coefficients(fm.pc)[3]*result.pc$load[3,2]
beta4 <- coefficients(fm.pc)[2]*result.pc$load[4,1] +
  coefficients(fm.pc)[3]*result.pc$load[4,2]

c(beta0=as.numeric(beta0),beta1=as.numeric(beta1),beta2=as.numeric(beta2),beta3=as.numeric(beta3),beta4=as.numeric(beta4))
result$coefficients

# diagnostics of the residuals
x11()
par(mfrow=c(2,2))
plot(fm.pc)

shapiro.test(residuals(fm.pc))

dev.off()

### Ridge and Lasso regression with glmnet

x <- model.matrix(Y ~ X1 + X2 + X3 + X4)[,-1] # matrix of predictors
y <- Y # vector of response
lambda.grid <- 10^seq(5,-3,length=50)

# Ridge regression
fit.ridge <- glmnet(x,y, lambda = lambda.grid, alpha=0) # alpha=0 -> ridge 

x11()
plot(fit.ridge,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

norm_l2 <- NULL
for(i in 1:50)
  norm_l2 <- c(norm_l2,sum((fit.ridge$beta[,i])^2))

plot(log(lambda.grid),norm_l2)

# Let's set lambda via CV
set.seed(1)
cv.ridge <- cv.glmnet(x,y,alpha=0,nfolds=3,lambda=lambda.grid)

bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge

plot(cv.ridge)
abline(v=log(bestlam.ridge), lty=1)

# Get the coefficients for the optimal lambda
coef.ridge <- predict(fit.ridge, s=bestlam.ridge, type = 'coefficients')[1:5,]
coef.ridge 

plot(fit.ridge,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
abline(v=log(bestlam.ridge))


### Lasso regression

fit.lasso <- glmnet(x,y, lambda = lambda.grid, alpha=1) # alpha=1 -> lasso 

plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=1)

norm_l1 <- NULL
for(i in 1:50)
  norm_l1 <- c(norm_l1,sum(abs(fit.ridge$beta[,i])))

plot(log(lambda.grid),norm_l1)

# Let's set lambda via CV
set.seed(1)
cv.lasso <- cv.glmnet(x,y,alpha=1,nfolds=3,lambda=lambda.grid)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)

# Get the coefficients for the optimal lambda
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')[1:5,]
coef.lasso 

plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
abline(v=log(bestlam.lasso))


# Compare coefficients estimates for LS, Ridge and Lasso
plot(rep(0, dim(x)[2]), coef(lm(y~x))[-1], col=rainbow(dim(x)[2]), pch=20, xlim=c(-1,3), ylim=c(-1,2), xlab='', ylab=expression(beta),
     axes=F)
points(rep(1, dim(x)[2]), coef.ridge[-1], col=rainbow(dim(x)[2]), pch=20)
points(rep(2, dim(x)[2]), coef.lasso[-1], col=rainbow(dim(x)[2]), pch=20)
abline(h=0, col='grey41', lty=1)
box()
axis(2)
axis(1, at=c(0,1,2), labels = c('LS', 'Ridge', 'Lasso'))
legend('topright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), pch=20, cex=1)

# l2 norm
sum((coef(lm(y~x))[-1])^2) # LS
sum((coef.ridge[-1])^2)    # ridge

# l1 norm
sum(abs(coef(lm(y~x))[-1])) # LS
sum(abs(coef.lasso[-1]))    # lasso


### Variable selection

result <- lm(Y ~ X1 + X2 + X3 + X4)
summary(result)

result1 <- lm(Y ~ X1 + X3 + X4)
summary(result1)

result2 <- lm(Y ~ X1 + X4)
summary(result2)


# diagnostics of the residuals

par(mfrow=c(2,2))
plot(result2)

shapiro.test(residuals(result2))


#_______________________________________________________________________________
##### Hitters dataset
#####-------------

library(ISLR)

help(Hitters)
names(Hitters)
dim(Hitters)

# remove NA's
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

###  Subset Selection Methods
library(leaps)

help(regsubsets)

# Best Subset Selection (exhaustive search)
regfit.full <- regsubsets(Salary~., data=Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
summary(regfit.full)

reg.summary <- summary(regfit.full)
names(reg.summary)

reg.summary$which

reg.summary$rsq   # r-squared
reg.summary$adjr2 # adjusted r-squared
reg.summary$rss   # residual sum of squares

x11(height=7,width=14)
par(mfrow=c(1,3))
plot(reg.summary$rsq,xlab="Number of Variables",ylab="R-squared",type="b")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b")
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="b")

# extract coefficient estimates associated with the models

which.max(reg.summary$adjr2)
coef(regfit.full,11)

coef(regfit.full,6)

# graphical table of best subsets
help(plot.regsubsets)

x11()
plot(regfit.full,scale="r2",main="Exhaustive search")

x11()
plot(regfit.full,scale="adjr2",main="Exhaustive search")


# Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

x11(height=7,width=14)
par(mfrow=c(1,3))
plot(summary(regfit.fwd)$rsq,xlab="Number of Variables",ylab="R-squared",type="b")
plot(summary(regfit.fwd)$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b")
plot(summary(regfit.fwd)$rss,xlab="Number of Variables",ylab="RSS",type="b")

x11()
plot(regfit.fwd,scale="r2",main="Forward Stepwise Selection")

x11()
plot(regfit.fwd,scale="adjr2",main="Forward Stepwise Selection")



regfit.bwd <- regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

x11(height=7,width=14)
par(mfrow=c(1,3))
plot(summary(regfit.bwd)$rsq,xlab="Number of Variables",ylab="R-squared",type="b")
plot(summary(regfit.bwd)$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b")
plot(summary(regfit.bwd)$rss,xlab="Number of Variables",ylab="RSS",type="b")

x11()
plot(regfit.bwd,scale="r2",main="Backward Stepwise Selection")

x11()
plot(regfit.bwd,scale="adjr2",main="Backward Stepwise Selection")


coef(regfit.full,7) # Exhaustive search
coef(regfit.fwd,7)  # Forward Stepwise Selection
coef(regfit.bwd,7)  # Backward Stepwise Selection

graphics.off()

### Choosing among models using the k-fold cross-validation approach
### (exhaustive search)

k <- 10

set.seed(1)
folds <- sample(1:k,nrow(Hitters),replace=TRUE)
folds
table(folds)

# function that performs the prediction for regsubsets
predict.regsubsets <- function(object,newdata,id){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object,id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

cv.errors <- matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit <- regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred <- predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i] <- mean( (Hitters$Salary[folds==j]-pred)^2 )
  }
}
cv.errors
root.mean.cv.errors <- sqrt(apply(cv.errors,2,mean)) # average over the columns
root.mean.cv.errors

plot(root.mean.cv.errors,type='b')

which.min(root.mean.cv.errors)
points(which.min(root.mean.cv.errors),root.mean.cv.errors[which.min(root.mean.cv.errors)], col='red',pch=19)

# estimation on the full dataset
reg.best <- regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,10)


### k-fold cross validation after model selection (  WRONG WAY!!  )

best.fit <- regsubsets(Salary~.,data=Hitters,nvmax=19)
summary(best.fit)

cv.errors_wrong <- matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k)
  for(i in 1:19){
    covariate <- which(summary(best.fit)$which[i,-c(1,19)])
    mod <- lm(Salary~.,data=Hitters[folds!=j,c(covariate,19)])
    pred <- predict(mod,Hitters)[folds==j]
    cv.errors_wrong[j,i] <- mean( (Hitters$Salary[folds==j]-pred)^2)
  }

cv.errors_wrong
root.mean.cv.errors_wrong <- sqrt(apply(cv.errors_wrong,2,mean)) # average over the columns
root.mean.cv.errors_wrong

plot(root.mean.cv.errors_wrong,type='b')

which.min(root.mean.cv.errors_wrong)
points(which.min(root.mean.cv.errors_wrong),root.mean.cv.errors_wrong[which.min(root.mean.cv.errors_wrong)], col='red',pch=19)

points(root.mean.cv.errors,type='b',col='blue')


### Ridge and Lasso regression with glmnet

x <- model.matrix(Salary~.,Hitters)[,-1] # predictor matrix
y <- Hitters$Salary # response
grid <- 10^seq(10,-2,length=100) # grid of lambda

# Ridge regression

ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)

plot(ridge.mod,xvar='lambda',label=TRUE)

# choosing the parameter lambda
set.seed(123)
cv.out <- cv.glmnet(x,y,alpha=0,nfold=3,lambda=grid) 

plot(cv.out)

bestlam.ridge <- cv.out$lambda.min
bestlam.ridge
log(bestlam.ridge)

abline(v=log(bestlam.ridge))

plot(ridge.mod,xvar='lambda',label=TRUE)
abline(v=log(bestlam.ridge))

# Lasso regression

lasso.mod <- glmnet(x,y,alpha=1,lambda=grid)

plot(lasso.mod,xvar='lambda',label=TRUE)

# choosing the parameter lambda
set.seed(123)
cv.out <- cv.glmnet(x,y,alpha=1,nfold=3,lambda=grid) 

plot(cv.out)

bestlam.lasso <- cv.out$lambda.min
bestlam.lasso
log(bestlam.lasso)

abline(v=log(bestlam.lasso))

plot(lasso.mod,xvar='lambda',label=TRUE)
abline(v=log(bestlam.lasso))


# Compare coefficients estimates for LS, Ridge and Lasso

coef.ridge <- predict(ridge.mod, s=bestlam.ridge, type = 'coefficients')[1:20,]
coef.lasso <- predict(lasso.mod, s=bestlam.lasso, type = 'coefficients')[1:20,]
coef.ridge
coef.lasso

plot(rep(0, dim(x)[2]), coef(lm(y~x))[-1], col=rainbow(dim(x)[2]), pch=20, xlim=c(-1,3), ylim=c(-1,2), xlab='', ylab=expression(beta),
     axes=F)
points(rep(1, dim(x)[2]), coef.ridge[-1], col=rainbow(dim(x)[2]), pch=20)
points(rep(2, dim(x)[2]), coef.lasso[-1], col=rainbow(dim(x)[2]), pch=20)
abline(h=0, col='grey41', lty=1)
box()
axis(2)
axis(1, at=c(0,1,2), labels = c('LS', 'Ridge', 'Lasso'))

# l2 norm
sum((coef(lm(y~x))[-1])^2) # LS
sum((coef.ridge[-1])^2)    # ridge

# l1 norm
sum(abs(coef(lm(y~x))[-1])) # LS
sum(abs(coef.lasso[-1]))    # lasso


##################################################################################
## Logistic Regression ##
help(glm)

##  Pb2 of 23/07/2009  ##
# A store of an appliance chain sells two types of televisions: with 4:3 
# screen and with 16:9 screen. The TV.txt file shows the number of 
# televisions sold annually for both types of televisions from 1999 
# to 2008. By introducing an appropriate logistic model and estimating 
# the parameters with the maximum likelihood method:
# a) comment the residual deviance by comparing it with that of the model
#    without regressor.
# Assuming that the store is representative of the Italian situation:
# b) provide a pointwise estimate for the proportion of 16:9 televisions 
#    sold in Italy in 2009;
# c) provide a pointwise estimate for the year in which sales of 16:9 
#    televisions exceeded those of 4:3;
# d) provide a pointwise estimate for the year in which 16:9 televisions
#    will cover (or have covered) 99% of the Italian market.

TV <- read.table('TV.txt', header=T)
head(TV)
dim(TV)

#a)
fit <- glm(Tipo ~ Anno, data=TV, family='binomial')
summary(fit)

plot(TV$Anno, as.numeric(TV$Tipo)-1)
lines(seq(1997, 2010, by=0.1), 
      predict(fit, data.frame(Anno=seq(1997,2010,by=0.1)), type='response'))

# null model
Freq.tot <- table(TV$Tipo)[2]/ (table(TV$Tipo)[1] + table(TV$Tipo)[2])
abline(h = Freq.tot, col='blue', lty=2)

#b)
predict(fit, data.frame(Anno=2009), type='response')

#c) 
(log(0.5/0.5) - coefficients(fit)[1]) / coefficients(fit)[2]
abline(h=0.5, col='red')

#d)
(log(0.99/0.01) - coefficients(fit)[1]) / coefficients(fit)[2]
abline(h=0.99, col='red')




#_______________________________________________________________________________
##### Classification and regression trees
#####--------------------------

### Classification Trees: Carseats dataset

attach(Carseats)

help(Carseats)
dim(Carseats)
names(Carseats)

hist(Sales)
abline(v=8,lwd=3,col='red')

High <- ifelse(Sales<=8,"No","Yes")

Carseats <- data.frame(Carseats,High)

library(tree)
help(tree)

tree.carseats <- tree(High~.-Sales,Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty=0)

tree.carseats

# use cross validation to prune the tree optimally
help(cv.tree)

set.seed(1)
cv.carseats <- cv.tree(tree.carseats,FUN=prune.misclass)

names(cv.carseats)
cv.carseats

plot(cv.carseats$size,cv.carseats$dev,type="b",xlab="size",ylab="misclass")

help(prune.misclass)
prune.carseats <- prune.misclass(tree.carseats,best=12)

plot(prune.carseats)
text(prune.carseats,pretty=0)

detach(Carseats)



### Regression Trees: Boston housing dataset

help(Boston)
dim(Boston)
names(Boston)

tree.boston <- tree(medv~.,Boston)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston <- cv.tree(tree.boston)

plot(cv.boston$size,cv.boston$dev,type='b',xlab='size',ylab='deviance')

prune.boston <- prune.tree(tree.boston,best=4)

plot(prune.boston)
text(prune.boston,pretty=0)

