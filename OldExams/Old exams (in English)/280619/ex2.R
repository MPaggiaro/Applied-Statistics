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

# data exploration:
profiling <- read.table("profiling.txt", header=T)
head(profiling)
plot(profiling)

# a better explanatory plot is the following:
attach(profiling)
levels(type)
plot(t1, t2, col = type)

# Division into subgroups
group1 <- profiling[which(profiling$type=="resident"),1:2]
group2 <- profiling[which(profiling$type=="tourist"),1:2]
plot(group1$t1, group1$t2)  # ress
plot(group2$t1, group2$t2)  # tours

# a) Build a classifier for variable "type": we use QDA.
# Verify assumptions: Gaussianity in each class
mcshapiro.test(group1)      #pvalue=0.11
mcshapiro.test(group2)     #pvalue=0.70
# ---> The classes are Gaussian

mean1 = sapply(group1, mean)
mean2 = sapply(group2, mean)

S1=var(group1)
S2=var(group2)
S1
S2
# clearly not similar variance matrices

# Actually perform the QDA
qda.s <- qda(profiling[,1:2], profiling[,3])
qda.s

# Report the mean within the groups
qda.s$means

# Report the prior probabilities estimated from the sample 
qda.s$prior

# Qualitative plot of the regions of classification
x11()
plot(profiling[,1:2], main='QDA plot', xlab='Year', ylab='AspectRatio', pch=20, col = profiling[,3])
legend('topleft', legend=c("Residents", "Tourists"), fill=c(1,2), cex=.7)
# Add centers
points(qda.s$means, pch=4,col=c(1,2,3) , lwd=2, cex=3)
# Define a new grid and plot
x  <- seq(min(profiling[,1]), max(profiling[,1]), length=200)
y  <- seq(min(profiling[,2]), max(profiling[,2]), length=200)
xy <- expand.grid(t1=x,t2=y)    #Correct for the name of the vars
z  <- predict(qda.s, xy)$post  
z1 <- z[,1] - z[,2]
z2 <- z[,2] - z[,1]
#z.q  <- predict(qda.s, xy)$post  
#z1.q <- z.q[,1] - z.q[,2] 
#z2.q <- z.q[,2] - z.q[,1]  
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
#contour(x, y, matrix(z1.q, 200), levels=0, drawlabels=F, add=T, lty=2, col="forestgreen")  
#contour(x, y, matrix(z2.q, 200), levels=0, drawlabels=F, add=T, lty=2, col="forestgreen")

####################
# TOP : Alternativa
plot(xy, col=predict(qda.s,xy)$class)
####################

# c) APER estimate:
predicted_type <- predict(qda.s, profiling[,1:2])
qda.s$prior

tab = table(classe.vera=profiling$type, classe.allocata=predicted_type$class)
tab

errors = (predicted_type$class != profiling$type)
APER = sum(errors)/length(profiling$type)
(10+50)/(256+751)

# d) prediction of a single datum
new.datum = c("t1"=35, "t2"=3)
new.datum

predict(qda.s, newdata = new.datum)
