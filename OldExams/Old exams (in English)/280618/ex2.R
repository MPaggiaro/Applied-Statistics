#### EXAM: 28 Giugno 2018
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
data <- read.table("mexican.txt", header = T)
head(data)
plot(data)
n <- dim(data)[1]

attach(data)

# a) complete ANOVA model (two-way)
complete.fit=aov(price ~ type.food + area + type.food:area)
summary.aov(complete.fit)

# assumptions:
# 1. Gaussianity in each group:# Build all the groups
g1=which(type.food=="Fajitas" & area=="Cancun")
g2=which(type.food=="Fajitas" & area=="Guanajato")
g3=which(type.food=="Fajitas" & area=="MexicoCity")
g4=which(type.food=="Tacos" & area=="Cancun")
g5=which(type.food=="Tacos" & area=="Guanajato")
g6=which(type.food=="Tacos" & area=="MexicoCity")

p1 = shapiro.test(price[g1])$p
p2 = shapiro.test(price[g2])$p
p3 = shapiro.test(price[g3])$p
p4 = shapiro.test(price[g4])$p
p5 = shapiro.test(price[g5])$p
p6 = shapiro.test(price[g6])$p

Ps = c(p1,p2,p3,p4,p5,p6)
Ps
# 2. Same variance.
bartlett.test(price, type.food:area)

# b) model reduction:

# first, remove the interaction food - area:
fit.additive = aov(price ~ type.food + area)
summary(fit.additive)

# second, remove the type of food:
reduced.fit = aov(price ~ area)
summary(reduced.fit)

# c) Bonferroni intervals:

# we have three groups, the areas.
### We use Bonferroni   {Global level 1-alpha}
alpha = 0.01
g=3
k <- g*(g-1)/2

Media   <- mean(price)
MediaGr  <- tapply(price, area, mean)
SSres <- sum(residuals(reduced.fit)^2)
S <- SSres/(n-g)

gr.names=levels(area)
gr.num=as.vector(table(area))

## Confidence interval for all differences  -- Prints also
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(gr.names[i],"-",gr.names[j]))        
    print(as.numeric(c(MediaGr[i]-MediaGr[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/gr.num[i] + 1/gr.num[j] )),
                       MediaGr[i]-MediaGr[j],
                       MediaGr[i]-MediaGr[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/gr.num[i] + 1/gr.num[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(MediaGr[i]-MediaGr[j] - qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/gr.num[i] + 1/gr.num[j] )),
                                       MediaGr[i]-MediaGr[j] + qt(1-alpha/(2*k), n-g) * sqrt( S * ( 1/gr.num[i] + 1/gr.num[j] )))))
  }
}
