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

# data loading:
data <- read.table("kimono.txt", header = T)
head(data)
attach(data)

# a) anova:
fit <- aov(value ~ city + type + city:type)
summary(fit)

# assumptions:
# 1) gaussianity of each group
shapiro.test(value[which(city==levels(city)[1] & type==levels(type)[1])])
shapiro.test(value[which(city==levels(city)[1] & type==levels(type)[2])])
shapiro.test(value[which(city==levels(city)[2] & type==levels(type)[1])])
shapiro.test(value[which(city==levels(city)[2] & type==levels(type)[2])])

# 2) equal variance:
bartlett.test(value, city:type)

# b) Model reduction:
fit <- aov(value ~ city + type)
summary(fit)

fit2 <- aov(value ~ type)
summary(fit2)

# c) Bonferroni intervals at 95%

alpha = 0.05
n <- dim(data)[1]
g=2
k <- g*(g-1)/2

Media   <- mean(value)
MediaGr  <- tapply(value, type, mean)
SSres <- sum(residuals(fit2)^2)
S <- SSres/(n-g)

gr.names=levels(type)
gr.num=as.vector(table(type))

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
