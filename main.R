### Install the packages : qrmtools, plyr, evir :
library(qrmtools);  library(evir); library(plyr)

### Load data :
data <- read.csv("AV36_100.csv", header=T) ; dim(data)
N=dim(data)[1]; M=dim(data)[2]; N; M


### Preprocess data :
inp <- data[,c(7,11,12,13,25)]
outp <- anonymise(inp,3)

newdata <- cbind(data[,c(1:6)],outp[,1],data[,c(8:10)], outp[,c(2,3,4)],data[,c(14:24)],outp[,5],data[,c(26:36)])[,-c(3,35)]
dim(newdata)


### Calulation the prob of re-identification of data in case of h=2-uplet:
h=2
x <- prob_re(newdata,h)$prob; length(x)


### Calulation the indicator of danger
v1=0.99; v2=0.999; alpha=0.05
id.danger.logit(x,v1,v2,alpha)


### Plot the figures
par(oma=c(0,0,2,0))
par(mfrow=c(2,2))
Plot_id(x,v1,v2,alpha)
title(main=paste("AV36 : N =",N,"observations, M =",M,"attributes, h =",h,"keys"),col=4,outer=T)
