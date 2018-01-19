exp1<-read.csv("C://Users/cschonberg/Dropbox/Research/ET Rules (Scott 2017 GSR)/MLM Analyses/MLM_condition1.csv", header=TRUE, sep=",")
names(exp1)[1]<-"Subject"

library(nlme)
library(multilevel)
library(lattice)

#graph each kid's latency change over time
#xyplot(singletonLatency~Time|as.factor(Subject), data=exp1, type=c("p","r","g"), col="blue",col.line="black",xlab="Time",ylab="singleton Latency")

null.model<-lme(singletonLatency~1, random=~1|Subject,data=exp1,na.action=na.omit,control=list(opt="optim"))
VarCorr(null.model)
#ICC = 20875.35/(20875.35+656336.89) = .0308

#check if linear time is significant
linear.time<-lme(singletonLatency~Time,random=~1|Subject,data=exp1,na.action=na.omit,control=list(opt="optim"))
summary(linear.time)$tTable

#check if quadratic time is significant (it's not)
quad.time<-lme(singletonLatency~poly(Time,2),random=~1|Subject,data=exp1,na.action=na.omit,control=list(opt="optim"))
summary(quad.time)$tTable

#check if cubic time is significant (it's not)
cube.time<-update(quad.time,singletonLatency~poly(Time,3))
summary(cube.time)$tTable

#change random effects in linear time model (ie let time vary randomly; does not improve model fit)
linear.time2<-update(linear.time,random=~Time|Subject)
anova(linear.time,linear.time2)

#check for autocorrelation (there isn't)
linear.time.auto<-update(linear.time,correlation=corAR1())
summary(linear.time.auto)
#check to see if accounting for autocorrelation improves fit (it doesn't)
anova(linear.time,linear.time.auto)

#check to see if variance is changing over time (it's not)
tapply(exp1$singletonLatency,exp1$Time,var,na.rm=T)



