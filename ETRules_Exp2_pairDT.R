exp2<-read.csv("MLM_condition2.csv", header=TRUE, sep=",")
names(exp2)[1]<-"Subject"

library(nlme)
library(multilevel)
library(lattice)

#graph each kid's latency change over time
xyplot(pairprop~TimeCent|as.factor(Subject), data=exp2, type=c("p","r","g"), col="blue",col.line="black",xlab="Time",ylab="Pair DT")

null.model<-lme(pairprop~1, random=~1|Subject,data=exp2,na.action=na.omit,control=list(opt="optim"))
VarCorr(null.model)
#ICC = 0.005035673/(0.005035673+0.021910982) = .1868

#check if linear time is significant (it is)
linear.time<-lme(pairprop~TimeCent,random=~1|Subject,data=exp2,na.action=na.omit,control=list(opt="optim"))
summary(linear.time)$tTable

#check if quadratic time is significant (it's not)
quad.time<-lme(pairprop~poly(TimeCent,2),random=~1|Subject,data=exp2,na.action=na.omit,control=list(opt="optim"))
summary(quad.time)$tTable

#check if cubic time is significant (it's not)
cube.time<-update(quad.time,pairprop~poly(TimeCent,3))
summary(cube.time)$tTable

#change random effects in linear time model (ie let time slope vary randomly; does not improve model fit)
linear.time2<-update(linear.time,random=~TimeCent|Subject)
anova(linear.time,linear.time2)

#check for autocorrelation (there might be? phi = .05)
linear.time.auto<-update(linear.time,correlation=corAR1())
summary(linear.time.auto)
#check to see if accounting for autocorrelation improves fit (it doesn't)
anova(linear.time,linear.time.auto)

#check to see if variance is changing over time (it's not)
tapply(exp2$pairprop,exp2$TimeCent,var,na.rm=T)



