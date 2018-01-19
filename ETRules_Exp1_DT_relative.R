exp1<-read.csv("MLM_condition1.csv", header=TRUE, sep=",")
names(exp1)[1]<-"Subject"

library(nlme)
library(multilevel)
library(lattice)

#graph each kid's latency change over time
xyplot(singleRelProp~Time|as.factor(Subject), data=exp1, type=c("p","r","g"), col="blue",col.line="black",xlab="Time",ylab="singleton relative prop DT")

null.model<-lme(singleRelProp~1, random=~1|Subject,data=exp1,na.action=na.omit,control=list(opt="optim"))
VarCorr(null.model)
#ICC = 0.0003075848/(0.0003075848+0.0991760712) = .00309

#check if linear time is significant (it's not)
linear.time<-lme(singleRelProp~Time,random=~1|Subject,data=exp1,na.action=na.omit,control=list(opt="optim"))
summary(linear.time)$tTable

#check if quadratic time is significant (it's not)
quad.time<-lme(singleRelProp~poly(Time,2),random=~1|Subject,data=exp1,na.action=na.omit,control=list(opt="optim"))
summary(quad.time)$tTable

#check if cubic time is significant (it's not)
cube.time<-update(quad.time,singleRelProp~poly(Time,3))
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
tapply(exp1$singleRelProp,exp1$Time,var,na.rm=T)



