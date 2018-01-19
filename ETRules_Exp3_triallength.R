exp3<-read.csv("MLM_condition3.csv", header=TRUE, sep=",")
names(exp3)[1]<-"Subject"

library(nlme)
library(multilevel)
library(lattice)

#graph each kid's latency change over time
xyplot(trialLength~Time|as.factor(Subject), data=exp3, type=c("p","r","g"), col="blue",col.line="black",xlab="Time",ylab="singleton prop DT")

null.model<-lme(trialLength~1, random=~1|Subject,data=exp3,na.action=na.omit,control=list(opt="optim"))
VarCorr(null.model)
#ICC = 12385262/(12385262+63089987) = .1641

#check if linear time is significant (it's not)
linear.time<-lme(trialLength~Time,random=~1|Subject,data=exp3,na.action=na.omit,control=list(opt="optim"))
summary(linear.time)$tTable

#check if quadratic time is significant (it's not)
quad.time<-lme(trialLength~poly(Time,2),random=~1|Subject,data=exp3,na.action=na.omit,control=list(opt="optim"))
summary(quad.time)$tTable

#check if cubic time is significant (it's not)
cube.time<-update(quad.time,trialLength~poly(Time,3))
summary(cube.time)$tTable

#change random effects in linear time model (ie let time vary randomly; does improve model fit)
linear.time2<-update(linear.time,random=~Time|Subject)
anova(linear.time,linear.time2)
summary(linear.time2)$tTable #time still isn't significant predictor

#check for autocorrelation (there isn't)
linear.time.auto<-update(linear.time,correlation=corAR1())
summary(linear.time.auto)
#check to see if accounting for autocorrelation improves fit (it does, but predictor still not significant)
anova(linear.time,linear.time.auto)

#check to see if variance is changing over time (it's not)
tapply(exp3$trialLength,exp3$Time,var,na.rm=T)



