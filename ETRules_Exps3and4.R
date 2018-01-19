ageCompare<-read.csv("MLM_condition3and4.csv", header=TRUE, sep=",")
names(ageCompare)[1]<-"Subject"

library(nlme)
library(multilevel)
library(lattice)

null.model<-lme(singletonLatency~1, random=~1|Subject,data=ageCompare,na.action=na.omit,control=list(opt="optim"))
VarCorr(null.model)
#ICC = 282681.4/(282681.4+12485834.2) = .0221

#check if linear time is significant (it's not)
linear.time<-lme(singletonLatency~TimeCent+Condition,random=~1|Subject,data=ageCompare,na.action=na.omit,control=list(opt="optim"))
summary(linear.time)$tTable

#check if quadratic time is significant (it's not)
quad.time<-lme(singletonLatency~poly(TimeCent,2)+Condition,random=~1|Subject,data=ageCompare,na.action=na.omit,control=list(opt="optim"))
summary(quad.time)$tTable

#check if cubic time is significant (it's not)
cube.time<-update(quad.time,singletonLatency~poly(TimeCent,3)+Condition)
summary(cube.time)$tTable

#change random effects in linear time model (ie let time vary randomly; does not improve model fit)
linear.time2<-update(linear.time,random=~TimeCent|Subject)
anova(linear.time,linear.time2)

#check for autocorrelation (there isn't)
linear.time.auto<-update(linear.time,correlation=corAR1())
summary(linear.time.auto)
#check to see if accounting for autocorrelation improves fit (it doesn't)
anova(linear.time,linear.time.auto)

#check to see if variance is changing over time (it's not)
tapply(ageCompare$singletonLatency,ageCompare$TimeCent,var,na.rm=T)



