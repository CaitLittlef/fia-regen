
#####################################################################
##### model species specific regen where there was a burn and plot was visited after
############################################################################

install.packages("party")
library(party)

install.packages("vcd")
library(vcd)

## transform predictors
#data.pipo$time.since.trans<-data.pipo$diff.year^0.25
#data.pipo$trans.live.BA <- data.pipo$BALive_122^0.25
data.pipo$regen<-as.factor(data.pipo$PIPOregen)
#data.pipo$precip<-data.pipo$MAP_1995^0.5
data.pipo$BA.pipo.live.tran<-data.pipo$BA.pipo.live^0.5
hist(data.pipo$BA.pipo.live)
hist(data.pipo$BA.pipo.live.tran)

data.pipo$time.since.fire.tran<-data.pipo$time.since.fire^0.3
hist(data.pipo$time.since.fire.tran)
##### fit MOB model

tree.mob<-mob(regen~BA.pipo.live.tran+resid.yr|time.since.fire+BA.total.live+CMD_1995+MAP_1995+fire.sev+Year+duff+litter,data=data.pipo,model=glinearModel,family= binomial(link=logit),control = mob_control(minsplit = 30))
plot(tree.mob,type="extended")

summary(tree.mob)
print(tree.mob)
## calculate deviance explained

tr_null.pipo<- glm(regen ~ 1, data = data.pipo, family=binomial(link=logit)) # deviance for null model
tr_glm.pipo<-glm(regen ~ BA.pipo.live.tran, data = data.pipo, family=binomial(link=logit)) ## deviance for glm model
(deviance(tr_null.pipo)-deviance(tree.mob))/deviance(tr_null.pipo)   ## 0.12 for tree based model with BIC pruning
(deviance(tr_null.pipo)-deviance(tr_glm.pipo))/deviance(tr_null.pipo) ## 0.02



### examine relationship between fire year and time since fire
rbPal <- colorRampPalette(c('grey','brown'))

#This adds a column of color values
# based on the y values

## convert NA values for total basal area to 0
data.pipo$TPASeed122Ac<-replace(data.pipo$TPASeed122Ac,is.na(data.pipo$TPASeed122Ac),0)


plot(jitter(data.pipo$time.since.fire,2),jitter(data.pipo$Year,2),xlab="time since fire",ylab="Year of Fire")
#abline(h=2000,col="blue")

install.packages("mgcv")
library(mgcv)
gam.time<-gam(Year~s(time.since.fire,k=3),data=data.pipo)
summary(gam.time)
#plot(gam.time,resid=T)

newd<-data.frame(time.since.fire=1:30)
y<-predict(gam.time,newdata=newd)
lines(newd$time.since.fire,y,col="blue",lwd=2)
data.pipo$resid.yr<-resid(gam.time)
hist(data.pipo$resid.yr)

gam.test<-gam(regen~s(BA.pipo.live,resid.yr,k=5)+s(time.since.fire,k=4),data=data.pipo,family=binomial(link=logit))
summary(gam.test)
plot(gam.test)

## plot interactions
par(mfrow=c(2,1))
vis.gam(gam.test,view=c("BA.pipo.live","resid.yr",type="response"),theta=240,phi=30)
vis.gam(gam.test,view=c("time.since.fire","resid.yr",type="response"),theta=40,phi=30)
vis.gam(gam.test,view=c("BA.pipo.live","time.since.fire",type="response"),theta=140,phi=30)

glm.test<-glm(regen ~ BA.pipo.live.tran+time.since.fire+resid.yr, data = data.pipo, family=binomial(link=logit)) ## 
summary(glm.test)
data.pipo$pred.r<-predict(gam.test,type="response")

## plot with projected recruit probability
data.pipo$col <- rbPal(10)[as.numeric(cut(data.pipo$pred.r,breaks = 10))]

plot(jitter(data.pipo$time.since.fire,5),jitter(data.pipo$Year,5),col=data.pipo$col,xlab="time since fire",ylab="Year of Fire")
#abline(h=2000,col="blue")