data <- read.csv("DATA_PlotFireClim_PostFireSamp_n1971.csv")
data$X <- NULL # In case there's weird X col added

## Spp in data ********* WHY THESE?? GET PICO?? ***********

# 106 Pinus edulis (2-needle)
# 122 Pinus ponderosa
# 133 Pinus monophylla (1-needle pinyon)
# 140 Pinus cembroides (Mex pinyon)
# 202 Pseudotsuga menziesii
# 803 Quercus arizonica
# 810 Quercus emoryi
# 814 Quercus gambelii
# 843 Quercus hypoleucoides
# 846 Quercus grisea
# 847 Quercus rugosa


## Convert all NAs in BA or TPA to zero
# Use caret ^ to specify start of string
BA.cols <- grep(pattern="^BA", x=colnames(data), value=TRUE)
TPA.cols <- grep(pattern="^TPA", x=colnames(data), value=TRUE)
data[BA.cols][is.na(data[BA.cols])] <- 0
data[TPA.cols][is.na(data[TPA.cols])] <- 0
data$DUFF_DEPTH[is.na(data$DUFF_DEPTH)] <- 0
data$LITTER_DEPTH[is.na(data$LITTER_DEPTH)] <- 0



## Create p/a regen var for maj spp
data <- data %>%
  mutate(PIEDregen = ifelse(data$TPASeed106Ac >0, 1, 0),
         PIPOregen = ifelse(data$TPASeed122Ac >0, 1, 0),
         PSMEregen = ifelse(data$TPASeed202Ac >0, 1, 0))
mean(data$PIEDregen) # 0.01826484
mean(data$PIPOregen) # 0.06950786
mean(data$PSMEregen) # 0.1212582



## Keep records where adult present
data.pied <- data %>%
  filter(BALive_106 >0 | BADeadStanding_106 >0 | BAMortStanding_106 >0 | BAMortDown_106 >0) %>%
  dplyr::rename(BALive_pied = BALive_106)
data.pipo <- data %>%
  filter(BALive_122 >0 | BADeadStanding_122 >0 | BAMortStanding_122 >0 | BAMortDown_122 >0) %>%
  dplyr::rename(BALive_pipo = BALive_122)  
data.psme <- data %>%
  filter(BALive_202 >0 | BADeadStanding_202 >0 | BAMortStanding_202 >0 | BAMortDown_202 >0) %>%
  dplyr::rename(BALive_psme = BALive_202)



## Check for outliers; BA sq ft/acre
max(data.pied$BALive_pied) # 124
max(data.pipo$BALive_pipo) # 458 seems high
hist(data.pipo$BALive_pipo)
data.pipo  <-data.pipo[data.pipo$BALive_pipo < 300,]
max(data.psme$BALive_psme) # 228


detach(data.pipo)
plot(BALive_pipo ~ FIRE.SEV)
cor.test(data.pipo$FIRE.SEV, data.pipo$BALive_pipo, method = "pearson")
data.clean$FIRE.


## is remaining live BA and fire severity correlated?
cor(data.pipo$fire.sev,data.pipo$BA.total.live,method="spearman",na.rm=T)
cor.test(data.pipo$fire.sev,data.pipo$BA.total.live,method="spearman")

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
