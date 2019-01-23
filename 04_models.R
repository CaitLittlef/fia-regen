data <- read.csv("DATA_PlotFireClim_PostFireSamp_n1971.csv")

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


## Create p/a regen var for maj spp
data <- data %>%
  mutate(PIEDregen = ifelse(data$TPASeed106Ac >0, 1, 0),
         PIPOregen = ifelse(data$TPASeed122Ac >0, 1, 0),
         PSMEregen = ifelse(data$TPASeed202Ac >0, 1, 0))
data$PIEDregen[is.na(data$PIEDregen)] <- 0
data$PIPOregen[is.na(data$PIPOregen)] <- 0
data$PSMEregen[is.na(data$PSMEregen)] <- 0

mean(data$PIEDregen) # 0.01826484
mean(data$PIPOregen) # 0.06950786
mean(data$PSMEregen) # 0.1212582

## Keep records where adult present
data.pied <- data %>%
  filter(BALive_106 >0 | BADeadStanding_106 >0 | BAMortStanding_106 >0 | BAMortDown_106 >0)
data.pipo <- data %>%
  filter(BALive_122 >0 | BADeadStanding_122 >0 | BAMortStanding_122 >0 | BAMortDown_122 >0)
data.psme <- data %>%
  filter(BALive_202 >0 | BADeadStanding_202 >0 | BAMortStanding_202 >0 | BAMortDown_202 >0)

####### START HERE AND CONVERT BA NAs TO ZERO ############
data$PIEDregen[is.na(data$PIEDregen)] <- 0
data$PIPOregen[is.na(data$PIPOregen)] <- 0
data$PSMEregen[is.na(data$PSMEregen)] <- 0


## subset data to where there is adult species present
#data.pipo<-subset(data.clim,data.clim$BALive_122 < 300 )
data.pipo<-subset(data.clim,data.clim$BALive_122 > 0 | data.clim$BADeadStanding_122 >0 | data.clim$BAMortStanding_122 >0 | data.clim$BAMortDown_122 >0 )
data.psme<-subset(data.clim,data.clim$BALive_202 > 0 | data.clim$BADeadStanding_202 >0 | data.clim$BAMortStanding_202 >0 | data.clim$BAMortDown_202 >0 )

dim(data.pipo)  #553 plots
dim(data.psme)  #724


### calculate time elapsed between fire and sampling
data.pipo$time.since.fire<-data.pipo$INVYR - data.pipo$Year
data.psme$time.since.fire<-data.psme$INVYR - data.psme$Year

## category for year of fire. Pre 2000 and post 2000
data.pipo$fireyr.cat<-ifelse(data.pipo$Year<2000,"early","late")

data.pipo$fireyr.cat<-factor(data.pipo$fireyr.cat)

## some fires are missing Year
data.pipo<-subset(data.pipo,data.pipo$diff.year<50)
dim(data.pipo)  #553 plots
hist(data.pipo$time.since.fire)

## convert NA values for PIPO basal area to 0
data.pipo$BA.pipo.live<-replace(data.pipo$BALive_122,is.na(data.pipo$BALive_122),0)
hist(data.pipo$BA.pipo.live) #### outlying value BA >500

data.psme$BA.psme.live<-replace(data.psme$BALive_202,is.na(data.psme$BALive_202),0)
hist(data.psme$BA.psme.live) #### outlying value BA >500

# drop outlying value
data.pipo<-data.pipo[data.pipo$BA.pipo.live<300,]

## convert NA values for total basal area to 0
data.pipo$BA.total.live<-replace(data.pipo$BALiveTot,is.na(data.pipo$BALiveTot),0)

## convert NA values for duff and litter to 0
data.pipo$duff<-replace(data.pipo$DUFF_DEPTH,is.na(data.pipo$DUFF_DEPTH),0)
data.pipo$litter<-replace(data.pipo$LITTER_DEPTH,is.na(data.pipo$LITTER_DEPTH),0)

########################################
### what is the fire severity?
###################################

# extract subset of total dataframe to work with
data.mtbs<-data.pipo[,218:246]
var.name<-names(data.mtbs)
yr.ind<-data.pipo$Year

#function to return MTBS fire severity for year of fire match
year.match<-function(x,data){
  yr<-grep(x,data)
  fluf<-data.mtbs[i,yr]
  return(fluf)
}

year.match(ty,var.name)  

#run function across loop
fire.sev<-numeric(length(yr.ind))

for (i in 1:length(fire.sev)){
  temp.year<-yr.ind[i]
  out<-year.match(temp.year,var.name)
  fire.sev[i]<-out
}

#drop category 5 and 6 and replace with NA
ind<-which(fire.sev==5 | fire.sev==6)
fire.sev<-replace(fire.sev,ind,values=NA)
data.pipo$fire.sev<-fire.sev


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
