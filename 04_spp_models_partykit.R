data.pied <- read.csv("data.pied.csv") ; data.pied$X <- NULL
data.pipo <- read.csv("data.pipo.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme.csv") ; data.psme$X <- NULL

currentDate <- Sys.Date()

## Accurately characterize variables bc...
# glmtree treats categorical and num differently.
data.pied$regen_pied <- factor(data.pied$regen_pied, ordered = FALSE)
data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.pied$FIRE.SEV <- factor(data.pied$FIRE.SEV, ordered = TRUE)
data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)


## Consider transformations
hist(data.pipo$YEAR.DIFF)
hist(data.pipo$YEAR.DIFF^0.5)
hist(data.pipo$BALive_pipo)
hist(data.pipo$BALive_pipo^0.5) 
data.pipo$BALive_pipo_trans <- data.pipo$BALive_pipo^0.5
data.psme$BALive_psme_trans <- data.psme$BALive_psme^0.5 


colnames(data.pipo)

################################################
## PIPO
tree.mob.pipo <- glmtree(regen_pipo ~ YEAR.DIFF
                | BALive_pipo + BALiveTot # using BA trans doesn't chng
                # + def59_z_03 #+ def68_z_03 # on, MAP excluded; off, MAP included - why?
                + def59_z_0 #+ def68_z_0
                + def59_z_1 #+ def68_z_1
                + def59_z_2 #+ def68_z_2
                # + def59_z_3 #+ def68_z_3 # on, MAP excluded; off, MAP included - why?
                + CMD_1995 + MAP_1995
                # + I(YEAR.DIFF) # on, MAP excluded; off, MAP included - why?
                + REBURN # on, MAP excluded; off, MAP included - why?
                + FIRE.SEV,
                data = data.pipo,
                family = binomial(link = "logit"),
                minsplit = 50)
## Output
tree.mob.pipo
tiff(paste0(out.dir,"pipo_BA-MAP-def_",currentDate,".tiff"),
     width = 640, height = 480, units = "px")
plot(tree.mob.pipo)
dev.off()

## Calculate deviance explained
glm.null.pipo <- glm(regen_pipo ~ 1,
                    data = data.pipo,
                    family=binomial(link=logit)) # deviance for null model
glm.full.pipo <- glm(regen_pipo ~ YEAR.DIFF,
                   data = data.pipo,
                   family=binomial(link=logit)) ## deviance for glm model
# Tree model:
(deviance(glm.null.pipo) - deviance(tree.mob.pipo))/deviance(glm.null.pipo)
# Non-tree model:
(deviance(glm.null.pipo) - deviance(glm.full.pipo))/deviance(glm.null.pipo)


## Reburn
# data.pipo.reburn <- data.pipo %>% filter(REBURN == "Y")
# tree.mob.pipo.reburn <- glmtree(regen_pipo ~ YEAR.DIFF
#                                 | BALive_pipo + BALiveTot 
#                                 # + def59_z_03 #+ def68_z_03
#                                 + def59_z_0 #+ def68_z_0
#                                 + def59_z_1 #+ def68_z_1
#                                 + def59_z_2 #+ def68_z_2
#                                 + def59_z_3 #+ def68_z_3
#                                 + CMD_1995 + MAP_1995
#                                 # + I(YEAR.DIFF)
#                                 # + REBURN,
#                                 + FIRE.SEV,
#                                 data = data.pipo.reburn,
#                                 family = binomial(link = "logit"),
#                                 minsplit = 50)
# tree.mob.pipo.reburn
# plot(tree.mob.pipo.reburn)
# dev.off()
## ^ one node if Y reburn; if N reburn, non-sensical -z-scores means greater likelihood

## Where is it failing?



################################################
## PSME
tree.mob.psme <- glmtree(regen_psme ~ YEAR.DIFF
                         | BALive_psme + BALiveTot # using BA trans doesn't chng
                         # + def59_z_03 #+ def68_z_03 # on, CMD less signif; - loglike same
                         + def59_z_0 #+ def68_z_0
                         # + def59_z_1 #+ def68_z_1 # on, CMD less signif; - loglike same
                         + def59_z_2 #+ def68_z_2
                         # + def59_z_3 #+ def68_z_3 # on, CMD less signif; - loglike same
                         + CMD_1995 + MAP_1995
                         # + I(YEAR.DIFF)
                         # + REBURN # # on means CMD less signif; - loglike same
                         + FIRE.SEV,
                         data = data.psme,
                         family = binomial(link = "logit"),
                         minsplit = 50)
## Output
tree.mob.psme
# tiff(paste0(out.dir,"psme_BA-CMD-SEV_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
plot(tree.mob.psme)
dev.off()

## Calculate deviance explained
glm.null.psme <- glm(regen_psme ~ 1,
                     data = data.psme,
                     family=binomial(link=logit)) # deviance for null model
glm.full.psme <- glm(regen_psme ~ YEAR.DIFF,
                     data = data.psme,
                     family=binomial(link=logit)) ## deviance for glm model
# Tree model:
(deviance(glm.null.psme) - deviance(tree.mob.psme))/deviance(glm.null.psme)
# Non-tree model:
(deviance(glm.null.psme) - deviance(glm.full.psme))/deviance(glm.null.psme)





################################################
## pied
tree.mob.pied <- glmtree(regen_pied ~ YEAR.DIFF
                         | BALive_pied + BALiveTot 
                         # + def59_z_03 #+ def68_z_03 
                         + def59_z_0 #+ def68_z_0
                         # + def59_z_1 #+ def68_z_1 
                         + def59_z_2 #+ def68_z_2
                         # + def59_z_3 #+ def68_z_3 
                         # + CMD_1995 + MAP_1995
                         # + I(YEAR.DIFF)
                         # + REBURN # 
                         + FIRE.SEV,
                         data = data.pied,
                         family = binomial(link = "logit"),
                         minsplit = 50)
## Output
tree.mob.pied
tiff(paste0(out.dir,"pied_BA-CMD-SEV_",currentDate,".tiff"),
     width = 640, height = 480, units = "px")
plot(tree.mob.pied)
dev.off()

## Calculate deviance explained
glm.null.pied <- glm(regen_pied ~ 1,
                     data = data.pied,
                     family=binomial(link=logit)) # deviance for null model
glm.full.pied <- glm(regen_pied ~ YEAR.DIFF,
                     data = data.pied,
                     family=binomial(link=logit)) ## deviance for glm model
# Tree model:
(deviance(glm.null.pied) - deviance(tree.mob.pied))/deviance(glm.null.pied)
# Non-tree model:
(deviance(glm.null.pied) - deviance(glm.full.pied))/deviance(glm.null.pied)



## Tidy
remove(list = ls(pattern = "glm")) 









###########################################
### SOLOMON'S CODE BELOW
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