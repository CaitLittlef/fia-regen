data.pied <- read.csv("data.pied.csv") ; data.pied$X <- NULL
data.pipo <- read.csv("data.pipo.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme.csv") ; data.psme$X <- NULL


## Consider transformations
hist(data.pipo$YEAR.DIFF)
hist(data.pipo$YEAR.DIFF^0.5)
hist(data.pipo$BALive_pipo)
hist(data.pipo$BALive_pipo^0.5) 
data.pipo$BALive_pipo_trans <- data.pipo$BALive_pipo^0.5 


## Fit MOB 
# An algorithm for model-based recursive partitioning... a PARAMETRIC MODEL TREE
# yielding a tree with fitted models associated with each terminal node.
# Partition model given before | with all following variables.
# ^ But if you've created that initial model WITHOUT those vars, how can you??
# Or are they all partitioned and THEN the model is constructed? I don't think so...

# model is created (just like single group may be created in initial round of a CART) and then each of those additional variables is tested to see if that can shake the model up -- if there's a better one to be had if the predictor set (x) is subdivided based on those partition variable values

# If a partitioning variable doesn't show up, that means it's not being used at all as a predcitor in this big picture (neither individual glms at nodes b/c that's just pre-| nor as partitioner (pre-|)?). 

# Seems like that can't be the case b/c adding in additional potential partitioners changes model even if they don't show up as relevant partitioners. And turning partitioning variables on or off, even if it doesn't change tree stucture, changes p values...

# Use if you can't fit a global model'

# remove.packages("partykit")
# install.packages("party")
# library(party)

################################################
## PIPO
tree.mob2 <- mob(regen_pipo ~ YEAR.DIFF
                | YEAR.DIFF + BALive_pipo + BALiveTot
                # + def59_z_03 #+ def68_z_03
                + def59_z_0 #+ def68_z_0
                # + def59_z_1 #+ def68_z_1
                # + def59_z_2 #+ def68_z_2
                # + def59_z_3 #+ def68_z_3
                + CMD_1995 + MAP_1995
                + FIRE.SEV,
                data = data.pipo,
                model = glinearModel, family = binomial(link = "logit"),
                control = mob_control(minsplit = 62))

plot(tree.mob)
plot(tree.mob, terminal_panel = NULL)
plot(tree.mob, tp_args = list(cdplot = FALSE))
plot(tree.mob, terminal_panel = node_scatterplot(tree.mob))
plot(tree.mob, terminal_panel = node_scatterplot)
plot(tree.mob, terminal_panel = node_bivplot)
plot(tree.mob, terminal_panel = node_bivplot(tree.mob, cdplot = FALSE))
coef(tree.mob, node = 6)
deviance(tree.mob)
logLik(tree.mob)
AIC(tree.mob)
BIC(tree.mob)




tree.mob
plot(tree.mob, terminal_panel=node_density)
plot(tree.mob, terminal_panel=node_scatterplot) # seems to be default
plot(tree.mob, tp_args = list(fivenum = FALSE))
plot(tree.mob, tp_args = list(cdplot = FALSE))


## Calculate deviance explained

glm.null.pipo <- glm(regen_pipo ~ 1,
                    data = data.pipo,
                    family=binomial(link=logit)) # deviance for null model
glm.full.pipo <- glm(regen_pipo ~ BALive_pipo,
                   data = data.pipo,
                   family=binomial(link=logit)) ## deviance for glm model

# tree model ###### HOW DO YOU GET DEVIANCE FROM A TREE?? #####
(deviance(glm.null.pipo) - deviance(tree.mob))/deviance(glm.null.pipo) #0.09
# non-tree mods
(deviance(glm.null.pipo) - deviance(glm.full.pipo))/deviance(glm.null.pipo) #0.02







################################################
## PSME
tree.mob <- mob(regen_psme ~ YEAR.DIFF
                | BALive_psme + BALiveTot
                # + def59_z_03 #+ def68_z_03
                + def59_z_0 #+ def68_z_0
                # + def59_z_1 #+ def68_z_1
                # + def59_z_2 #+ def68_z_2
                # + def59_z_3 #+ def68_z_3
                + CMD_1995 + MAP_1995
                + FIRE.SEV,
                data = data.psme,
                model = glinearModel, family = binomial(link = "logit"),
                control = mob_control(minsplit = 30))

plot(tree.mob)
prettytree(tree.mob)
tree.mob
plot(tree.mob, terminal_panel=node_density)
plot(tree.mob, terminal_panel=node_scatterplot) # seems to be default
plot(tree.mob, tp_args = list(fivenum = FALSE))
plot(tree.mob, tp_args = list(cdplot = FALSE))


## Calculate deviance explained

glm.null.pipo <- glm(regen_pipo ~ 1,
                     data = data.pipo,
                     family=binomial(link=logit)) # deviance for null model
glm.full.pipo <- glm(regen_pipo ~ BALive_pipo,
                     data = data.pipo,
                     family=binomial(link=logit)) ## deviance for glm model

# tree model ###### HOW DO YOU GET DEVIANCE FROM A TREE?? #####
(deviance(glm.null.pipo) - deviance(tree.mob))/deviance(glm.null.pipo) #0.09
# non-tree mods
(deviance(glm.null.pipo) - deviance(glm.full.pipo))/deviance(glm.null.pipo) #0.02









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