data.pied <- read.csv("data.pied.csv") ; data.pied$X <- NULL
data.pipo <- read.csv("data.pipo.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme.csv") ; data.psme$X <- NULL

currentDate <- Sys.Date()

## Exclude sites w/ fire record but fire.sev 5 or 6 (here, NA)
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]


## Accurately characterize variables bc...
# glmtree treats categorical and num differently.
# N.b., for ordered factors, set ordinal=L2 to use dedicated ordinal stat...
# else, with too many vars, requisite p-threshold is too low to capture FIRE.SEV
# see https://stats.stackexchange.com/questions/392516/removal-of-partitioning-variable-in-final-glmtree-with-new-unused-partitioner/392682#392682
data.pied$regen_pied <- factor(data.pied$regen_pied, ordered = FALSE)
data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.pied$FIRE.SEV <- factor(data.pied$FIRE.SEV, ordered = TRUE)
data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
data.pied$REBURN <- factor(data.pied$REBURN, ordered = TRUE)
data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
data.psme$REBURN <- factor(data.psme$REBURN, ordered = TRUE)


################################################
## PIPO
tree.mob.pipo <- glmtree(regen_pipo ~ YEAR.DIFF
                | BALive_pipo + BALiveTot # using BA trans doesn't chng
                # + def59_z_03 #+ def68_z_03 
                # + def59_z_13 #+ def68_z_13
                # + def59_z_0 #+ def68_z_0
                # + def59_z_1 #+ def68_z_1
                # + def59_z_2 #+ def68_z_2
                # + def59_z_3 #+ def68_z_3 
                + CMD_1995 + MAP_1995
                # + I(YEAR.DIFF) 
                + REBURN 
                # + CMD.CHNG
                + FIRE.SEV,
                data = data.pipo, #data.pipo[data.pipo$REBURN == "Y",],
                family = binomial(link = "logit"),
                minsplit = 50,
                ordinal = "L2") # Use dedicated ordinal statistic for ordinal vars (FIRE.SEV)

## Output
plot(tree.mob.pipo)
tree.mob.pipo
summary(tree.mob.pipo)
# tiff(paste0(out.dir,"pipo_BA_MAP_sev_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
plot(tree.mob.pipo)
dev.off()

# Weird that +/- 59z0 bumps out FIRE.SEV -- that's only metric tho. 
# Probably related to fire weather that year?
cor.test(data.pipo$def59_z_0, as.numeric(data.pipo$FIRE.SEV))
plot(data.pipo$def59_z_0, as.numeric(data.pipo$FIRE.SEV))

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


## Grab & plot observations in given node
class(tree.mob.pipo)
tree <- tree.mob.pipo

# Get obs for each node (orig as df) and save row.names (match orig dataset)
obs.node1 <- (tree[[1]]$data) %>% row.names()
obs.node2 <- (tree[[2]]$data) %>% row.names()
obs.node3 <- (tree[[3]]$data) %>% row.names()
obs.node4 <- (tree[[4]]$data) %>% row.names()
obs.node5 <- (tree[[5]]$data) %>% row.names()
obs.node6 <- (tree[[6]]$data) %>% row.names()
obs.node7 <- (tree[[7]]$data) %>% row.names()

# Create new col in dataframe to assign node number
data.pipo$NODE <- NA

# Assign NODE value -- need INDEX, not df
# data.pipo$NODE[data.pipo[obs.node5 ,]]
# ^ won't work b/c what's in brackets is a dataframe
# data.pipo$NODE[rownames(data.pipo) %in% obs.node5] <- "node5" # works b/c its index 
# ^ works because what's in brackets is logical TRUE/FALSE

# Nb b/c subsetting on column ($NODE), don't need col references above as needed here:
a <- data.pipo[rownames(data.pipo) %in% obs.node2 ,] # df
b <- data.pipo[obs.node2 ,] # df
identical(a, b) # TRUE

# Only assign terminal nodes
data.pipo$NODE %>% factor()
# data.pipo$NODE[rownames(data.pipo) %in% obs.node1] <- "node1"
# data.pipo$NODE[rownames(data.pipo) %in% obs.node2] <- "node2"
data.pipo$NODE[rownames(data.pipo) %in% obs.node3] <- "node3"
data.pipo$NODE[rownames(data.pipo) %in% obs.node4] <- "node4"
# data.pipo$NODE[rownames(data.pipo) %in% obs.node5] <- "node5"
data.pipo$NODE[rownames(data.pipo) %in% obs.node6] <- "node6"
data.pipo$NODE[rownames(data.pipo) %in% obs.node7] <- "node7"

data.pipo %>% group_by(NODE) %>% count()

p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = data.pipo, aes(x = LON_FS, y = LAT_FS,
                                   size = YEAR.DIFF,
                                   col = NODE))
p


## This just gets rules, I think
tr <- unclass(node_party(tree))
lapply(get_paths(tree, nodeids(tree, terminal = TRUE)),
       function(path) tr[path])



## Eval skill of model in predicting hold-out data.
# N.b., an apply k-fold cross-validation on glm using boot::cv.glm but not here on tree (i.e. list)
data$pred <- predict(tree.mob.pipo, newdata=data, type = "response") # response gives probabilities

data <- data.pipo

#Randomly shuffle the data
data<-data[sample(nrow(data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation (https://stackoverflow.com/questions/21380236/cross-validation-for-glm-models)
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  #Use test and train data partitions however you desire...
}

tree.mob.pipo <- glmtree(regen_pipo ~ YEAR.DIFF | BALive_pipo + BALiveTot + CMD_1995 + MAP_1995 + FIRE.SEV,
                         data = trainData,
                         family = binomial(link = "logit"),
                         minsplit = 50,
                         ordinal = "L2")
plot(tree.mob.pipo)


# Create confusion matrix
# https://stats.stackexchange.com/questions/17052/how-to-test-a-logistic-regression-model-developed-on-a-training-sample-on-the-d
predTest <- predict(tree.mob.pipo, testData)
thresh  <- 0.5            # threshold for categorizing predicted probabilities
predFac <- cut(predTest, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
confsn    <- table(testData$regen_pipo, predFac, dnn=c("actual", "predicted"))
addmargins(confsn)
#       predicted
# actual  0  1 Sum
#    0   36  1  37
#    1   14  3  17
#    Sum 50  4  54


# Alt confusion matrix
install.packages("caret")
library(caret)
# specifying threshold 
predTest <- predict(tree.mob.pipo, testData)
confusionMatrix(data = as.factor(as.numeric(predTest>0.5)), # needs factor but set num for 0/1 first
                reference = as.factor(testData$regen_pipo), # needs factor
                dnn = c("prediction", "reference"))

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