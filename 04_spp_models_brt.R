######################################################
### SETUP
######################################################

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo_2019-08-21.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_2019-08-21.csv") ; data.psme$X <- NULL

## What's it like without fire?
# All sites with and without fire:
# mean(data.pipo$regen_pipo) # 0.2557105
# mean(data.psme$regen_psme) # 0.4452158
# mean(data.pipo$BALive_pipo_m) # 9.203423
# mean(data.psme$BALive_psme_m) # 9.550503
# sites without fire:
pipo.woburn <- data.pipo[is.na(data.pipo$FIRE.SEV) ,]
# psme.woburn <- data.psme[is.na(data.psme$FIRE.SEV) ,]
# mean(pipo.woburn$regen_pipo) # 0.2659649
# mean(psme.woburn$regen_psme) # 0.4694846
# mean(pipo.woburn$BALive_pipo_m) # 9.684631
# mean(psme.woburn$BALive_psme_m) # 10.39258
# State-specific BA: 4 AZ, 35 NM, 16 ID, 30 MT
# pipo.woburn %>%
#   filter(STATECD.x == 16 | STATECD.x == 30) %>%
#   summarize(mean = mean(BALive_pipo_m),
#             median = median(BALive_pipo_m))
# pipo.woburn %>%
#   filter(STATECD.x == 4 | STATECD.x == 35) %>%
#   summarize(mean = mean(BALive_pipo_m),
#             median = median(BALive_pipo_m))
# psme.woburn %>%
#   filter(STATECD.x == 16 | STATECD.x == 30) %>%
#   summarize(mean = mean(BALive_psme_m),
#             median = median(BALive_psme_m))
# psme.woburn %>%
#   filter(STATECD.x == 4 | STATECD.x == 35) %>%
#   summarize(mean = mean(BALive_psme_m),
#             median = median(BALive_psme_m))

## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]

## ID outliers present in this fire-filtered dataset:
# max(data.pipo$DUFF_DEPTH) ; mean(data.pipo$DUFF_DEPTH)
# boxplot(data.pipo$DUFF_DEPTH)
# max(data.pipo$LITTER_DEPTH) ; mean(data.pipo$LITTER_DEPTH)
# boxplot(data.pipo$LITTER_DEPTH)
# 
# max(data.psme$DUFF_DEPTH_cm) ; mean(data.psme$DUFF_DEPTH_cm)
# boxplot(data.psme$DUFF_DEPTH_cm)
# max(data.psme$LITTER_DEPTH_cm) ; mean(data.psme$LITTER_DEPTH_cm)
# boxplot(data.psme$LITTER_DEPTH_cm)
# Crazy big outlier in psme duff
data.psme <- data.psme[data.psme$DUFF_DEPTH_cm < 13,]


## What are dimensions of dataset?
# count(data.pipo) # 521
# count(data.psme) # 693
# count(data.pipo) + count(data.psme) # 1214
# count(inner_join(data.pipo, data.psme, by = "UNIQUEID")) # 224 both pipo and psme
# count(full_join(data.pipo, data.psme, by = "UNIQUEID")) # 990
# 990 + 224 # 1214
# fire count
# count(full_join(data.pipo, data.psme, by = "UNIQUEID"), "Fire_ID.x")
# temp <- full_join(data.pipo, data.psme, by = c("UNIQUEID", "Fire_ID"))
# length(unique(temp$Fire_ID)) # 431
# rm(temp)



## Set variable classes
# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) 
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
# data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))

data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
# data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV)
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
# data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV, ordered = TRUE)

data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
# data.pipo$REBURN <- as.numeric(data.pipo$REBURN)
# data.psme$REBURN <- factor(data.psme$REBURN, ordered = TRUE)
# data.psme$REBURN <- as.numeric(data.psme$REBURN)


# If I've already run & saved models and want to re-load, do so here:
# tobeloaded <- paste0(out.dir,"pipo_mods_2019-08-23.Rdata") ; sp <- c("pipo")
# tobeloaded <- paste0(out.dir,"psme_mods_2019-09-02.Rdata") ; sp <- c("psme")
temp.env = new.env()
invisible(lapply(tobeloaded, load, envir = temp.env))
models = as.list(temp.env)
rm(temp.env, tobeloaded)

## If want to re-load stats.new from existing model run (for boxplot)
# stats.new <- read.csv(paste0(out.dir,"pipo_brt_stats_fin_relinf_ordered_2019-08-23.csv"))
# stats.new <- read.csv(paste0(out.dir,"psme_brt_stats_fin_relinf_ordered_2019-07-03.csv"))
# stats.new[1] <- NULL # extra column gets added


# PIPO or PSME?? Retain only the variables being used in models.
## PIPO
data.brt <- data.pipo %>%
  dplyr::select(regen_pipo, BALive_pipo_m, YEAR.DIFF, def.tc, tmax.tc, ppt.tc, #CMD_CHNG,
                 # def59_z_max1,
                # def59_z_max12,
                def59_z_max13,
                # def59_z_max14,
                # def59_z_max15,
                DUFF_DEPTH_cm, LITTER_DEPTH_cm, FIRE.SEV, REBURN) %>%
  rename(regen_brt = regen_pipo,
         BALive_brt_m = BALive_pipo_m) ; sp <- c("pipo")



# PSME
# data.brt <- data.psme %>%
#   dplyr::select(regen_psme, BALive_psme_m, YEAR.DIFF, def.tc, tmax.tc, ppt.tc, #CMD_CHNG,
#                 def59_z_max13,
#                 # def59_z_max1, def59_z_max12, def59_z_max13, def59_z_max14, def59_z_max15,
#                 DUFF_DEPTH_cm, LITTER_DEPTH_cm, FIRE.SEV, REBURN) %>%
#   rename(regen_brt = regen_psme,
#          BALive_brt_m = BALive_psme_m) ; sp <- c("psme")



## List vars (factors last so any iterative plot creation doesn't stop midway thru)
# Iteratively remove each var to explore improvement in AUC
explan.vars <- c("YEAR.DIFF",
                 "BALive_brt_m",
                 "def.tc",
                 "tmax.tc",
                 "ppt.tc",
                 # "def59_z_max1",
                 # "def59_z_max12",
                 "def59_z_max13",
                 # "def59_z_max14",
                 # "def59_z_max15",
                 "DUFF_DEPTH_cm",
                 "LITTER_DEPTH_cm",
                 "FIRE.SEV",
                 "REBURN")

# Names (with expressions)
explan.vars.names <- c("Years since fire",
                       expression(paste("Live BA (m"^2,"ha"^-1,")")),
                       "Deficit (mm)",
                       expression(paste("Max temp (",degree*C,")")),
                       "Precip (mm)",
                       "Max deficit anomaly",
                       "Duff depth (cm)",
                       "Litter depth (cm)",
                       "Fire severity",
                       "Reburn")


## After iteratively dropping w/ 10 runs (below) to see which boosts AUC, re-define explan.vars

## PIPO
pipo.explan.vars <- explan.vars[-c(3, 5, 7, 8, 10)] # have iteratively removed vars and 
# pipo.explan.vars <- explan.vars[-c(7, 8, 9, 10)] # have iteratively removed vars, including CMD to begin with
# ^ This is the final dataset for PIPO, as removing others doesn't improve AUC
pipo.explan.vars.names <- explan.vars.names[-c(3, 5, 7, 8, 10)]
# pipo.explan.vars.names <- explan.vars.names[-c(7, 8, 9, 10)]
# pipo.explan.vars.names <- explan.vars.names[-c(3, 5, 6, 8, 9, 10, 11)] # max15, exclude cmd

# N.b., overall AUC with max def 13, following var selection process gives AUC: 0.7704252
# N.b., swapping max def 12 in for max def 1-3 but retaining other var selection gives AUC: 0.7630822
# N.b., swapping max def 1 (so, just 1) in for max def 1-3 but retaining other var selection gives AUC: 0.7584048
# 1-4: 0.774231
# 1-5: 0.7717764


pipo.explan.vars
pipo.explan.vars.names

## PSME
psme.explan.vars <- explan.vars[-c(3, 5, 6, 8, 9, 10)] # excluding 13cm duff outlier, max 13
# psme.explan.vars <- explan.vars[-c(3, 5, 6, 8, 9, 10)] # including 13cm duff outlier, max 13
# psme.explan.vars <- explan.vars[-c(3, 5, 7, 9, 10)]  # max1-5
# ^ This is the final dataset for PSME, as removing others doesn't improve AUC
psme.explan.vars.names <- explan.vars.names[-c(3, 5, 6, 8, 9, 10)] # excluding 13cm duff outlier, max 13
# psme.explan.vars.names <- explan.vars.names[-c(3, 5, 6, 8, 9, 10)] # including 13cm duff outlier, max 13
# psme.explan.vars.names <- explan.vars.names[-c(3, 5, 7, 9, 10)] # max1-5
psme.explan.vars
psme.explan.vars.names

if (sp == "pipo") explan.vars <- pipo.explan.vars else explan.vars <- psme.explan.vars
if (sp == "pipo") explan.vars.names <- pipo.explan.vars.names else explan.vars.names <- psme.explan.vars.names

explan.vars
explan.vars.names
sp

###############################################################
##NOW CAN JUMP TO PDPS SCRIPT IF DON'T NEED TO RE-RUN MODELS##
###############################################################

###################
#### BRT SETUP ####   
###################

## Set learning rate
# LR<-0.01
# LR<-0.02
# LR<-0.005
LR<-0.001
# LR<-0.0005

## Set tree complexity
# TC <- 1
TC <- 3
# TC <- 5
# TC <- 10

# How many iterations?
# num.loops <- 1
# num.loops <- 1:2
# num.loops <- 1:3
# num.loops <- 1:5
num.loops <- 1:10
# num.loops <- 1:100


## Create empty list to store models in; create vectors to store stats, etc.
models <- list()
model.name <- NULL
brt.perc.dev.expl <- NULL
cv.correlation <- NULL
cv.discrim <- NULL
trees <- NULL
var <- list()
rel.inf <- list()
auc <- NULL

## WHAT SPECIES ARE YOU RUNNING?
print(sp)

################
### BRT LOOP ###   
################

### !!! IF ITERATIVELY DROPPING, ACTIVATE THE FOLLOWING LINES:
# Lines 174-176: for (v in 2:length(explan.vars)) 
# Lines 181-184: version <- paste0("x",explan.vars[v])
# Lines 190-191: gbm.x = explan.vars[-v],
# Lines 264-273 & 271-280: change csv names

start <- Sys.time() 
for (v in 1){ # if not iteratively dropping vars
# for (v in 2:length(explan.vars)){ # iteratively drops all vars (except YEAR.DIFF)
  # for (v in 2:12){ # if poops out, complete from pt of pooping
  for (i in num.loops){
  # for (i in 9:100){ # if poops out, complete run from pt of pooping
    
  # If iteratively dropping var, include x in name to ID which has been left out.
  # version <- "allvars"
  # version <- paste0("x",explan.vars[v])
  # version <- "fin"
  version <- "fin_1yr"

  data <- data.brt
  
  # Loop through model creation i times
  models[[i]] <-gbm.step(data=data, 
                  gbm.x = explan.vars,
                  # gbm.x = explan.vars[-v],
                  gbm.y = "regen_brt",          
                  family = "bernoulli", 
                  tree.complexity = TC, # number of nodes in a tree
                  learning.rate = LR, 
                  bag.fraction = 0.75, # pretty universally used
                  n.trees=500, # starting number of trees -- good for pipo
                  step.size=5, # iteratively add this number of trees
                  max.trees=2250, # max out at this number of trees -- good for pipo
                  verbose=TRUE) 
  ## Grab info/stats
  # Model name
  model.name.temp <- paste0(sp,".",version,".",i)
  # Percent deviance explained
  brt.perc.dev.expl.temp <-1-(models[[i]]$self.statistics$mean.resid/models[[i]]$self.statistics$mean.null)
  # CV correlation
  cv.correlation.temp  <- models[[i]]$cv.statistics$correlation.mean
  # CV discrim (ie training auc; see source code for gbm.step)
  cv.discrim.temp  <- models[[i]]$cv.statistics$discrimination.mean
  # Number of trees in best model
  tree.temp <-
    models[[i]]$trees.fitted[match(TRUE, models[[i]]$cv.values == min(models[[i]]$cv.values))]
  
  # Capture each variable & its relative influence
  var.temp <- paste0(models[[i]]$contributions$var) 
  rel.inf.temp <- paste0(models[[i]]$contributions$rel.inf)

  # Add temp of each metric to vector
  model.name <- c(model.name, model.name.temp)
  brt.perc.dev.expl <- c(brt.perc.dev.expl, brt.perc.dev.expl.temp)
  cv.correlation <- c(cv.correlation, cv.correlation.temp)
  cv.discrim <- c(cv.discrim, cv.discrim.temp)
  trees <- c(trees, tree.temp )
  var <- c(var, var.temp)
  rel.inf <- c(rel.inf, rel.inf.temp)
  
  ## Calc auc
  pred <- predict.gbm(models[[i]], n.trees = tree.temp, data, type = "response")
  roccurve <- roc(data$regen_brt ~ pred)
  auc.temp <- pROC::auc(roccurve)
  auc <- c(auc, auc.temp)
  } 
}

print(Sys.time() - start)


#####################################################################
## COMPILE ALL MODEL PERFORMANCE STATS & VARIABLE INFLUENCE VALUES ##
#####################################################################

# How many vars? If all, retain all in number. If dropping iteratively, remove one.
num.vars <- ifelse((version == "allvars" | version == "fin" | version == "fin_1yr"), length(explan.vars), length(explan.vars)-1)

# Size of matrix will adjust depending on number of explanatory variables
var.mat <- matrix(unlist(var), ncol = num.vars, byrow = TRUE) 
rel.inf.mat <- matrix(unlist(as.numeric(rel.inf)), ncol = num.vars, byrow = TRUE) 

stats <- cbind.data.frame(model.name, cv.correlation, cv.discrim, auc, brt.perc.dev.expl,
                 trees, var.mat, rel.inf.mat)
colnames <- c("model.name", "cv.correlation", "cv.discrim", "auc", "brt.perc.dev.expl", "num.trees",
              paste0("var.", 1:num.vars), # create col names based on num vars.
              paste0("rel.inf.", 1:num.vars))
colnames(stats) <- colnames              

## What's AUC range? For all variables included, min-max auc = 0.0095.
# Therefore, drop vars that increase AUC by meager < 0.1 to have parsimonious model.
mean(auc) ; sd(auc) ; range(auc) ; max(auc) - min(auc)

# Save as csv -- options below are for runs with vars dropped iteratively
currentDate <- Sys.Date()
# csvFileName <- paste0(sp,"_brt_stats_all_", currentDate,".csv") # all vars in
csvFileName <- paste0(sp,"_brt_stats_x1_", currentDate,".csv") # 1 var removed
# csvFileName <- paste0(sp,"_brt_stats_x2_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x3_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x4_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x5_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x6_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x7_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x8_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_fin_", currentDate,".csv")
csvFileName <- paste0(sp,"_brt_stats_fin_1yr_", currentDate,".csv")
write.csv(stats, paste0(out.dir,"/",csvFileName))

## If iteratively dropping, get mean & sd of all stats and relative influences
stats.sum <- stats[,1:6]
# Create column that just has model version (i.e., which var is dropped), not 1, 2, 3...10
stats.sum$version <- gsub('.([^.]*)$', '', stats$model.name) # match everything before last .
stats.sum <- stats.sum %>%
  group_by(version) %>%
  dplyr::select(-model.name) %>%
  summarize_all(list(mean, sd)) # nb this will name _fn1 and _fn2, not _mean and _sd

# Save as csv
currentDate <- Sys.Date()
# csvFileName <- paste0(sp,"_brt_stats_all_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x1_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x2_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x3_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x4_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x5_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x6_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x7_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x8_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_fin_sum_", currentDate,".csv")
csvFileName <- paste0(sp,"_brt_stats_fin_1yr_sum_", currentDate,".csv")
write.csv(stats.sum, paste0(out.dir,"/",csvFileName))


#########################################################
##IF ITERATIVELY DROPPING VARS, THIS IS YOUR PLAYGROUND##
#########################################################

## See which variable drop maximizes AUC, doesn't kill % dev explained.
# What was auc and %dev of model that contained all vars?

# pipo
# all.auc <- 0.785582688 # original with all variables
# all.perc.dev. <- 0.095961917 # original with all variables
# all.auc <- 0.775534118 # original with all variables max 1-3 not 1-5
# all.auc <- 0.7688434 # all variables max 1

# psme
# all.auc <- 0.8495447 # original with all variables
# all.perc.dev. <- 0.221335143 # original with all variables

# If increase from drop is >0, pursue drop. 
# If decrease from drop is <0.01, pursue drop for more parsinomious model.
# If decrease from drop is >0.01, do not pursue drop and retain dropped var.
stats.sum <- as.data.frame(stats.sum)
stats.sum[which.max(stats.sum$auc_fn1),]
stats.sum[which.max(stats.sum$auc_fn1),]$version
stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1
stats.sum[which.max(stats.sum$auc_fn1),]$brt.perc.dev.expl_fn1

########################################################################## 
########################################################################## 
#### PIPO VARIABLE SELECTION: DEF.MAX.13 WAS BEST OF ALL DEF.MAXS NUM ####
########################################################################## 


##################################### BELOW HAS DROPPING ASSOCIATED WITH MAX13 ####
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - all.auc
# ^ dropping litter increases auc by 0.004748579 (pursue any increase)
# No litter: 0.780282697
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.780282697 #(no litter)
# ^ dropping reburn decreases auc but by -0.002008393 (pursue <0.01 decrease)
# No reburn: 0.7782743
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7782743 #(no reburn)
# ^ dropping precip increases auc by 0.0005211252 (pursue any increase)
# No precip: 0.7787954
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7787954 #(no precip)
# ^ dropping deficit decreases auc by -0.004212758 (pursue < 0.01 decrease)
# No deficit: 0.7745826
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7745826 #(no deficit)
# ^ dropping DUFF decreases auc but by -0.0037769 (pursue < 0.01 decrease) 
# No duff: 0.7708057
stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7745826 #(no duff)
# ^ dropping FIRE.SEV decreases auc by -0.01215869, which is too big a drop. 
# No FIRESEV: 0.7624239
# So, retain fire severity.

# Final model explan.vars for pipo
explan.vars
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max13" "FIRE.SEV"    
# ^ AUC is 0.7745826
0.7745826 - all.auc # -0.000951518 # not substantial decline from all, either.


##################################### BELOW HAS DROPPING ASSOCIATED WITH MAX1 ####
# all.auc <- 0.768843387
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - all.auc (no reburn)
# ^ dropping reburn increases auc by 0.0022851 (pursue any increase)
# No reburn: 0.7711285 
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7729009 #(no litter)
# ^ dropping litter increases auc by 0.0000000100412 (pursue any increase)
# No litter: 0.7729009
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7738617 #(no def max 1)
# ^ dropping 1 yr deficit decreases auc but by -0.00000002011314 (pursue <0.01 decrease)
# STOPPING HERE B/C MAX13 WAS BETTER AND RETAINS A POST-FIRE DEFICIT


##################################### BELOW HAS DROPPING ASSOCIATED WITH MAX12 ####
# all.auc <- 0.7730469
stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - all.auc
# ^ dropping litter increase auc by 0.004427275 (pursue any increase)
# no litter: 0.7774742
stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 -  0.7774742
# ^ dropping fire severity increases auc by 0.0006763565 (pursue any increase)
# no fire severity: 0.7781506
stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 -  0.7781506
# dropping duff decreases AUC but by only -0.002182913 (pursue <0.01 decrease)
# no duff: 0.7759677
stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 -  0.7759677
# dropping reburn decreases auc but by only -0.003662467 (pursue < 0.01 decrease)
# no reburn: 0.7723052 
stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7723052
# ^ dropping 12 yr defciit decreases auc but by -0.0006532908 (pursue <0.01 decrease)
# STOPPING HERE B/C MAX13 WAS BETTER AND RETAINS A POST-FIRE DEFICIT


##################################### BELOW HAS DROPPING ASSOCIATED WITH MAX15 ####
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - all.auc
# ^ dropping litter increases auc by 0.001513789 (pursue any increase)
# No litter: 0.7870965
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7870965
# ^ dropping reburn decreases auc but by -0.006177389 (pursue <0.01 decrease)
# No reburn: 0.7809191
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7809191
# ^ dropping FIRE SEV decreases auc but by -0.0007182607 (pursue <0.01 decrease)
# No FIRE.SEV: 0.7802008
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7802008
# ^ dropping precip increases auc by 0.0002559884 (pursue any increase)
# No precip: 0.7804568
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7804568
# ^ dropping deficit decreases auc but by -0.004511402 (pursue <0.01 decrease)
# No deficit: 0.7759454
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7759454
# ^ dropping DUFF decreases auc but by -0.008151174 (pursue <0.01 decrease)
# No duff: 0.767794226
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.767794226
# ^ dropping def zmax decrease auc by -0.01405875 (STOP)
# That's too much of a drop, so retain def zmax in final model.

# Final model explan.vars for pipo
# explan.vars
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max15" 
# ^ AUC is 0.767794226
# 0.767794226 - all.auc # -0.01778846 # not substantial decline from all, either.






#### PSME VARIABLE SELECTION ####
## Iteratively working thru maxes (1, 12, 13, 14, 15), 14 showed most promise at first.
## auc vals were 1: 0.8458473, 12: 0.8433526, 13: 0.8482665, 14: 0.848701, 15: 0.8466342.
## 13 is not far behind 14.
## 14 was retained with final model selection (13 doesn't), but has minimal influence.
## Sticking with 13.

##################################### BELOW HAS DROPPING ASSOCIATED WITH MAX13 ####
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - all.auc
# ^ dropping fire sev increases auc by 0.0005087891 (pursue any increase)
# No fire sev: 0.8500535
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8500535
# ^ dropping deficit decreases but by -0.001090366 (pursue decrease < 0.01)
# no def: 0.8489631
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8489631
# ^ dropping litter decreases but by -0.0005060559 (pursue decrease < 0.01)
# no litter: 0.848457
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.848457
# ^ dropping reburn decreases but by -0.001110884 (pursue decrease < 0.01)
# no reburn: 0.8473461
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8473461
# ^ dropping max def 13 decreases but by -0.005159629 (pursue decrease < 0.01)
# no def max 13: 0.8421865
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8421865
# ^ dropping precip decreases but by -0.004929258 (pursue decrease < 0.01)
# no precip: 0.8372572
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8372572
# ^ dropping duff decrease by MORE than 0.01 therefore retain (-0.01655588)
# no duff: 0.8207013

# final psme vars:
# explan.vars
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "DUFF_DEPTH_cm"


# Below is what I did for PSME variable selection; now complete.
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - all.auc
# ^ dropping def decreases auc but by -0.0004619858 (pursue <0.01 decreaes)
# No def: 0.8478045 
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8478045
# ^ dropping reburn increases auc by 0.0005817358 (pursue any increase)
# No reburn: 0.8483862
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8483862
# ^ dropping FIRE.SEV increases auc by 0.00112926 (pursue any increase)
# No fire sev: 0.8495155
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8495155
# ^ dropping litter decreases auc but by -0.001676767 (pursue <0.01 decreases)
# No litter: 0.8478387
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8478387
# ^ dropping precip decreases auc but by -0.005967746 (pursue <0.01 decreases)
# No precip: 0.841871
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.841871
# ^ dropping def max 13 decreases auc but by -0.005321088 (pursue <0.01 decreases)
# no max def13: 0.836549912
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.836549912
# ^ dropping duff decreases auc by -0.01620437 which is too much.
# RETAIN DUFF

#
# FINAL PSME EXPLAN VARS
# explan.vars
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "DUFF_DEPTH_cm"

##################################### BELOW HAS DROPPING ASSOCIATED WITH MAX14 ####
# Below is what I did for PSME variable selection; now complete.
# I think dropped def first.
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8478045
# ^ dropping fire sev increases auc by 0.001173018 (pursue any increase)
# No fire sev: 0.851791
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.851791
# ^ dropping reburn decreaes auc but by -0.001447442 
# No reburn: 0.8503436
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8503436
# ^ dropping duff decreases auc but by -0.001019766 (pursue <0.01 decrease)
# No duff: 0.8493238
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8493238
# ^ dropping precip decreases auc but by -0.0057419 (pursue <0.01 decrease)
# no precip: 0.8435819
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8435819
# ^ dropping def14 decreases auc by -0.01092952
# no def14: 0.8326524
# RETAIN DEF14

# FINAL PSME VARIABLES:
# explan.vars
# [1] "YEAR.DIFF"       "BALive_brt_m"    "tmax.tc"         "def59_z_max14"   "LITTER_DEPTH_cm"


##################################### BELOW HAS DROPPING ASSOCIATED WITH MAX15 ####
# def dropped first... I think?
# ^ dropping fire severity increases auc by 0.0007740266 (pursue > 0 increase)
# No fire severity: 0.8477908 
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8477908
# ^ dropping DUFF decreases auc but by -0.002578711 (pursue < 0.01 decrease)
# No duff: 0.8452121
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8452121
# ^ dropping def zmax decreases auc but by -0.004358817 (persue < 0.01 decrease)
# No def zmax: 0.8408533
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8408533
# ^ dropping precip decreases auc but by -0.007696194 (persue < 0.01 decrease)
# No precip: 0.8331571
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8331571
# ^ dropping litter decreases auc by -0.01233934 (do NOT pursue b/c > 0.01 decrease)
# No litter: 0.8208178 -- RETAIN litter in final model.





######################################################
##CREATE ORDERED STATS LIST BY VAR WITH FINAL VARS####
######################################################

## Create a list for storing re-named relative influence values.
#List b/c they'll be dataframes, technically.
stats.list <- list()

# How many rows should get looped through?
rows <- 1:nrow(stats)

# Which are variable columns & relative influence columns?
var.cols <- grep(pattern="^var.*", x=colnames(stats))
rel.inf.cols <- grep(pattern="^rel.inf.*", x=colnames(stats))

# Run loop to rename values and store in list.
for (k in rows){
  # Pull out one row at a time from stats table
  stats.row <- stats[k,] # Don't have to specify rows hereafter b/c there's only 1.
  colnames(stats.row)
  
  # Take variables (as row entires) and unlist, in order. Unname takes off col names.
  col.to <- unname(unlist(stats.row[,var.cols])) %>% as.character()
  
  # Define which columns I want to change: they'll go from FROM to TO.
  col.from <- colnames(stats.row[,rel.inf.cols]) 
  
  # Use vars to specify columns for renaming. Send those FROM cols to TO.
  stats.row <- stats.row %>% rename_at(vars(col.from), ~col.to)
  # Alt: stats.row %>% rename_at(vars(col.from), function(x) col.to) %>% head(2)
  # Rename_at is applied to all selected columns. I.e., function(x) basically receives
  # the col.from values in x, then does stuff with 'em. Vars is selector.
  
  # Rel.inf values are now in columns with associated var names. Drop cols w/ var names.
  stats.row <- stats.row %>% dplyr::select(-(var.cols)) 
  
  # Fill list with each new row of stats AND asso. col names. These are technically dataframes
  stats.list[[k]] <- stats.row
}

# Bind all the rows stored as separate list columns (done to maintain col names).
# dplyr's bind_rows automagically splices contents of lists per col names. 
stats.new <- bind_rows(stats.list) # bind_rows automatically splices contents of lists per col names.


## Boxplot of relative influence; plot by median
# Gather data to make ggplot happy
temp <- stats.new[,-(1:6)] # retain only cols with variables
View(temp) # PIPO: Yr, live, def, fire sev, tmax 
View(temp) # PSME: ba, yr, duff, tmax 
temp <- gather(temp, key = "var", value = "rel.inf")


# Var names in order of influence:
# pipo:# Yr, live, def, fire sev, tmax
pipo.var.names <- c("Years since fire",
                    expression(paste("Live BA (m"^"2","ha"^"-1",")")),
                    "Max deficit anomaly",
                    "Fire severity",
                    expression(paste("Max temp (",degree*C,")")))
# psme: BA, yrs, litter, tmax
psme.var.names <- c(expression(paste("Live BA (m"^"2","ha"^"-1",")")),
                    "Years since fire",
                    "Duff depth (cm)",
                    expression(paste("Max temp (",degree*C,")")))

if (sp == "pipo") var.names <- pipo.var.names else var.names <- psme.var.names
rm(pipo.var.names, psme.var.names)

# Create & save box-plot
tiff(paste0(out.dir, sp, "_brt_stats_", version, "_relinf_", currentDate,".tif"),
            res = 300, width = 5, height = 4, units = "in")
ri.plot <- ggplot(data = temp,
                       aes(x = reorder(var, -rel.inf, # put - before rel.inf if reverse
                                       FUN = median),
                           y = rel.inf))
ri.plot + geom_boxplot() +
  labs(x = NULL, y = "Relative influence (%)") + 
  scale_x_discrete(labels = var.names) +
  scale_y_continuous() + 
  coord_flip() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw(base_size = 18) +
  theme(axis.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  # theme_caitlin()
dev.off()


## Write ordered stats to new csv
currentDate <- Sys.Date()
write.csv(stats.new, paste0(out.dir,sp,"_brt_stats_", version, "_relinf_ordered_",  currentDate,".csv"))

## Get summary (mean & sd) of all stats and relative influences
stats.sum <- stats.new %>%
  dplyr::select(-model.name) %>%
  summarize_all(list(mean, sd)) # nb this will name _fn1 and _fn2, not _mean and _sd

## Write summary of stats to new csv
currentDate <- Sys.Date()
write.csv(stats.sum, paste0(out.dir,sp,"_brt_stats_",version, "_relinf_sum_",currentDate,".csv"))



#######################
###SAVE MODELS JIC#####   
#######################

# Save models JIC. save() needs names and will look to global envi unless I say look to list.
names(models) <- paste0(sp,".",version,".",1:length(num.loops)) # First assign names to each of the models.
currentDate <- Sys.Date()
Rdata.name <- paste0(out.dir, sp, "_mods_", currentDate,".Rdata")
save(list=names(models),
     file=paste0(Rdata.name),
     envir=as.environment(models))
# load() # Insert that .Rdata here to reload models



#######################
### CLEAN YOSELF UP ###   
#######################


## Clean up
# remove(list = ls(pattern = "stats"), name, col.to, col.from, currentDate) 
# remove(var, var.mat1, var.mat2)
# remove(rel.inf, rel.inf.mat1, rel.inf.mat2)
# remove(list = ls(pattern = ".temp"))
# remove(sample, LR, num.loops, sample.size, start, currentDate,
#        model.name, CV.correlation, BRT.perc.dev.expl, csvFileName, stats)


