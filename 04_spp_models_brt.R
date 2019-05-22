######################################################
### SETUP
######################################################

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo_2019-05-14.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_2019-05-14.csv") ; data.psme$X <- NULL

## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]

## Set variable classes
# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) 
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))

data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
# data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV) 
data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
# data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV, ordered = TRUE)

data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
# data.pipo$REBURN <- as.numeric(data.pipo$REBURN) 
data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
# data.psme$REBURN <- as.numeric(data.psme$REBURN) 


# Create CMD relative chng (from observed z-score slopes)
data.pipo$CMD_CHNG <- data.pipo$def59.z.slope
data.psme$CMD_CHNG <- data.psme$def59.z.slope

# Create proportion BA
data.pipo$BAProp_pipo <- data.pipo$BALive_pipo/data.pipo$BALiveTot
data.psme$BAProp_psme <- data.psme$BALive_psme/data.psme$BALiveTot



#################
### BRT SETUP ###   
#################

## PIPO OR PSME??
data.brt <- data.pipo %>%
  rename(regen_brt = regen_pipo,
         BALive_brt = BALive_pipo) ; sp <- c("pipo")

# data.brt <- data.psme %>%
#   rename(regen_brt = regen_psme,
#          BALive_brt = BALive_psme) ; sp <- c("psme")


## Set learning rate
# if (sample.size < 1500){ LR<-0.002}
# if (sample.size > 4500){ LR<-0.01}
# LR<-0.01
# LR<-0.02
# LR<-0.005
LR<-0.001
# LR<-0.0005
# LR<-0.0001

## Set tree complexity
TC <- 3
# TC <- 5
# TC <- 10

# How many bootstraps?
# num.loops <- 1
# num.loops <- 1:2
# num.loops <- 1:5
num.loops <- 1:10
# num.loops <- 1:20


## List vars (put factors last so plot creation doesn't stop midway thru)
# Iteratively remove each var to explore improvement in AUC

explan.vars <- c("YEAR.DIFF",
          "BALive_brt",
          "BALiveTot",
          "def.tc",
          "tmax.tc",
          "ppt.tc",
          "CMD_CHNG",
          "def59_z_max15",
          "DUFF_DEPTH",
          "LITTER_DEPTH",
          "FIRE.SEV",
          "REBURN")

## After iteratively dropping (below) to see which boosts AUC, re-define explan.vars
# explan.vars <- explan.vars[-10] # remove LITTER_DEPTH (biggest increase in auc)
# explan.vars <- explan.vars[-c(7,10)] # remove CMD_CHNG, TOO
# ^ This is the final dataset, as removing others doesn't improve AUC 

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

### !!! IF NOT ITERATIVELY DROPPING, CHNG:
# 1) for (v in 2:length(explan.vars)) 
# 2) version <- paste0("x",explan.vars[v])
# 3) gbm.x = explan.vars[-v],

# nb looping auc code takes lots longer than running brt_CV.R after.
# If I do after, see code_ref for reading in indivd csvs and bind_rows to compare 

start <- Sys.time() 
# for (v in 1){ # if not iteratively dropping vars
for (v in 2:length(explan.vars)){ # iteratively drops all vars (except YEAR.DIFF)
# for (v in 9:12){ # if poops out, complete from pt of pooping
  for (i in num.loops){
  # for (i in 10){ # if poops out, complete run from pt of pooping
    
  # If iteratively dropping var, activate x to ID which has been left out.
  # Also change gbm.x in formula below!
  version <- "allvars"
  # version <- paste0("x",explan.vars[v])
  # version <- "fin"
    
  # Pull random sample; if defined sample.size above
  # sample <- data.brt[which(data.brt$UNIQUEID %in% sample(data.brt$UNIQUEID, sample.size)), ]
  # Or use all
  data <- data.brt
  
  # Loop through model creation i times
  models[[i]] <-gbm.step(data=data, 
                  # gbm.x = explan.vars,
                  gbm.x = explan.vars[-v],
                  gbm.y = "regen_brt",          
                  family = "bernoulli", 
                  tree.complexity = TC, # number of nodes in a tree
                  learning.rate = LR, 
                  bag.fraction = 0.5, # pretty universally used
                  n.trees=500, # starting number of trees
                  step.size=5, # iteratively add this number of trees
                  max.trees=1250, # max out at this number of trees
                  verbose=TRUE) 
  ## Grab info/stats
  # Model name
  model.name.temp <- paste0(sp,".",version,".",i)
  # Percent deviance explained
  brt.perc.dev.expl.temp <- 1-(models[[i]]$self.statistics$mean.resid/models[[i]]$self.statistics$mean.null)
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

# calculate my own auc; get vector cv.auc.by.hand
# source("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/fia-regen/04b_spp_models_brt_CV.R")

# How many vars? If all, retain first.
num.vars <- ifelse((version == "allvars" | version == "fin"), length(explan.vars), length(explan.vars)-1)

# Size of matrix will adjust depending on number of explanatory variables
var.mat <- matrix(unlist(var), ncol = num.vars, byrow = TRUE) 
rel.inf.mat <- matrix(unlist(as.numeric(rel.inf)), ncol = num.vars, byrow = TRUE) 

stats <- cbind.data.frame(model.name, cv.correlation, cv.discrim, auc, brt.perc.dev.expl,
                 trees, var.mat, rel.inf.mat)
colnames <- c("model.name", "cv.correlation", "cv.discrim", "auc", "brt.perc.dev.expl", "num.trees",
              paste0("var.", 1:num.vars), # create col names based on num vars.
              paste0("rel.inf.", 1:num.vars))
colnames(stats) <- colnames              

# What's var in auc values? min-max = 0.0095 -- maybe drop vars that decrease auc by meager <0.01.
mean(auc) ; sd(auc) ; range(auc) ; max(auc) - min(auc)

# Save as csv
currentDate <- Sys.Date()
# csvFileName <- paste0(sp,"_brt_stats_all_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x1_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x2_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x3_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_fin_", currentDate,".csv")
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
# csvFileName <- paste0(sp,"_brt_stats_x1_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x2_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x3_sum_", currentDate,".csv")
csvFileName <- paste0(sp,"_brt_stats_fin_sum_", currentDate,".csv")
# write.csv(stats.sum, paste0(out.dir,"/",csvFileName))

## w/o litter & cmd gives highest auc; removing others does not increase.
# Set that final model agian and proceed below.


### !!! BELOW ONLY WORKS WHEN NOT ITERATIVELY DROPPING VARS AS ABOVE !!! ###



######################################
## CREATE ORDERED STATS LIST BY VAR ##
######################################

# Create a list for storing re-named relative influence values. List b/c they'll be dataframes, technically.
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

# Boxplot of relative influence; plot by median
temp <- stats.new[,-(1:6)] # retain only cols with variables
med <- apply(temp, MARGIN = 2, FUN = median, na.rm = TRUE)
order <- order(med, decreasing = TRUE)
par(cex.axis=0.75)
boxplot(temp[,order], las=2)
# tiff(paste0(out.dir, sp, "_brt_stats_", version, "_relinf_", currentDate,".tif"))
boxplot(temp[,order], las=2)
dev.off()

# Write to new csv
currentDate <- Sys.Date()
write.csv(stats.new, paste0(out.dir,sp,"_brt_stats_", version, "_relinf_ordered_",  currentDate,".csv"))

# Get mean & sd of all stats and relative influences
stats.sum <- stats.new %>%
  dplyr::select(-model.name) %>%
  summarize_all(list(mean, sd)) # nb this will name _fn1 and _fn2, not _mean and _sd

# Write to new csv
currentDate <- Sys.Date()
write.csv(stats.sum, paste0(out.dir,sp,"_brt_stats_",version, "_relinf_sum_",currentDate,".csv"))



#############################################
## PARTIAL DEPENDENCE PLOTS EACH VAR KINDA ##
#############################################

## pdps account for average effect of other vars. BUT, plotmo isn't full shebang.
# plotmo(models[[i]], type = "response", pmethod = "partdep")
# ^ plotmo is "poor mans" pdp b/c holds other vals at median (or avg with pmethod = partdep).
# Even with pmethod = partdep, I think TRUE pdp would compute at all vals of other vars...
# ... and then average those predicted variables for maybe every val of var of interest?

# ref: https://stats.stackexchange.com/questions/122721/r-partial-dependency-plots-from-gbm-package-values-and-y-axis/122802


par(mfrow=c(1,1))

predictors<-list(rep(NA,length(models))) ## space for data: however many models are run
responses<-list(rep(NA,length(models)))

# Create folder for plots
currentDate <- Sys.Date()
dir.create(paste0(out.dir, sp,"_brt_plots_", currentDate))
plot.dir <- paste0(out.dir, sp,"_brt_plots_", currentDate)


## Loop through vars and overlay loess fit for each mod on one plot per var.
# N.b., FIRE.SEV and REBURN (factors) are at end and not here in loop (hence length - 2).
for (i in 1:(length(explan.vars)-2)){ 
  
  # Loop through the models and populate  lists of predictors & (marginal) responses.
  # With plot.gbm, other vars  "integrated out" -- not true pdp with mean effect of other vars.
  # With type = "response", automagically on scale of repsonse var (don't need to subtract mean)
  # Return.grid skips plot and gives grid of predictor vals and response vals

  for(j in 1:length(models)){
    gbm.mod<-models[[j]]
    r1 <- gbm::plot.gbm(gbm.mod, i.var = i, type = "response", return.grid = TRUE)
    predictors[[j]]<-r1[,1]
    responses[[j]]<-r1[,2]
  }
  
  # currentDate <- Sys.Date()
  tiff(paste0(plot.dir, "/", explan.vars[[i]], ".tif"))
  
  # Get limits for plotting
  ymin=min(unlist(responses))
  ymax=max(unlist(responses))
  xmin=min(unlist(predictors))
  xmax=max(unlist(predictors))

  ## Create first plot of first model (j = 1), then overlay next models.
  ## This will be dummy for getting plot set-up (so hist doesn't get too big)
  j <- 1
  par(mar=c(5.5,5.1,4.1,2.1))
  temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
  plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1.5, 
       xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
  
  
  ## Add histogram to show predictor distribution. Make big top margin so bars are tiny.
  par(new = TRUE, mar=c(5.5,5.1,25.1,2.1)) # 25 gives top margin
  hist(data.brt[,explan.vars[i]],
       xlab = NULL, ylab = NULL, axes = FALSE, main = NULL,
       col = "light grey")
  
  
  ## Re-do first plot of first model (j = 1), b/c hist covered it up, then overlay next models
  j <- 1
  par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
  temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
  plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1.5, 
       xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
  
  
  ## Overlay models (starting with number 2 til the second to last
  for(j in 2:(length(models)-1)){
    ## variable i, model j
    par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
    temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
    plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1.5,
         xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
  }
  
  
  ## create final overlay with last model; add labels here.
  ## variable i, j=last
  j <- length(models)
  par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
  temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
  plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1.5,
       ylab="Prob. of juv. presence", xlab=explan.vars[i], ylim=c(ymin,ymax),xlim=c(xmin,xmax),
       main="", font.lab=1, font.axis=1, cex.lab=1.8, cex.axis=1.5)
  
  
  ## Add 5th & 95th percentile values so we can no which bounds to trust
  (quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.05, 0.95)))
  abline(v = quant[1], lty = 2, lwd = 2, col = "red")
  abline(v = quant[2], lty = 2, lwd = 2, col = "red")
  
  
  ## Store the plots in a list
  myplots[[i]] <- recordPlot()
  
  dev.off()
}  

#############################
#############################
#### FIX THIS HOT GARB!! ####
#############################
#############################

## Same deal except for factors -- figure out why central val = 1 added back in!!
# FIRE.SEV
for(j in 1:length(models)){
  gbm.mod<-models[[j]]
  r1 <- gbm::plot.gbm(gbm.mod, i.var = "FIRE.SEV", type = "response", return.grid = TRUE)
  predictors[[j]]<-r1[,1]
  responses[[j]]<-r1[,2]
}
# currentDate <- Sys.Date()
tiff(paste0(plot.dir, "/FIRE.SEV.tif"))

# Get limits for plotting
ymin=min(unlist(responses))
ymax=max(unlist(responses))
# xmin=min(unlist(predictors))
# xmax=max(unlist(predictors))

# Set predictors to num (FIRE.SEV factor levels, which correspond with sev classes).
# Jitter pts in x and y else looks sparse.
# Add distribution (hist) of FIRE.SEV values
j <- 1
par(mar=c(5.5,5.1,4.1,2.1))
plot(responses[[j]] ~ as.numeric(predictors[[j]]), xlab ="", ylab = "", xaxt='n',yaxt='n')
## Add histogram to show predictor distribution. Make big top margin so bars are tiny.
par(new = TRUE, mar=c(5.5,5.1,25.1,2.1)) # 12.1 gives top margin
hist(as.numeric(data.brt$FIRE.SEV),
     xlab = NULL, ylab = NULL, axes = FALSE, main = NULL,
     col = "light grey")
## Replot j = 1 b/c hist covered it
par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
plot(responses[[j]] ~ as.numeric(predictors[[j]]), xlab ="", ylab = "", xaxt='n',yaxt='n')
## Proceed with subsequent models til second to last
for(j in 2:(length(models)-1)){
  par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
  plot(jitter(responses[[j]], 0.25) ~ jitter(as.numeric(predictors[[j]]), factor = 0.25), xlab ="", xaxt='n',yaxt='n', ylab = "")
}
## Final model
  j <- length(models)
  par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
  plot(jitter(responses[[j]], 0.25) ~ jitter(as.numeric(predictors[[j]]), 0.25), xlab ="Fire severity", ylab = "Prob. of juv. presence", ylim=c(ymin,ymax), xlim=c(xmin,xmax), main="", font.lab=1, font.axis=1, cex.lab=1.8, cex.axis=1.5)
dev.off()


# REBURN
for(j in 1:length(models)){
  gbm.mod<-models[[j]]
  r1 <- gbm::plot.gbm(gbm.mod, i.var = "REBURN", type = "response", return.grid = TRUE)
  predictors[[j]]<-as.numeric(r1[,1])
  responses[[j]]<-r1[,2]
}
# currentDate <- Sys.Date()
tiff(paste0(plot.dir, "/REBURN"))

# Get limits for plotting
ymin=min(unlist(responses))
ymax=max(unlist(responses))
# xmin=min(unlist(predictors))
# xmax=max(unlist(predictors))

# Set predictors to num (FIRE.SEV factor levels, which correspond with sev classes).
# Jitter pts in x and y else looks sparse.
# Add distribution (hist) of REBURN values
j <- 1
par(mar=c(5.5,5.1,4.1,2.1))
plot(responses[[j]] ~ predictors[[j]], xlab ="", ylab = "", xaxt='n',yaxt='n')
## Add histogram to show predictor distribution. Make big top margin so bars are tiny.
par(new = TRUE, mar=c(5.5,5.1,12.1,2.1)) # 12.1 gives top margin
hist(as.numeric(data.brt$REBURN),
     xlab = NULL, ylab = NULL, axes = FALSE, main = NULL,
     col = "light grey")
## Replot j = 1 b/c hist covered it
par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
plot(responses[[j]] ~ predictors[[j]], xlab ="", ylab = "", xaxt='n',yaxt='n')
## Proceed with subsequent models til second to last
for(j in 2:(length(models)-1)){
  par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
  plot(jitter(responses[[j]], 0.25) ~ predictors[[j]], xlab ="", xaxt='n',yaxt='n', ylab = "")
}
## Final model
j <- length(models)
par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
plot(jitter(responses[[j]], 0.25) ~ predictors[[j]], xlab ="Reburn", ylab = "Prob. of juv. presence", ylim=c(ymin,ymax), xlim=c(xmin,xmax), main="", font.lab=1, font.axis=1, cex.lab=1.8, cex.axis=1.5)
dev.off()


#########################
#########################
#### END OF HOT GARB ####
#########################
#########################



############################################
## PDP FOR YEARS.DIFF -- OTHER VAR LEVELS ##
############################################

par(mfrow=c(1,1))
# Which var am I varying? # Change in newdata mutate (pre-loop) & newdata transform (in loop) below
var <- "BALive_brt"
# var <- "BALiveTot"
# var <- "def.tc" 
# var <- "tmax.tc"
# var <- "ppt.tc" 
# var <- "def59_z_max15"
# var <- "DUFF_DEPTH" 
# var <- "FIRE.SEV"
# var <- "REBURN"


# Only looking at YEAR.DIFF here
i <- 1 # explan.vars[i] still exists in loop below & 1 = YEAR.DIFF

## Create list for dumping predictors (here, all YEAR.DIFF) & responses x 10 models
predictors<-list(rep(NA,length(models))) 
responses<-list(rep(NA,length(models)))

## Create folder for plots
currentDate <- Sys.Date()
dir.create(paste0(out.dir, sp,"_yr.diff_pdp_by_",var,"_", currentDate))
plot.dir <- paste0(out.dir, sp,"_yr.diff_pdp_by_",var,"_", currentDate)

## For predictions, create 100 new YEAR.DIFF values.
year.new <- seq(from = min(data.brt$YEAR.DIFF), to = max(data.brt$YEAR.DIFF), by = 0.25)

## Here, create new dataset with new vals for all but...
print(var)
newdata <- data.brt %>% # Create new data with all but YEAR.DIFF & BALive_brt
  mutate(
         # BALive_brt = mean(BALive_brt), # turn on/off
         BALiveTot = mean(BALiveTot),
         def.tc = mean(def.tc),
         tmax.tc = mean(tmax.tc),
         ppt.tc = mean(ppt.tc),
         def59_z_max15 = mean(def59_z_max15),
         DUFF_DEPTH = mean(DUFF_DEPTH),
         FIRE.SEV = mode(FIRE.SEV),
         REBURN = mode(REBURN)
         )
newdata <- newdata[1:length(year.new),] # keep just enough (of all the same vals) for new years.
newdata$YEAR.DIFF <- year.new
YN <- levels(newdata$REBURN) # To call REBURN levels without redefining var, create YN look-up


# Create new data to predict with (x 10 mods) for each of 5 quantiles; predict with 5 x new data
for (q in 1:5){ # Pick quantiles (0, 25, 50, 75, 100)
# for (q in 1:3){ # Pick quantiles (10, 50 90)
# for (q in 1:4){ # For 4 levels of FIRE.SEV; convenient: factor num = sev; chng tiff name below, too.
# for (q in 1:2){ # For 2 levels of REBURN; chng tiff name below, too.
  # Loop through each of the j models
  for(j in 1:length(models)){
    gbm.mod<-models[[j]]
    r1 <- predict(gbm.mod,
                  newdata = transform(newdata, BALive_brt = quantile(BALive_brt)[q]),
                  # newdata = transform(newdata, BALiveTot = quantile(BALiveTot)[q]),
                  # newdata = transform(newdata, def.tc = quantile(def.tc)[q]),
                  # newdata = transform(newdata, tmax.tc = quantile(data.brt[,var])[q]),
                  # newdata = transform(newdata, def.tc = quantile(ppt.tc)[q]),
                  # newdata = transform(newdata, def59_z_max15 = quantile(def59_z_max15)[q]),
                  # newdata = transform(newdata, DUFF_DEPTH = quantile(DUFF_DEPTH)[q]),
                  # newdata = transform(newdata, FIRE.SEV = as.factor(q)), 
                  # newdata = transform(newdata, REBURN = as.factor(YN[q])),
                  n.trees = gbm.mod$n.trees,
                  type = "response",
                  se.fit = TRUE)
    predictors[[j]] <- newdata$YEAR.DIFF 
    responses[[j]] <- r1
  }
  
  # currentDate <- Sys.Date()
  tiff(paste0(plot.dir, "/yr.diff_pdp_by_",var,"_q",q,"_", quantile(data.brt[,var])[q], ".tif"))
  # tiff(paste0(plot.dir, "/yr.diff_pdp_by_",var,"_", q, ".tif"))
  # tiff(paste0(plot.dir, "/yr.diff_pdp_by_",var,"_", YN[q], ".tif"))
  
  # ymin=min(unlist(responses))
  # ymax=max(unlist(responses))
  xmin=min(unlist(predictors))
  xmax=max(unlist(predictors))
  ymin = 0.15
  ymax = 0.40
  
  ## Create first plot of first model (j = 1), then overlay next models.
  ## This will be dummy for getting plot set-up (so hist doesn't get too big)
  j <- 1
  par(mar=c(5.5,5.1,4.1,2.1))
  temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
  plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1.5, 
       xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
  
  
  ## Add histogram to show predictor distribution. Make big top margin so bars are tiny.
  par(new = TRUE, mar=c(5.5,5.1,25.1,2.1)) # 25 gives top margin
  hist(data.brt[,explan.vars[i]],
       xlab = NULL, ylab = NULL, axes = FALSE, main = NULL,
       col = "light grey")
  
  ## Re-do first plot of first model (j = 1), b/c hist covered it up, then overlay next models
  j <- 1
  par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
  temp.lo <- loess(responses[[m]] ~ predictors[[m]], span = 0.5)
  plot(predictors[[m]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1.5, 
       xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
  
  
  ## Overlay models (starting with number 2 til the second to last
  for(j in 2:(length(models)-1)){
    ## variable i, model j
    par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
    temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
    plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1.5,
         xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
  }
  
  ## create final overlay with last model; add labels here.
  ## variable i, j=last
  j <- length(models)
  par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
  temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
  plot(predictors[[j]], fitted(temp.lo), col = "black", lty=1, type='l', lwd=1.5,
       ylab="Prob. of juv. presence", xlab=explan.vars[i], ylim=c(ymin,ymax),xlim=c(xmin,xmax),
       main="", font.lab=1, font.axis=1, cex.lab=1.8, cex.axis=1.5)
  
  
  ## Add 5th & 95th percentile values so we can know which bounds to trust
  (quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.05, 0.95)))
  abline(v = quant[1], lty = 2, lwd = 2, col = "red")
  abline(v = quant[2], lty = 2, lwd = 2, col = "red")
  
  dev.off()
}  




##########################################
### PREP TO AVERAGE ALL LINES TOGETHER ###   
##########################################

predictors<-list() 
responses<-list()
temp.lo <- list()
all.lo <- NULL

## Loop through the models and populate the lists of predictors and responses. 
# these are marginal effects of selected variables.
# Calculate the x and y limits for plotting.
# Adjust the response scale as in dismo partial plots function (subtract mean)

# Can't just call vars in order of how they appear in model, b/c each model has diff order. 
#So, specify which var to use as predictor in gbm::plot.gbm below 
for(i in 1:(length(explan.vars)-2)){
  for(j in 1:length(models)){
    # store evaluation points (explan.var values) and the marginal effects. return.grid gives values, no graphics.
    r1 <- gbm::plot.gbm(models[[j]], i.var = explan.vars[i], return.grid = TRUE) 
    predictors[[j]]<-r1[,1] # store the predictor values
    responses[[j]]<-r1[,2] - mean(r1[,2]) # store the predictor values, standardized by subtracting mean
    # run smoothing (local weighted scatter smooth = lowess aka loess)
    temp.lo[[j]] <- loess(responses[[j]] ~ predictors[[j]], span = 0.5)
    # store smoothing predictor values, predictED values, and which variable. "unname" drops extra col from x.
    df.lo <- data.frame(var = paste0(explan.vars[i]), x = unname(temp.lo[[j]]$x), y = temp.lo[[j]]$fitted)
    # iteratively add these individ var loess dfs to one that includes all variables.
    all.lo <- rbind(all.lo, df.lo)
  }
}

# Save these loess values for this type of drought so I can plot average across all droughts.
currentDate <- Sys.Date()
write.csv(all.lo, paste0(out.dir, sp,"_brt_lo.pred_z1_noELEV_noREBURN_noFIRESEV_", currentDate,".csv"))

# Once all drougth types are run, proceed with BRT_05_PlotVarInf_AllDroughts.R 









## Clean up
# remove(list = ls(pattern = "stats"), name, col.to, col.from, currentDate) 
# remove(var, var.mat1, var.mat2)
# remove(rel.inf, rel.inf.mat1, rel.inf.mat2)
# remove(list = ls(pattern = ".temp"))
# remove(sample, LR, num.loops, sample.size, start, currentDate,
#        model.name, CV.correlation, BRT.perc.dev.expl, csvFileName, stats)



#######################
### SAVE MODELS JIC ###   
#######################

# Save models JIC. save() needs names and will look to global envi unless I say look to list.
# names(models) <- paste0("model_", 1:length(num.loops)) # First assign names to each of the models.
# currentDate <- Sys.Date()
# Rdata.name <- paste0(scratchdir, "/", drought.type, "_noEVInoC_", currentDate,".Rdata")
# save(list=names(models),
#      file=paste0(Rdata.name),
#      envir=as.environment(models))
# load() # Insert that .Rdata here to reload models



