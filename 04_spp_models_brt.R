######################################################
### SETUP
######################################################

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo_2019-06-12.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_2019-06-12.csv") ; data.psme$X <- NULL

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

# Keep only candidate variables
data.pipo <- data.pipo %>%
  dplyr::select(regen_pipo, BALive_pipo_m, YEAR.DIFF, def.tc, tmax.tc, ppt.tc, CMD_CHNG,
                def59_z_max15, DUFF_DEPTH_cm, LITTER_DEPTH_cm, FIRE.SEV, REBURN)
head(data.pipo)                
                
data.psme <- data.psme %>%
  dplyr::select(regen_psme, BALive_psme_m, YEAR.DIFF, def.tc, tmax.tc, ppt.tc, CMD_CHNG,
                def59_z_max15, DUFF_DEPTH_cm, LITTER_DEPTH_cm, FIRE.SEV, REBURN)
head(data.psme)    


#################
### BRT SETUP ###   
#################

## PIPO OR PSME??
data.brt <- data.pipo %>%
  rename(regen_brt = regen_pipo,
         BALive_brt_m = BALive_pipo_m) ; sp <- c("pipo")

# data.brt <- data.psme %>%
#   rename(regen_brt = regen_psme,
#          BALive_brt_m = BALive_psme_m) ; sp <- c("psme")


## Set learning rate
# LR<-0.01
# LR<-0.02
# LR<-0.005
LR<-0.001

## Set tree complexity
TC <- 3
# TC <- 5
# TC <- 10

# How many iterations?
# num.loops <- 1
# num.loops <- 1:2
# num.loops <- 1:5
# num.loops <- 1:10
num.loops <- 1:100


## List vars (factors last so any iterative plot creation doesn't stop midway thru)
# Iteratively remove each var to explore improvement in AUC
explan.vars <- c("YEAR.DIFF",
          "BALive_brt_m",
          "def.tc",
          "tmax.tc",
          "ppt.tc",
          "CMD_CHNG",
          "def59_z_max15",
          "DUFF_DEPTH_cm",
          "LITTER_DEPTH_cm",
          "FIRE.SEV",
          "REBURN")

# Names (with expressions)
explan.vars.names <- c("Years since fire",
                       expression(paste("Live BA (m"^"2","ha"^"-1",")")),
                       "Deficit (mm)",
                       expression(paste("Max temp (",degree*C,")")),
                       "Precip (mm)",
                       "Relative change in deficit",
                       "Max deficit anomaly",
                       "Duff depth (cm)",
                       "Litter depth (cm)",
                       "Fire severity",
                       "Reburn")

# # Names (no expressions)
# explan.vars.names <- c("Years since fire",
#                        "Live BA (sq m per ha)",
#                        "Deficit (mm)",
#                        "Max temp (degrees C)",
#                        "Precip (mm)",
#                        "Relative change in deficit",
#                        "Max deficit anomaly",
#                        "Duff depth (cm)",
#                        "Litter depth (cm)",
#                        "Fire severity",
#                        "Reburn")
                       

## After iteratively dropping w/ 10 runs (below) to see which boosts AUC, re-define explan.vars

## PIPO
explan.vars <- explan.vars[-c(3, 5, 6, 9, 10, 11)] # have iteratively removed vars.
# ^ This is the final dataset for PIPO, as removing others doesn't improve AUC 
explan.vars.names <- explan.vars.names[-c(3, 5, 6, 9, 10, 11)]
explan.vars
explan.vars.names

## PSME
# explan.vars <- explan.vars[-c(3, 5, 6, 9, 10, 11)] # have iteratively removed vars.
# ^ This is the final dataset for PIPO, as removing others doesn't improve AUC 
explan.vars


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

start <- Sys.time() 
for (v in 1){ # if not iteratively dropping vars
# for (v in 2:length(explan.vars)){ # iteratively drops all vars (except YEAR.DIFF)
# for (v in 2:12){ # if poops out, complete from pt of pooping
  for (i in num.loops){
  # for (i in 87:100){ # if poops out, complete run from pt of pooping
    
  # If iteratively dropping var, include x in name to ID which has been left out.
  # version <- "allvars"
  # version <- paste0("x",explan.vars[v])
  version <- "fin"
    
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
                  n.trees=500, # starting number of trees
                  step.size=5, # iteratively add this number of trees
                  max.trees=1250, # max out at this number of trees
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

## What's AUC range? For all variables included, min-max auc = 0.0095.
# Therefore, drop vars that increase AUC by meager < 0.1 to have parsimonious model.
mean(auc) ; sd(auc) ; range(auc) ; max(auc) - min(auc)

# Save as csv -- options below are for runs with vars dropped iteratively
currentDate <- Sys.Date()
# csvFileName <- paste0(sp,"_brt_stats_all_", currentDate,".csv") # all vars in
# csvFileName <- paste0(sp,"_brt_stats_x1_", currentDate,".csv") # 1 var removed
# csvFileName <- paste0(sp,"_brt_stats_x2_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x3_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x4_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x5_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x6_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x7_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x8_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_fin_", currentDate,".csv")
csvFileName <- paste0(sp,"_brt_stats_fin_", currentDate,".csv")
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
# csvFileName <- paste0(sp,"_brt_stats_x4_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x5_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x6_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x7_sum_", currentDate,".csv")
# csvFileName <- paste0(sp,"_brt_stats_x8_sum_", currentDate,".csv")
csvFileName <- paste0(sp,"_brt_stats_fin_sum_", currentDate,".csv")
write.csv(stats.sum, paste0(out.dir,"/",csvFileName))


## See which variable drop maximizes AUC, doesn't kill % dev explained.
# What was auc and %dev of model that contained all vars?
all.auc <- 0.783147713
all.perc.dev. <- 0.090818961
# If increase from drop is >0, pursue drop. 
# If decrease from drop is <0.01, pursue drop for more parsinomious model.
# If decrease from drop is >0.01, do not pursue drop and retain dropped var.
stats.sum <- as.data.frame(stats.sum)
stats.sum[which.max(stats.sum$auc_fn1),]
stats.sum[which.max(stats.sum$auc_fn1),]$version
stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1
stats.sum[which.max(stats.sum$auc_fn1),]$brt.perc.dev.expl_fn1

## Below is what I did for PIPO variable selection; now complete.
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - all.auc
# # ^ drop reburn b/c improves auc by > 0: 0.784513927.
# # No reburn:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.784513927
# # ^ drop ppt.tc which decreases auc but by < 0.01: 0.783954759
# # No ppt.tc:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.783954759
# # ^ drop litter which decreases auc but by < 0.01: 0.7830532
# # No litter:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7830532
# # ^ drop CMD_CHNG which decreases auc but by < 0.01: 0.7801363
# # No CMD_CHNG:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7801363
# # ^ says to drop def which decreases auc by < 0.01: 0.7770176
# # No def: 
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7770176
# # ^ Says to drop DUFF which decreases auc by < 0.01: 0.77050475
# # BUT, that's > 0.01 LESS THAN the original all-var AUC:
# all.auc - 0.77050475 # 0.01264296
# # So stop, don't keep whittling away: retain DUFF.

all.auc - stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1


# Final model explan.vars 
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max15" "DUFF_DEPTH_cm"
# ^ AUC is 0.7751101; which is only 0.008 units less than all-var AUC. So better.


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

## Boxplot of relative influence; plot by median
# temp <- stats.new[,-(1:6)] # retain only cols with variables
# med <- apply(temp, MARGIN = 2, FUN = median, na.rm = TRUE)
# order <- order(med, decreasing = TRUE)
# par(cex.axis=0.75)
# boxplot(temp[,order]) # las = 2 would make labels vertical
# tiff(paste0(out.dir, sp, "_brt_stats_", version, "_relinf_", currentDate,".tif"))
# boxplot(temp[,order])
# dev.off()

# Gather data to make ggplot happy
temp <- stats.new[,-(1:6)] # retain only cols with variables
temp <- gather(temp[,order], key = "var", value = "rel.inf")
var.names <- c("Years since fire",
               "Max deficit anomaly",
               expression(paste("Live BA (m"^"2","ha"^"-1",")")),
               "Duff depth (cm)",
               expression(paste("Max temperature (",degree*C,")")))
# var.names <- c("YRS",
#                "CMD Z",
#                "BA",
#                "DUFF",
#                "TEMP")
               
# addline_format <- function(x,...){ # replaces spaces in x with new line.
#   gsub('\\s','\n',x)
# }

 
tiff(paste0(out.dir, sp, "_brt_stats_", version, "_relinf_", currentDate,".tif"),
            res = 300, width = 5, height = 4, units = "in")
rel.inf.plot <- ggplot(data = temp,
                       aes(x = reorder(var, -rel.inf, # put - before rel.inf if reverse
                                       FUN = median),
                           y = rel.inf))
rel.inf.plot + geom_boxplot() +
  labs(x = NULL, y = "Relative influence (%)") + 
  # scale_x_discrete(labels = addline_format(var.names)) +
  scale_x_discrete(labels = var.names) +
  scale_y_continuous() + 
  coord_flip() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_bw(base_size = 12)
  # theme_caitlin()
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
# N.b., if FIRE.SEV and REBURN (factors), -2 and use code at end of loop. 
# for (i in 1:(length(explan.vars)-2)){
for (i in 1:(length(explan.vars))){   
  
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
  
  
  ## Add 5th & 95th percentile values so we can no which range of predictor var to trust
  (quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.05, 0.95)))
  abline(v = quant[1], lty = 2, lwd = 2, col = "red")
  abline(v = quant[2], lty = 2, lwd = 2, col = "red")
  

  dev.off()
  
  
  ##############################################################################
  ## Alternative with ggplot (no histogram)
  
  ## Tidy data into single dataframe so can add multiple curves onto same plot)
  predictor.cols <- do.call(cbind, predictors) # each col reps a model -- VALS ALL IDENTICAL
  response.cols <- do.call(cbind, responses) # each col reps a model
  pred.df <- as.data.frame(cbind(predictor.cols[,1], response.cols))
  colnames(pred.df) <- c("x", paste0("y", 1:length(models)))
  
  # Gather into long-form with diff rows for each predictor and each model
  pred.df.long <- tidyr::gather(pred.df, key = "model", value = "y", -"x")
  
  # Compute median and upper/lower bounds
  pred.df.long <- pred.df.long %>%
    group_by(x) %>%
    summarise(mean.y = mean(y),
              # median.y = quantile(y, probs = 0.50),
              lower.y = quantile(y, probs = 0.025),
              upper.y= quantile(y, probs = 0.975))
  
  # Create plotting object for 95% bounds (else adding ribbon is choppy)
  g1 <- ggplot(pred.df.long) + 
    stat_smooth(aes(x = x, y = lower.y), method = "loess", span = 0.25, se = FALSE) +
    stat_smooth(aes(x = x, y = upper.y), method = "loess", span = 0.25, se = FALSE)
  
  # Build plot object for rendering 
  gg1 <- ggplot_build(g1)
  
  # Extract data for the loess lines from the 'data' slot
  df.up.low <- data.frame(x = gg1$data[[1]]$x,
                    ymin = gg1$data[[1]]$y,
                    ymax = gg1$data[[2]]$y)
  
  ## Add 5th & 95th percentile values so we can no which range of predictor var to trust
  (quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.05, 0.95)))
  
  # Save
  # tiff(paste0(plot.dir, "/", explan.vars[[i]], "_alt.tiff"))
  
  # Create partial curve (orig data) and add in new ribbon data
  plot <- ggplot() +
    geom_smooth(data = pred.df.long, aes(x = x, y = mean.y), span = 0.25, se = FALSE, col = "black") + 
    geom_ribbon(data = df.up.low, aes(x = x, ymin = ymin, ymax = ymax),
                alpha = 0.5, fill = "light grey") +
    geom_vline(xintercept = quant[1], lty = 2, lwd = 1, col = "red") +
    geom_vline(xintercept = quant[2], lty = 2, lwd = 1, col = "red") +
    scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
                                max(data.brt[,explan.vars[i]]))) +
    scale_y_continuous(expand=c(0,0), limits=c(0.15,0.31)) +
    expand_limits(x = 0) + 
    labs(x = paste0(explan.vars.names[i]),
         y = "Probability of juvenile presence") +
    # coord_cartesian(xlim=c(min(explan.vars[i]),max(explan.vars[i])), ylim=c(0.15,0.45)) +
    theme_bw(base_size = 18) +
    theme(panel.grid.minor = element_blank())
  plot
  # Save
  tiff(paste0(plot.dir, "/", explan.vars[[i]], "_alt.tiff"))
  print(plot) # When using ggplot in for loop, need to print.
  dev.off()

  
  ##############################################################################
  ## 2nd alternative to overlay histgram
  
  
  # Create partial curve (orig data) and add in new ribbon data
  plot <- ggplot() +
    geom_smooth(data = pred.df.long, aes(x = x, y = mean.y), span = 0.25, se = FALSE, col = "black") + 
    geom_ribbon(data = df.up.low, aes(x = x, ymin = ymin, ymax = ymax),
                alpha = 0.5, fill = "light grey") +
    geom_vline(xintercept = quant[1], lty = 2, lwd = 1, col = "red") +
    geom_vline(xintercept = quant[2], lty = 2, lwd = 1, col = "red") +
    
    # geom_rug(data = data.brt, aes(x = explan.vars[i]), sides = "b") + 
    
    scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
                                max(data.brt[,explan.vars[i]]))) +
    scale_y_continuous(expand=c(0,0), limits=c(0.15,0.31)) + #else ribbon for yr diff cut-off
    expand_limits(x = 0) + 
    # labs(x = paste0(explan.vars.names[i]),
    labs(x = NULL,
         y = "Probability of juvenile presence") +
    coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]),
                           max(data.brt[,explan.vars[i]]))) +
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin=unit(c(1,1,-0.5,1), "cm")) # shrink margins for adjacency
  
  hist <- ggplot(data = data.brt, aes(x = data.brt[,explan.vars[i]])) +
    # geom_histogram(binwidth = 0.5) +
    geom_histogram(bins = 30) +
    geom_vline(xintercept = quant[1], lty = 2, lwd = 1, col = "red") +
    geom_vline(xintercept = quant[2], lty = 2, lwd = 1, col = "red") +
    # scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
    #                             max(data.brt[,explan.vars[i]]))) +
    coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]),
                           max(data.brt[,explan.vars[i]]))) +
    labs(x = paste0(explan.vars.names[i]),
         # y = "Frequency") +
         # y = NULL) +
         y = "   ") + # else not aligned
    theme_bw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin=unit(c(-0.25,1,1,1), "cm")) # shrink margins for adjacency
  
  ## Specifying width
  # Ref: https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange
  
  plots <- list(plot, hist)
  grobs <- list()
  widths <- list()
  
  
  # Collect the widths for each grob of each plot
  for (l in 1:length(plots)){
    grobs[[l]] <- ggplotGrob(plots[[l]])
    widths[[l]] <- grobs[[l]]$widths[2:5]
  }
  
  # Use do.call to get the max width
  maxwidth <- do.call(grid::unit.pmax, widths)
  
  # Assign the max width to each grob
  for (l in 1:length(grobs)){
    grobs[[l]]$widths[2:5] <- as.list(maxwidth)
    
  }
  
  # Plot
  tiff(paste0(plot.dir, "/", explan.vars[[i]], "_alt2.tiff"))
  # do.call("grid.arrange", c(grobs, ncol = 1))
  grid.arrange(grobs = grobs, ncol = 1, heights = c(3,1))
  dev.off()
} 






  #############################
#############################
#### FIX THIS HOT GARB!! ####
#############################
#############################

# ## Same deal except for factors -- figure out why central val = 1 added back in!!
# # FIRE.SEV
# for(j in 1:length(models)){
#   gbm.mod<-models[[j]]
#   r1 <- gbm::plot.gbm(gbm.mod, i.var = "FIRE.SEV", type = "response", return.grid = TRUE)
#   predictors[[j]]<-r1[,1]
#   responses[[j]]<-r1[,2]
# }
# # currentDate <- Sys.Date()
# tiff(paste0(plot.dir, "/FIRE.SEV.tif"))
# 
# # Get limits for plotting
# ymin=min(unlist(responses))
# ymax=max(unlist(responses))
# # xmin=min(unlist(predictors))
# # xmax=max(unlist(predictors))
# 
# # Set predictors to num (FIRE.SEV factor levels, which correspond with sev classes).
# # Jitter pts in x and y else looks sparse.
# # Add distribution (hist) of FIRE.SEV values
# j <- 1
# par(mar=c(5.5,5.1,4.1,2.1))
# plot(responses[[j]] ~ as.numeric(predictors[[j]]), xlab ="", ylab = "", xaxt='n',yaxt='n')
# ## Add histogram to show predictor distribution. Make big top margin so bars are tiny.
# par(new = TRUE, mar=c(5.5,5.1,25.1,2.1)) # 12.1 gives top margin
# hist(as.numeric(data.brt$FIRE.SEV),
#      xlab = NULL, ylab = NULL, axes = FALSE, main = NULL,
#      col = "light grey")
# ## Replot j = 1 b/c hist covered it
# par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
# plot(responses[[j]] ~ as.numeric(predictors[[j]]), xlab ="", ylab = "", xaxt='n',yaxt='n')
# ## Proceed with subsequent models til second to last
# for(j in 2:(length(models)-1)){
#   par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
#   plot(jitter(responses[[j]], 0.25) ~ jitter(as.numeric(predictors[[j]]), factor = 0.25), xlab ="", xaxt='n',yaxt='n', ylab = "")
# }
# ## Final model
#   j <- length(models)
#   par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
#   plot(jitter(responses[[j]], 0.25) ~ jitter(as.numeric(predictors[[j]]), 0.25), xlab ="Fire severity", ylab = "Prob. of juv. presence", ylim=c(ymin,ymax), xlim=c(xmin,xmax), main="", font.lab=1, font.axis=1, cex.lab=1.8, cex.axis=1.5)
# dev.off()
# 
# 
# # REBURN
# for(j in 1:length(models)){
#   gbm.mod<-models[[j]]
#   r1 <- gbm::plot.gbm(gbm.mod, i.var = "REBURN", type = "response", return.grid = TRUE)
#   predictors[[j]]<-as.numeric(r1[,1])
#   responses[[j]]<-r1[,2]
# }
# # currentDate <- Sys.Date()
# tiff(paste0(plot.dir, "/REBURN"))
# 
# # Get limits for plotting
# ymin=min(unlist(responses))
# ymax=max(unlist(responses))
# # xmin=min(unlist(predictors))
# # xmax=max(unlist(predictors))
# 
# # Set predictors to num (FIRE.SEV factor levels, which correspond with sev classes).
# # Jitter pts in x and y else looks sparse.
# # Add distribution (hist) of REBURN values
# j <- 1
# par(mar=c(5.5,5.1,4.1,2.1))
# plot(responses[[j]] ~ predictors[[j]], xlab ="", ylab = "", xaxt='n',yaxt='n')
# ## Add histogram to show predictor distribution. Make big top margin so bars are tiny.
# par(new = TRUE, mar=c(5.5,5.1,12.1,2.1)) # 12.1 gives top margin
# hist(as.numeric(data.brt$REBURN),
#      xlab = NULL, ylab = NULL, axes = FALSE, main = NULL,
#      col = "light grey")
# ## Replot j = 1 b/c hist covered it
# par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
# plot(responses[[j]] ~ predictors[[j]], xlab ="", ylab = "", xaxt='n',yaxt='n')
# ## Proceed with subsequent models til second to last
# for(j in 2:(length(models)-1)){
#   par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
#   plot(jitter(responses[[j]], 0.25) ~ predictors[[j]], xlab ="", xaxt='n',yaxt='n', ylab = "")
# }
# ## Final model
# j <- length(models)
# par(new=TRUE, mar=c(5.5,5.1,4.1,2.1))
# plot(jitter(responses[[j]], 0.25) ~ predictors[[j]], xlab ="Reburn", ylab = "Prob. of juv. presence", ylim=c(ymin,ymax), xlim=c(xmin,xmax), main="", font.lab=1, font.axis=1, cex.lab=1.8, cex.axis=1.5)
# dev.off()


#########################
#########################
#### END OF HOT GARB ####
#########################
#########################




############################################
## PDP FOR YEARS.DIFF -- OTHER VAR LEVELS ##
############################################

par(mfrow=c(1,1))
## Which var am I varying? # Change in newdata mutate (pre-loop) & newdata transform (in loop) below
# Only need to do this for final variables retained
explan.vars
# var <- "BALive_brt_m"
# var <- "BALiveTot_m"
# var <- "def.tc"
# var <- "tmax.tc"
# var <- "ppt.tc"
# var <- "def59_z_max15"
var <- "DUFF_DEPTH_cm"
# var <- "LITTER_DEPTH_cm" 
# var <- "FIRE.SEV"
# var <- "REBURN"


# Only looking at YEAR.DIFF here
i <- 1 # explan.vars[i] still exists in loop below & 1 = YEAR.DIFF

## Create list for dumping predictors (here, all YEAR.DIFF) & responses x 10 models
predictors<-list(rep(NA,length(models))) 
responses<-list(rep(NA,length(models)))

# For ggplot
all.quants.predictors <- list(rep(NA,length(models)*3))
all.quants.responses <- list(rep(NA,length(models)*3))
quantile <- list(rep(NA,length(models)*3))


## Create folder for plots
currentDate <- Sys.Date()
dir.create(paste0(out.dir, sp,"_yr.diff_pdp_by_",var,"_", currentDate))
plot.dir <- paste0(out.dir, sp,"_yr.diff_pdp_by_",var,"_", currentDate)

## For predictions, create 100 new YEAR.DIFF values.
year.new <- seq(from = min(data.brt$YEAR.DIFF), to = max(data.brt$YEAR.DIFF), by = 0.25)

## Set values of all other vars (exept that of interest) to mean; add on year.new.
print(var)
newdata <- data.brt %>% # Create new data with all but YEAR.DIFF & BALive_brt
  mutate(
         BALive_brt_m = mean(BALive_brt_m), # turn on/off var that's selected above
         BALiveTot_m = mean(BALiveTot_m),
         def.tc = mean(def.tc),
         tmax.tc = mean(tmax.tc),
         ppt.tc = mean(ppt.tc),
         CMD_CHNG = mean(CMD_CHNG),
         def59_z_max15 = mean(def59_z_max15),
         # DUFF_DEPTH_cm = mean(DUFF_DEPTH_cm),
         LITTER_DEPTH_cm = mean(LITTER_DEPTH_cm),
         FIRE.SEV = Mode(FIRE.SEV), # homegrown function
         REBURN = Mode(REBURN)
         )
newdata <- newdata[1:length(year.new),] # keep just enough (of all the same vals) for new years.
newdata$YEAR.DIFF <- year.new
levels(newdata$REBURN)
NY <- levels(newdata$REBURN) # To call REBURN levels without redefining var, create YN look-up


# Create new data to predict with (*n mods) for each quantile (q); predict with q*newdata
# Tried (0, 25, 50, 75, 100) but 0 and 100 are too extreme. Stick with 10%, 50%, 90%
for (q in 1:3){ # Pick quantiles BUT MUST SPECIFY IN PROBS (10, 50 90)
probs <- c(0.1, 0.5, 0.9)
# for (q in 1:4){ # For 4 levels of FIRE.SEV; convenient: factor num = sev; chng tiff name below, too.
# for (q in 1:2){ # For 2 levels of REBURN; chng tiff name below, too.
  # Loop through each of the j models
  for(j in 1:length(models)){
    gbm.mod<-models[[j]]
    r1 <- predict(gbm.mod,
                  # newdata = transform(newdata, BALive_brt_m = quantile(BALive_brt_m, probs = probs)[q]),
                  # newdata = transform(newdata, BALiveTot_m = quantile(BALiveTot_m, probs = probs)[q]),
                  # newdata = transform(newdata, def.tc = quantile(def.tc, probs = probs)[q]),
                  # newdata = transform(newdata, tmax.tc = quantile(tmax.tc, probs = probs)[q]),
                  # newdata = transform(newdata, ppt.tc = quantile(ppt.tc, probs = probs)[q]),
                  # newdata = transform(newdata, def59_z_max15 = quantile(def59_z_max15, probs = probs)[q]),
                  newdata = transform(newdata, DUFF_DEPTH_cm = quantile(DUFF_DEPTH_cm, probs = probs)[q]),
                  # newdata = transform(newdata, LITTER_DEPTH_cm = quantile(LITTER_DEPTH_cm, probs = probs)[q]),
                  # newdata = transform(newdata, FIRE.SEV = as.factor(q)), 
                  # newdata = transform(newdata, REBURN = as.factor(NY[q])),
                  n.trees = gbm.mod$n.trees,
                  type = "response",
                  se.fit = TRUE)
    predictors[[j]] <- newdata$YEAR.DIFF 
    responses[[j]] <- r1
  }
all.quants.predictors[[q]] <- predictors
all.quants.responses[[q]] <- responses
quantile[[q]] <- rep(q, length(models)) # Capture quantile num for ALL models

  # currentDate <- Sys.Date()
  tiff(paste0(plot.dir, "/yr.diff_pdp_by_",var,"_q",q,"_", quantile(data.brt[,var], probs = probs)[q], ".tif"))
  # tiff(paste0(plot.dir, "/yr.diff_pdp_by_",var,"_", q, ".tif"))
  # tiff(paste0(plot.dir, "/yr.diff_pdp_by_",var,"_", YN[q], ".tif"))
  
  # ymin=min(unlist(responses))
  # ymax=max(unlist(responses))
  xmin=min(unlist(predictors))
  xmax=max(unlist(predictors))
  ymin = 0.0
  ymax = 0.5
  
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
  
  
  ## Add 5th & 95th percentile values so we can know which bounds to trust (here, it's just YEAR.DIFF)
  (quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.05, 0.95)))
  abline(v = quant[1], lty = 2, lwd = 2, col = "red")
  abline(v = quant[2], lty = 2, lwd = 2, col = "red")
  
  dev.off()
}  



###########################################################################
###########################################################################
## ALT WITH GGPLOT
###########################################################################
###########################################################################

# These are lists (of n models) within lists (of q quants)
length(all.quants.responses) # 3
all.quants.responses[[1]] # has 10 lists full of response values
all.quants.responses[[2]] # has 10 lists full of response values
all.quants.responses[[3]] # has 10 lists full of response values
all.quants.responses[[4]] # does not exist b/c only q=3 quants

# Combine into one list, with each element containing responses
pred.temp1 <- do.call(cbind, all.quants.predictors)
resp.temp1 <- do.call(cbind, all.quants.responses)
quant.temp <- do.call(cbind, quantile)
quant.temp <-c(quant.temp[,1:3]) # Gives quants repeated n models times

length(resp.temp1) # 30
length(quant.temp) # 30
resp.temp1[[1]] # gives response values for q1, model 1
resp.temp1[[11]] # gives response values for q3, model 1
resp.temp1[[21]] # gives response values for q3, model 1

# Combine further into matrix
pred.temp2 <- do.call(cbind, pred.temp1) # each col reps a model (of 10) for each quant (3) - VALS IDENTICAL B/C PREDICTOR VALUES ALL THE SAME
resp.temp2 <- do.call(cbind, resp.temp1) # each col reps a model (of 10) for each quant (3) - VALS DIFF ACROSS MODELS AND QUANTS
nrow(pred.temp2) # 117 for each predicted response
ncol(pred.temp2) # 30 for each model (3 quants * 10 models each)

# Pull into dataframe (repeat all predictor values which are identical)
pred.df <- as.data.frame(cbind(pred.temp2[,1], resp.temp2))
# Name with quants (1-3) and each model (1-10 or 1-100)
colnames(pred.df) <- c("x", paste0("y", quant.temp, "mod",1:length(models)))
                                  
# Gather into long-form with diff rows for each predictor and each model
pred.df.long <- tidyr::gather(pred.df, key = "model", value = "y", -"x")

# Add col to specify quant (not best way but whatevs)
pred.df.long$q <- paste0("q", mid(pred.df.long$model,2,1))

# Compute median and upper/lower bounds
pred.df.long <- pred.df.long %>%
  group_by(x, q) %>%
  summarise(mean.y = mean(y),
            # median.y = quantile(y, probs = 0.50),
            lower.y = quantile(y, probs = 0.025),
            upper.y= quantile(y, probs = 0.975))

# Create plotting object for 95% bounds (else adding ribbon is choppy)
g1 <- ggplot(pred.df.long, aes(group=q)) + 
  stat_smooth(aes(x = x, y = lower.y), method = "loess", span = 0.25, se = FALSE) +
  stat_smooth(aes(x = x, y = upper.y), method = "loess", span = 0.25, se = FALSE)

# plot(g1)

# Build plot object for rendering 
gg1 <- ggplot_build(g1)

# Extract data for the loess lines from the 'data' slot
df.up.low <- data.frame(x = gg1$data[[1]]$x,
                        ymin = gg1$data[[1]]$y,
                        ymax = gg1$data[[2]]$y,
                        q = gg1$data[[1]]$group)

## Add 5th & 95th percentile values so we can no which range of predictor var to trust
(quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.05, 0.95)))



# Create partial curve (orig data) and add in new ribbon data
plot <- ggplot() +
  geom_smooth(data = pred.df.long,
              aes(x = x, y = mean.y, color = q),
              span = 0.25,
              se = FALSE) +
              # position=position_jitter(w=0.1, h=0.00)) +
  scale_color_manual(values = palette[3:5],
                     name = "Duff depth (cm)",
                     # name = expression(paste("Live BA (m"^"2","ha"^"-1",")")),
                     # name = expression(paste("Max temp (",degree*C,")")),
                     # name = "Max deficit anomaly",
                     labels = c(expression(paste("10"^"th"," percentile")),
                                expression(paste("50"^"th"," percentile")),
                                expression(paste("90"^"th"," percentile")))) +
  geom_ribbon(data = df.up.low,
              aes(x = x, ymin = ymin, ymax = ymax,
                  group = q, fill = factor(df.up.low$q)),
              alpha = 0.15,
              # position=position_jitter(w=0.0, h=0.01),
              show.legend = FALSE) +
  scale_fill_manual(values = palette[3:5]) +
  geom_vline(xintercept = quant[1], lty = 2, lwd = 1, col = "red") + #cbPalette[7]) +
  geom_vline(xintercept = quant[2], lty = 2, lwd = 1, col = "red") + #cbPalette[7]) +
  # scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
  #                             max(data.brt[,explan.vars[i]]))) +
  scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.14,0.38)) +
  expand_limits(x = 0) + 
  labs(x = paste0(explan.vars.names[i]),
       y = "Probability of juvenile presence") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.justification=c(1,0), # defines which side of legend .position coords refer to
        legend.position=c(1,0),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
        
plot
# Save
tiff(paste0(plot.dir, "/yr.diff_pdp_by_",var,"_all_qs.tif"))
# tiff(paste0(plot.dir, "/", explan.vars[[i]], "_alt.tiff"))
print(plot) # When using ggplot in for loop, need to print.
dev.off()










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


