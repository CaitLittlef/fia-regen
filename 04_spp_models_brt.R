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

# data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV) # for partial dependence plots
# data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV, ordered = TRUE)

# data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
data.pipo$REBURN <- as.numeric(data.pipo$REBURN) # for partial dependence plots
# data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
data.psme$REBURN <- as.numeric(data.psme$REBURN) # for partial dependence plots
# data.psme$REBURN <- factor(data.psme$REBURN, ordered = TRUE)


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


## Set sample size.
# sample.size <- 50
# sample.size <- 75
# sample.size <- 100
# sample.size <- 150
# sample.size <- 200
# sample.size <- 250 # any smaller kept failing.

# sample.size <- 1000
# sample.size <- 10000
# sample.size <- 25000
# sample.size <- 50000

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
# TC <- 3
TC <- 5
# TC <- 10

# How many bootstraps?
# num.loops <- 1
# num.loops <- 1:5
num.loops <- 1:10
# num.loops <- 1:20


## List vars (put factors last so plot creation doesn't stop midway thru)
# Iteratively remove each var to explore improvement in AUC

explan.vars <- c("YEAR.DIFF", "BALive_brt", "BALiveTot",
          "def.tc",
          # "tmax.tc",
          "ppt.tc", "CMD_CHNG",
          "DUFF_DEPTH", "LITTER_DEPTH",
          # "def59_z_1",
          # "def59_z_12", "def59_z_13", "def59_z_14", "def59_z_15",
          # "ELEV",
          "def59_z_max15",
          "FIRE.SEV",
          "REBURN")

# version <- c("all")
# version <- c("xREBURN")
# version <- c("xREBURN_xFIRESEV")
# version <- c("xFIRESEV")
version <- c("xtmax.tc")

## Create empty list to store models in; create vectors to store 
models <- list()
model.name <- NULL
brt.perc.dev.expl <- NULL
cv.correlation <- NULL
cv.auc <- NULL
trees <- NULL
var <- list()
rel.inf <- list()

## WHAT SPECIES ARE YOU RUNNING?
print(sp)

################
### BRT LOOP ###   
################

start <- Sys.time()
for (i in num.loops){

  # Pull random sample
  # sample <- data.brt[which(data.brt$UNIQUEID %in% sample(data.brt$UNIQUEID, sample.size)), ]
  # Or use all
  sample <- data.brt
  
  models[[i]] <-gbm.step(data=sample, 
                  gbm.x = explan.vars, 
                  gbm.y = "regen_brt",          
                  family = "bernoulli", # for 0/1
                  tree.complexity = TC, # number of nodes in a tree
                  learning.rate = LR, 
                  bag.fraction = 0.5, # pretty universally used
                  n.trees=500, # start with this number of trees
                  step.size=5, # iteratively add this number of trees
                  max.trees=1000, # max out at this number of trees
                  verbose=TRUE) # show me what happens!
  ## Grab info/stats
  # Model name
  model.name.temp <- paste0(sp,i)
  # Percent deviance explained
  brt.perc.dev.expl.temp <- 1-(models[[i]]$self.statistics$mean.resid/models[[i]]$self.statistics$mean.null)
  # CV correlation
  cv.correlation.temp  <- models[[i]]$cv.statistics$correlation.mean
  # CV AUC
  cv.auc.temp  <- models[[i]]$cv.statistics$discrimination.mean
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
  cv.auc <- c(cv.auc, cv.auc.temp)
  trees <- c(trees, tree.temp )
  var <- c(var, var.temp)
  rel.inf <- c(rel.inf, rel.inf.temp)

}
print(Sys.time() - start)


#####################
### 1 MODEL PLOTS ###   
#####################

## Partial deviance of each variable
# par(mai=c(0.6,0.1,0.1,0.1), mfrow=c(2,2))
# gbm.plot(models[[i]], write.title=FALSE, y.label = "fitted function", smooth=TRUE, common.scale = FALSE) # marginal response curves
# dev.off()

# plotmo(models[[i]], type = "response", pmethod = "partdep")


#####################################################################
## COMPILE ALL MODEL PERFORMANCE STATS & VARIABLE INFLUENCE VALUES ##
#####################################################################

# calculate my own auc; get vector cv.auc.by.hand
source("C:/Users/clittlef/Google Drive/2RMRS/fia-regen/fia-regen/04b_spp_models_brt_CV.R")

# Size of matrix will adjust depending on number of explanatory variables
var.mat <- matrix(unlist(var), ncol = length(explan.vars), byrow = TRUE) 
rel.inf.mat <- matrix(unlist(as.numeric(rel.inf)), ncol = length(explan.vars), byrow = TRUE) 

stats <- cbind.data.frame(model.name, cv.correlation, cv.auc, as.numeric(cv.auc.by.hand), brt.perc.dev.expl,
                 trees, var.mat, rel.inf.mat)
colnames <- c("model.name", "cv.correlation", "cv.auc", "cv.auc.by.hand", "brt.perc.dev.expl", "num.trees",
              paste0("var.",1:(length(explan.vars))), # create col names based on num vars.
              paste0("rel.inf.",1:(length(explan.vars))))
colnames(stats) <- colnames              

### WHY ARE MY AUC VALUES DIFF THAN DISCRIM VALUE IN MODEL OUTPUT?? ###

# Save as csv
currentDate <- Sys.Date()
csvFileName <- paste0(sp,"_brt_stats_", version, "_", currentDate,".csv")
write.csv(stats, paste0(out.dir,"/",csvFileName))


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
for (i in rows){
  # Pull out one row at a time from stats table
  stats.row <- stats[i,] # Don't have to specify rows hereafter b/c there's only 1.
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
  stats.list[[i]] <- stats.row
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
tiff(paste0(out.dir, sp, "_brt_stats_ORDERED_RELINF_", version, "_", currentDate,".tif"))
boxplot(temp[,order], las=2)
dev.off()

# Write to new csv
currentDate <- Sys.Date()
write.csv(stats.new, paste0(out.dir,sp,"_brt_stats_ORDERED_", version, "_",  currentDate,".csv"))


# Get mean & sd of all stats and relative influences
stats.sum <- stats.new %>%
  dplyr::select(-model.name) %>%
  summarize_all(funs(mean, sd))

# Write to new csv
currentDate <- Sys.Date()
write.csv(stats.sum, paste0(out.dir,sp,"_brt_stats_SUMMARY_",version, "_",currentDate,".csv"))

# Clean up
# remove(list = ls(pattern = "stats"), name, col.to, col.from, currentDate) 
# remove(var, var.mat1, var.mat2)
# remove(rel.inf, rel.inf.mat1, rel.inf.mat2)
# remove(list = ls(pattern = ".temp"))
# remove(sample, LR, num.loops, sample.size, start, currentDate,
#        model.name, CV.correlation, BRT.perc.dev.expl, csvFileName, stats)



####################################
## OVERLAY ALL ITERATIONS OF BRTS ##
####################################

# ref: https://stats.stackexchange.com/questions/122721/r-partial-dependency-plots-from-gbm-package-values-and-y-axis/122802

#If factor variable
# predictors[[j]] <- factor(predictors[[j]],levels = levels(gbm.object$gbm.call$dataframe[,gbm.object$gbm.call$gbm.x[k]]))


par(mfrow=c(1,1))

predictors<-list(rep(NA,length(models))) ## space for data: however many models are run
responses<-list(rep(NA,length(models)))

# Create folder for plots
currentDate <- Sys.Date()
dir.create(paste0(out.dir, sp,"_brt_plots_", currentDate))
plot.dir <- paste0(out.dir, sp,"_brt_plots_", currentDate)

## Create a list to store the plots in
myplots <- list()

## Loop through the variables and overlay loess fit for each model on one plot per variable

for (i in 1:length(explan.vars)){ 
  
  ## Loop through the models and populate the lists of predictors and (marginal) responses.
  ## With plot.gbm, other vars are "integrated out" -- not true pdp with mean of other vars.
  ## Calc x & y lims for plotting.
  ## With type = "response", don't need to subtract mean to put on scale of response var.
  
  
  ## !!!!!!!!!!! BUT CONFIRM THIS ^ !!!!!!!!!!!
  
  
  
  for(j in 1:length(models)){
    gbm.mod<-models[[j]]
    # r1 <- plotmo(gbm.mod, nresponse = i, type = "response", pmethod = "partdep", return.grid = TRUE)
    r1 <- gbm::plot.gbm(gbm.mod, i.var = i, type = "response", return.grid = TRUE)
    # return.grid only gives eval pts & avg predictions. no graphics. c.f. gbm.plot
    predictors[[j]]<-r1[,1]
    responses[[j]]<-r1[,2]# - mean(r1[,2])
  }
  
  # currentDate <- Sys.Date()
  tiff(paste0(plot.dir, "/", explan.vars[[i]], ".tif"))
  # pdf(paste0(plot.dir, "/", explan.vars[[i]], ".pdf"))
  # par(mar=c(5.5,5.1,4.1,2.1))
  
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

