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


## If I've already run & saved models and want to re-load, do so here:
# tobeloaded <- paste0(out.dir,"pipo_mods_2019-06-14.Rdata") ; sp <- c("pipo")
tobeloaded <- paste0(out.dir,"psme_mods_2019-06-14.Rdata") ; sp <- c("psme")
temp.env = new.env()
invisible(lapply(tobeloaded, load, envir = temp.env))
models = as.list(temp.env)
rm(temp.env, tobeloaded)

## If want to re-load stats.new from existing model run (for boxplot)
# stats.new <- read.csv(paste0(out.dir,"pipo_brt_stats_fin_relinf_ordered_2019-06-14.csv"))
stats.new <- read.csv(paste0(out.dir,"psme_brt_stats_fin_relinf_ordered_2019-06-14.csv"))
# stats.new[1] <- NULL # extra column gets added


## PIPO OR PSME?? RETAIN ONLY VARIABLES USED.
# data.brt <- data.pipo %>%
#   dplyr::select(regen_pipo, BALive_pipo_m, YEAR.DIFF, def.tc, tmax.tc, ppt.tc, CMD_CHNG,
#                 def59_z_max15, DUFF_DEPTH_cm, LITTER_DEPTH_cm, FIRE.SEV, REBURN) %>%
#   rename(regen_brt = regen_pipo,
#          BALive_brt_m = BALive_pipo_m) ; sp <- c("pipo")

data.brt <- data.psme %>%
  dplyr::select(regen_psme, BALive_psme_m, YEAR.DIFF, def.tc, tmax.tc, ppt.tc, CMD_CHNG,
                def59_z_max15, DUFF_DEPTH_cm, LITTER_DEPTH_cm, FIRE.SEV, REBURN) %>%
  rename(regen_brt = regen_psme,
         BALive_brt_m = BALive_psme_m) ; sp <- c("psme")




#################
### BRT SETUP ###   
#################



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
                       expression(paste("Live BA (m"^2,"ha"^-1,")")),
                       "Deficit (mm)",
                       expression(paste("Max temp (",degree*C,")")),
                       "Precip (mm)",
                       "Relative change in deficit",
                       "Deficit anomaly",
                       "Duff depth (cm)",
                       "Litter depth (cm)",
                       "Fire severity",
                       "Reburn")


## After iteratively dropping w/ 10 runs (below) to see which boosts AUC, re-define explan.vars

## PIPO
pipo.explan.vars <- explan.vars[-c(3, 5, 6, 8, 9, 10, 11)] # have iteratively removed vars.
# ^ This is the final dataset for PIPO, as removing others doesn't improve AUC
pipo.explan.vars.names <- explan.vars.names[-c(3, 5, 6, 8, 9, 10, 11)]
pipo.explan.vars
pipo.explan.vars.names

## PSME
psme.explan.vars <- explan.vars[-c(3,5,6,7,9,10,11)] # have iteratively removed vars.
# ^ This is the final dataset for PSME, as removing others doesn't improve AUC
psme.explan.vars.names <- explan.vars.names[-c(3,5,6,7,9,10,11)]
psme.explan.vars
psme.explan.vars.names

if (sp == "pipo") explan.vars <- pipo.explan.vars else explan.vars <- psme.explan.vars
if (sp == "pipo") explan.vars.names <- pipo.explan.vars.names else explan.vars.names <- psme.explan.vars.names


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
# Lines 159-160: for (v in 2:length(explan.vars)) 
# Lines 166-168: version <- paste0("x",explan.vars[v])
# Lines 174-175: gbm.x = explan.vars[-v],
# Lines 248-257 & 271-280: change csv names

start <- Sys.time() 
for (v in 1){ # if not iteratively dropping vars
# for (v in 2:length(explan.vars)){ # iteratively drops all vars (except YEAR.DIFF)
# for (v in 2:12){ # if poops out, complete from pt of pooping
  for (i in num.loops){
  # for (i in 26:100){ # if poops out, complete run from pt of pooping
    
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
                  n.trees=500, # starting number of trees -- good for pipo
                  # n.trees=1000 ,# starting number of trees -- good for psme
                  step.size=5, # iteratively add this number of trees
                  max.trees=2000, # max out at this number of trees -- good for pipo
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
# csvFileName <- paste0(sp,"_brt_stats_all_sum_", currentDate,".csv")
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

# pipo
# all.auc <- 0.783147713 # original with all variables
# all.perc.dev. <- 0.090818961 # original with all variables

# psme
# all.auc <- 0.848814314 # original with all variables
# all.perc.dev. <- 0.216967537 # original with all variables

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
# # ^ Says to drop DUFF which decreases auc by < 0.01: 0.7693645
# No DUFF:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.7693645
# ^ Next best drop would be def zmax, but drop is > 0.01: 0.753792
# So, final model includes def zmax

# Final model explan.vars for pipo
# explan.vars
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "def59_z_max15" 
# ^ AUC is 0.7693645
# 0.7693645 - all.auc # -0.01378321 # not substantial decline from all, either.


## Below is what I did for PSME variable selection; now complete.
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - all.auc
# # ^ drop reburn b/c improves auc by > 0: 0.8490268
# # No reburn:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8490268
# # ^ Says to drop DEF which decreases auc by < 0.01: 0.8483103
# # No def:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8483103
# # ^ drop FIRE.SEV b/c improves auc by > 0: 0.8484848
# # No fire sev:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8484848
# # ^ drop zmax which decreases auc by < 0.01: 0.8474562
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8474562
# # ^ drop CMD_CHMG which decreases auc by < 0.01: 0.8447604
# # No CMD_CHNG:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8447604
# # ^ drop litter which decreases auc but by < 0.01: 0.841895249
# # No litter:
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.841895249
# # ^ drop ppt which decreases auc by by < 0.01: 0.8362686
# # No ppt
# stats.sum[which.max(stats.sum$auc_fn1),]$auc_fn1 - 0.8362686
# # ^ Dropping DUFF gives next highest AUC, but that's a too-big decrease (-0.01557)
# # Retain DUFF in final model. How does w/ DUFF compare to all-var AUC?
# all.auc - 0.8207007 # 0.02811361

# Final vars for PSME
# explan.vars
# [1] "YEAR.DIFF"     "BALive_brt_m"  "tmax.tc"       "DUFF_DEPTH_cm"



### !!! BELOW ONLY WORKS WHEN NOT ITERATIVELY DROPPING VARS !!! ###
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
temp <- gather(temp, key = "var", value = "rel.inf")

# Var names in order of influence:
# pipo: yrs, anomaly, BA, tmax
pipo.var.names <- c("Years since fire",
                    "Deficit anomaly",
                    expression(paste("Live BA (m"^"2","ha"^"-1",")")),
                    expression(paste("Max temp (",degree*C,")")))
# psme: BA, yrs, duff, tmax
psme.var.names <- c(expression(paste("Live BA (m"^"2","ha"^"-1",")")),
                    "Years since fire",
                    "Duff depth (cm)",
                    expression(paste("Max temp (",degree*C,")")))
var.names <- ifelse(sp == "pipo", pipo.var.names, psme.var.names)
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


