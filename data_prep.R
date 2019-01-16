###############
## DATA PREP ##
###############

#####################################
## Load data
plot <- read.csv('SP_PLOT_data.csv')                  ## plot data
cond.tree.seed <- read.csv('SP_COND_TREE_SEED.csv')   ## seedling & tree data
climate <- read.csv('SP_ClimateLayers.csv')           ## ClimateWNA data
# prism.1971.2000 <- read.csv('SP_Prism_71_00.csv')   ## PRISM norms 1971-2000
# prism.1981.2010 <- read.csv('SP_Prism_81_10.csv')   ## PRISM norms 1981-2010
mtbs <- read.csv('SP_MTBS_CMBJoin.csv')               ## severity; CMB=climate monitoring branch (NOAA)
time.since.fire <- read.csv('SP_TimeSinceFire.csv')   ## time since fire; NIMS=Nat Info Mgmt System


## Quick peak: plot lat/long
plot(plot.data$LON_FS, plot.data$LAT_FS, pch=19, cex=0.1)
# CO, OR, not present; WY "sparse"
# How many pts each? (lu states from FIADB manual)
count(plot, STATECD)
# STATECD     n
# 1        4 19867 AZ
# 2        6    12 CA
# 3        8 16114 CO
# 4       16 11040 ID
# 5       30 22263 MT
# 6       32 14659 NV
# 7       35 15879 NM
# 8       38   177 ND
# 9       46    32 SD
# 10      49 15898 UT
# 11      53    13 WA
# 12      56  5381 WY


#####################################
## Confirm there are no dupe plots
plot %>% 
  group_by(CN) %>% 
  filter(n()>1) %>% # could stop here, or generate summary...
  summarize(n=n()) # to see that no, there are no dupes


## Drop field types that aren't forested
# FLDTYPCD assigned by crew; 3-dig code per type
# Also shouldn't have: 
# 980 Tropical hardwoods group
# 988 Cloud forest
# 990 Exotic hardwoods group
# 999 Nonstocked
any(cond.tree.seed$FLDTYPCD>979) # TRUE
cond.tree.seed %>% 
  filter(FLDTYPCD > 979) %>%
  group_by(FLDTYPCD) %>% 
  summarize(n=n()) # only one record -- delete it.
# Per manual, 995 = other exotic hardwoods. DELETE.
cond.tree.seed <- filter(cond.tree.seed, ! is.na(FLDTYPCD) | FLDTYPCD > 979)


## Choose plots where all 4 subplots are in same condition
# Sum of all condition proportions for a plot = 1.
# If CONDPROP_UNADJ < 1, that condition isn't the only one.
cond.tree.seed <-filter(cond.tree.seed, CONDPROP_UNADJ==1)
dim(cond.tree.seed) # 27540


#####################################
## Assign ID unique to plot AND yr; ID unqiue to plot, that's same across yrs.
attach(cond.tree.seed)
cond.tree.seed$UNIQUEID<-paste(PLOT,STATECD,COUNTYCD,INVYR,sep='_')
cond.tree.seed$PLOTID<-paste(PLOT,STATECD,COUNTYCD,sep='_')
detach(cond.tree.seed)
dim(cond.tree.seed) # 27540


#####################################
## Attach lat/long/elev
data <- merge(cond.tree.seed,plot[,c(1,10,11,12)], by.x ='PLT_CN',by.y='CN')
data <- merge(cond.tree.seed,
              plot[,c("CN", "LAT_FS", "LON_FS", "ELEV")],
              by.x ='PLT_CN', by.y='CN')
plot(data$LON_FS, data$LAT_FS, pch=19, cex=0.1)


## Scope revisited plots
data %>%
  group_by(PLOTID) %>%
  mutate(FREQ=n()) %>%
  filter(FREQ > 1) %>%
  with(plot(LON_FS, LAT_FS, # with b/c plot doesn't take arg of dataset
            pch=19, cex=0.1)) 
# WYO virtually excluded
