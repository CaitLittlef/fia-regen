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

## Alt: 
plot %>%
  group_by(STATECD) %>%
  summarize(n=n())


## Confirm there are no dupe plots
plot %>% 
  group_by(CN) %>% 
  filter(n()>1) %>% # could stop here, or generate summary...
  summarize(n=n()) # to see that no, there are no dupes


## Assign ID unique to plot AND yr; ID unqiue to plot, that's same across yrs.
attach(cond.tree.seed)
cond.tree.seed$UNIQUEID<-paste(PLOT,STATECD,COUNTYCD,INVYR,sep='_')
cond.tree.seed$PLOTID<-paste(PLOT,STATECD,COUNTYCD,sep='_')
detach(cond.tree.seed)
dim(cond.tree.seed) # 125673

## Attach lat/long/elev
data <- merge(cond.tree.seed,plot[,c(1,10,11,12)], by.x ='PLT_CN',by.y='CN')
data <- merge(cond.tree.seed,
              plot[,c("CN", "LAT_FS", "LON_FS", "ELEV")],
              by.x ='PLT_CN', by.y='CN')
plot(data$LON_FS, data$LAT_FS, pch=19, cex=0.1)
