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


## Quick peek
plot(plot.data$LON_FS, plot.data$LAT_FS, pch=19, cex=0.1)
# CO, OR, not present; WY "sparse"
# How many pts each? 
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
## CONDITION SELETIONS
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
## UNIQUE IDS FOR PLOTS
#####################################

## Assign ID unique to plot AND yr; ID unqiue to plot, that's same across yrs.
attach(cond.tree.seed)
cond.tree.seed$UNIQUEID<-paste(PLOT,STATECD,COUNTYCD,INVYR,sep='_')
cond.tree.seed$PLOTID<-paste(PLOT,STATECD,COUNTYCD,sep='_')
detach(cond.tree.seed)
# Put those IDs up front for ease of access
cond.tree.seed <- cond.tree.seed %>%
  select(PLOTID, UNIQUEID, everything())
dim(cond.tree.seed) # 27540

## Attach lat/long/elev
data <- merge(cond.tree.seed,
              plot[,c("CN", "LAT_FS", "LON_FS", "ELEV")],
              by.x ='PLT_CN', by.y='CN')
plot(data$LON_FS, data$LAT_FS, pch=19, cex=0.1)





#####################################
## REVISITED PLOTS
#####################################

## Multiple records per PLOTID -- 1 each inventory yr
moo <- data %>%
  group_by(PLOTID) %>%
  mutate(FREQ=n()) %>%
  filter(FREQ > 1) %>%
  select(PLOTID, FREQ, INVYR) %>%
  as.data.frame()
max(moo$FREQ) # 4 is max time a plot visited

# But quick looksee shows some plots have dupe yrs...
# Only litter/duff depth seem diff in same-yr records.
# moo %>%
#   group_by(PLOTID, INVYR) %>%
#   summarise(DBL.YR =n()) %>% as.data.frame() # yup, some doubles

# Keep only one distinct record.
moo <- distinct(moo)
# moo %>%
#   group_by(PLOTID, INVYR) %>%
#   summarise(DBL.YR =n()) %>% as.data.frame() # looks like only 1s...

# Re-run frequency
doo <- moo %>%
  select(-FREQ) %>%
  group_by(PLOTID) %>%
  mutate(FREQ=n()) %>%
  as.data.frame()
max(doo$FREQ) # 3 is max time a plot visited

# Populate table with years visited
plots.rev <- doo %>%
  group_by(PLOTID) %>%
  mutate(YR.1ST = nth(INVYR, 1), # select 1st instance
         YR.2ND = nth(INVYR, 2), # select 2nd instance
         YR.3RD = nth(INVYR, 3)) %>% # select 3rd instance
  select(-INVYR) %>%
  distinct() # needed b/c plots have same # records as visits
dim(plots.rev) # 5892

# Clean-up
rm(moo, doo)






#####################################
## MTBS
#####################################
## Generate unique ID for mtbs plots
mtbs$PLOTID<-paste(mtbs$PLOT,mtbs$STATECD,mtbs$COUNTYCD,sep='_')
mtbs <- mtbs %>% select(PLOTID, everything())

## Which of the plots burned?
data.burned <- inner_join(data, mtbs, by = "PLOTID") #3536 plots
data.burned <- rename(data.burned, FIRE.YR = Year)


## How many were sampled after burned (one or more times, one or more burns)?
data.burned %>%
  filter(INVYR > FIRE.YR) %>%
  count(PLOTID) # 1992 plots sampled post-burn (incl. some multiple samples)


## How many were sampled twice after burn?
# Recall group_by un-group for final delivery so have to count()
data.burned %>%
  filter(INVYR > FIRE.YR) %>%
  group_by(PLOTID) %>%
  mutate(FREQ.BURN.SAMP = n()) %>%
  filter(FREQ.BURN.SAMP > 1) %>%
  count(PLOTID) # 442 plots were sampled multiple times post-burn


## List plots that burned and were sampled after -- keep latest visit
data.burned.samp <- data.burned %>%
  filter(INVYR > FIRE.YR) %>%
  group_by(PLOTID) %>%
  mutate(LATEST.INVYR = max(INVYR)) %>%
  mutate(LATEST.FIRE = max(FIRE.YR)) %>% # Nix reburns (saw at least 1)
  filter(LATEST.FIRE > 0) %>% # at 1992 (matches above) til X these zeros
  select(PLOTID, LATEST.INVYR, LATEST.FIRE) %>%
  distinct() # 1971 records of latest inventory post-most recent burn

test
