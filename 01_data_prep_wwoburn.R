###############
## DATA PREP ##
###############

#####################################
## Load data
plot <- read.csv('SP_PLOT_data.csv')                  ## plot data
cond.tree.seed <- read.csv('SP_COND_TREE_SEED.csv')   ## seedling & tree data
climate <- read.csv('SP_ClimateLayers.csv')           ## ClimateWNA data
prism.1971.2000 <- read.csv('SP_Prism_71_00.csv')     ## PRISM norms 1971-2000
prism.1981.2010 <- read.csv('SP_Prism_81_10.csv')     ## PRISM norms 1981-2010
mtbs <- read.csv('SP_MTBS_CMBJoin.csv')               ## severity; CMB=climate monitoring branch (NOAA)
# time.since.fire <- read.csv('SP_TimeSinceFire.csv')   ## time since fire; NIMS=Nat Info Mgmt System



## Quick peek
# plot(plot$LON_FS, plot$LAT_FS, pch=19, cex=0.05)
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
## CONDITION SELECTIONS
#####################################

library(dplyr)

## Confirm there are no dupe plots
plot %>% 
  group_by(CN) %>% 
  filter(n()>1) # no dupes


## Drop field types that aren't forested or have NA
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
  summarize(n=n()) # Per manual, 995 = other exotic hardwoods. DELETE.
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
  dplyr::select(PLOTID, UNIQUEID, everything())
dim(cond.tree.seed) # 27540

## Attach lat/long/elev
data <- merge(cond.tree.seed,
              plot[,c("CN", "LAT_FS", "LON_FS", "ELEV")],
              by.x ='PLT_CN', by.y='CN')



plot(data$LON_FS, data$LAT_FS, pch=19, cex=0.1)
data <- distinct(data)
if(any(duplicated(data))) cat("YOU'VE BEEN DUPED!!")

## But litter/duff sometimes have two measurements per UNIQUEID
# Keep only one distinct record.
data <- distinct(data, UNIQUEID, .keep_all = TRUE) # keeps first row of any dupes
#27540


#####################################
## REVISITED PLOTS
#####################################

## Multiple records per PLOTID -- 1 each inventory yr
moo <- data %>%
  group_by(PLOTID) %>%
  mutate(FREQ=n()) %>%
  filter(FREQ > 1) %>%
  dplyr::select(PLOTID, FREQ, INVYR) %>%
  as.data.frame()
max(moo$FREQ) # 3 is max time a plot visited

# But quick looksee confirms no dupe yrs
moo %>%
  group_by(PLOTID, INVYR) %>%
  summarise(DBL.YR =n()) %>% as.data.frame() 

# Populate table with years visited
plots.rev <- moo %>%
  group_by(PLOTID) %>%
  mutate(YR.1ST = nth(INVYR, 1), # select 1st instance
         YR.2ND = nth(INVYR, 2), # select 2nd instance
         YR.3RD = nth(INVYR, 3)) %>% # select 3rd instance
  dplyr::select(-INVYR) %>%
  distinct() # needed b/c plots have same # records as visits
dim(plots.rev) # 5737

# Clean-up
rm(moo)






#####################################
## MTBS 
#####################################
## Generate unique ID for mtbs plots
mtbs$PLOTID<-paste(mtbs$PLOT,mtbs$STATECD,mtbs$COUNTYCD,sep='_')
mtbs <- mtbs %>%
  rename(FIRE.YR = Year) %>%
  filter(! FIRE.YR == 0) %>%  # Some records missing fire year
  dplyr::select(PLOTID, everything())

## Which of the plots burned?
data.wwoburn <- left_join(data, mtbs, by = "PLOTID") #27461 plots
class(data.wwoburn)

## How many were sampled after burned (one or more times, one or more burns)?
data.wwoburn %>%
  filter(INVYR > FIRE.YR) %>%
  count(PLOTID) # 1961 plots sampled after a burn/burns
data.wwoburn %>%
  filter(INVYR > FIRE.YR) %>%
  count(PLOTID) %>%
  filter(n>1) # 398 sampled more than once after a burn/burns


## List plots that burned and were sampled after burn
# Keeping latest visit, latest fire (or sites that didn't burn, hence is.na())
# If that visit happened after reburn, reburn = Y.
data.wwoburn.samp <- data.wwoburn %>%
  group_by(PLOTID) %>%
  filter(INVYR == max(INVYR)) %>%
  filter(is.na(FIRE.YR) | INVYR > FIRE.YR) %>% 
  mutate(REBURN = ifelse(n() > 1, "Y", "N")) %>%
  filter(is.na(FIRE.YR) | FIRE.YR == max(FIRE.YR))
data.wwoburn.samp$REBURN[is.na(data.wwoburn.samp$FIRE.YR)] <- NA
temp <- data.wwoburn.samp[! is.na(data.wwoburn.samp$FIRE.YR) ,] %>%
  dplyr::select(PLOTID, FIRE.YR, INVYR, REBURN, paste0(mtbs.cols))
if(all(temp$INVYR > temp$FIRE.YR)) cat("All visits are AFTER fire")
if(any(duplicated(data.wwoburn.samp))) cat("YOU'VE BEEN DUPED!!") # 20860


## Assign burn severity to each record & yrs btwn fire and sampling
# Pull out burn severity columns; keep only those with 4 digits (year); .=any character; $=end
mtbs.cols <- grep(pattern="^mtbs_....$", x=colnames(data.wwoburn.samp), value=TRUE)
moo <- data.wwoburn.samp[mtbs.cols] %>% as.data.frame() # weird class change; force df    
moo$FIRE.YR <- data.wwoburn.samp$FIRE.YR
moo$INVYR <- data.wwoburn.samp$INVYR
moo$YEAR.DIFF <- NA
moo$YEAR.DIFF <- (moo$INVYR - moo$FIRE.YR)
moo$FIRE.SEV <- NA

# Return severity from year of fire
# Not sure why I need composite index calling for all rows...
# but excluding it duplicates burn sev variables.
moo$FIRE.SEV <-
  moo[cbind(
    seq_len(nrow(moo)),
    match(paste0(moo$FIRE.YR),
          substr(names(moo),6,9))
  )]

data.wwoburn.samp$YEAR.DIFF <- NA
data.wwoburn.samp$YEAR.DIFF <- moo$YEAR.DIFF
data.wwoburn.samp$FIRE.SEV <- NA
data.wwoburn.samp$FIRE.SEV <- moo$FIRE.SEV
data.wwoburn.samp <- as.data.frame(data.wwoburn.samp) # else still grouped

# Replace cat 5 & 6 with NA; One site burned but has YR/mtbs mismatch (FIRE.SEV = 0) so drop it.
# Rownames get ugly/re-assigned with conditional == 0, so calling that row explicitly.
data.wwoburn.samp %>% count(FIRE.SEV)
data.wwoburn.samp$FIRE.SEV[data.wwoburn.samp$FIRE.SEV == 5 | data.wwoburn.samp$FIRE.SEV == 6] <- NA
data.wwoburn.samp[which(data.wwoburn.samp$FIRE.SEV == 0),]
data.wwoburn.samp <- data.wwoburn.samp[-3035,]
data.wwoburn.samp %>% count(FIRE.SEV)


## Lots ~Yellowstone, Idaho
temp <- data.wwoburn.samp[! is.na(data.wwoburn.samp$FIRE.YR) ,]
map('state', region = c('cali', 'oreg', 'wash','idaho','monta','wyo','utah','ariz','new mex','colo'))
points(temp$LON_FS, temp$LAT_FS,pch=1,cex=sqrt(temp$YEAR.DIFF))
legend("bottomleft",c("time since fire = 4 years","time since fire = 16 years"),pch=c(1,1), pt.cex=c(2,4))

## Tidy
data.clean <- data.wwoburn.samp
# rm(data, data.wwoburn, moo, doo, mtbs.cols, temp, plots.rev)


#####################################
## CLIMATE 
#####################################
## Assign IDs
# nb some records have >1 inventory yrs, but cliamte data are identical --> drop INVYR
climate$PLOTID <- paste(climate$PLOT_NIMS,
                        climate$STATECD,
                        climate$COUNTYCD,
                        sep='_')
climate <- climate %>%
  dplyr::select(PLOTID, everything(),-INVYR, -STATECD, -COUNTYCD, -PLOT_NIMS) %>%
  distinct() # still look to be some dupes

prism.1981.2010$PLOTID <- paste(prism.1981.2010$PLOT,
                                prism.1981.2010$STATECD,
                                prism.1981.2010$COUNTYCD,sep='_')
prism <- prism.1981.2010 %>%
  dplyr::select(PLOTID, everything(), -STATECD, -COUNTYCD, -PLOT) %>%
  distinct()

## Join plot data to climate data
data.all <- data.clean %>%
  left_join(climate, by = "PLOTID") %>%
  left_join(prism, by = "PLOTID")

## N.b., there are still some essentially duplicate records...
# b/c duff/litter values differ subtely. 
# Here, keep distinct UNIQUEID values -- the first row if not distinct.
data.all <- distinct(data.all, UNIQUEID, .keep_all = TRUE)


## Save as csv
sapply(data.all, class) # make sure all are real cols, not lists
write.csv(data.all, "DATA_PlotwwoFirePrismClimWNA-noTerra_n20859.csv")

temp <- data.all[which(data.all$FIRE.YR > 1900),] # these are burned sites

rm(temp, data, data.clean, data.wwoburn, data.wwoburn.samp, moo, plots.rev, reburns, mtbs.cols)
