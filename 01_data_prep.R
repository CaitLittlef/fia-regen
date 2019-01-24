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
plot(plot$LON_FS, plot$LAT_FS, pch=19, cex=0.1)
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
  select(PLOTID, UNIQUEID, everything())
dim(cond.tree.seed) # 27540

## Attach lat/long/elev
data <- merge(cond.tree.seed,
              plot[,c("CN", "LAT_FS", "LON_FS", "ELEV")],
              by.x ='PLT_CN', by.y='CN')
plot(data$LON_FS, data$LAT_FS, pch=19, cex=0.1)
data <- distinct(data)
if(any(duplicated(data))) cat("YOU'VE BEEN DUPED!!")



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
dim(plots.rev) # 5829

# Clean-up
rm(moo, doo)






#####################################
## MTBS 
#####################################
## Generate unique ID for mtbs plots
mtbs$PLOTID<-paste(mtbs$PLOT,mtbs$STATECD,mtbs$COUNTYCD,sep='_')
mtbs <- mtbs %>%
  rename(FIRE.YR = Year) %>%
  filter(! FIRE.YR == 0) %>%  # Some records missing fire year
  select(PLOTID, everything())

## Which of the plots burned?
data.burned <- inner_join(data, mtbs, by = "PLOTID") #3483 plots


## ID reburns.
# reburns <- data.burned %>%
#   group_by(PLOTID) %>%
#   mutate(NUM.BURNS = n()) %>%
#   filter(NUM.BURNS > 1) %>%
#   select(PLOTID, FIRE.YR, NUM.BURNS) %>%
#   distinct() # 1152 plots had reburns


## How many were sampled after burned (one or more times, one or more burns)?
data.burned %>%
  filter(INVYR > FIRE.YR) %>%
  count(PLOTID) # 1961 plots sampled after a burn/burns
data.burned %>%
  filter(INVYR > FIRE.YR) %>%
  count(PLOTID) %>%
  filter(n>1) # 428 sampled more than once after a burn/burns


## List plots that burned and were sampled after burn
# Keeping latest visit, latest fire.
# May want to keep ANY visit that happened after a fire but before any reburn.
# But for now, stick with a single record for each plot.
data.burned.samp <- data.burned %>%
  group_by(PLOTID) %>%
  filter(INVYR > FIRE.YR) %>%
  filter(FIRE.YR == max(FIRE.YR)) %>%
  filter(INVYR == max(INVYR))
if(all(data.burned.samp$INVYR > data.burned.samp$FIRE.YR)) cat("All visits are AFTER fire")
if(any(duplicated(data.burned.samp))) cat("YOU'VE BEEN DUPED!!") # 2000



## Assign burn severity to each record
# Pull out burn severity columns; keep only those with 4 digits (year); $=end
mtbs.cols <- grep(pattern="^mtbs_....$", x=colnames(data.burned.samp), value=TRUE)
moo <- data.burned.samp[mtbs.cols] %>% as.data.frame() # weird class change; force df    
moo$FIRE.YR <- data.burned.samp$FIRE.YR

# Return burn severity from year of fire
# Not sure why I need composite index calling for all rows...
# but excluding it duplicates burn sev variables.
moo$BURN.SEV <-
  moo[cbind(
    seq_len(nrow(moo)),
    match(paste0(moo$FIRE.YR),
          substr(names(moo),6,9))
  )]


########## START HERE!

########################################
### what is the fire severity?
###################################

# extract subset of total dataframe to work with
data.mtbs<-data.pipo[,218:246]
var.name<-names(data.mtbs)
yr.ind<-data.pipo$Year

#function to return MTBS fire severity for year of fire match
year.match<-function(x,data){
  yr<-grep(x,data)
  fluf<-data.mtbs[i,yr]
  return(fluf)
}

year.match(ty,var.name)  

#run function across loop
fire.sev<-numeric(length(yr.ind))

for (i in 1:length(fire.sev)){
  temp.year<-yr.ind[i]
  out<-year.match(temp.year,var.name)
  fire.sev[i]<-out
}

#drop category 5 and 6 and replace with NA
ind<-which(fire.sev==5 | fire.sev==6)
fire.sev<-replace(fire.sev,ind,values=NA)
data.pipo$fire.sev<-fire.sev


## is remaining live BA and fire severity correlated?
cor(data.pipo$fire.sev,data.pipo$BA.total.live,method="spearman",na.rm=T)
cor.test(data.pipo$fire.sev,data.pipo$BA.total.live,method="spearman")
###############################################3
















## Create years since var
data.burned.samp$YEAR.DIFF <- (data.burned.samp$INVYR - data.burned.samp$FIRE.YR)
data.burned.samp <- data.burned.samp %>% select(PLOTID, UNIQUEID, INVYR, FIRE.YR, YEAR.DIFF, everything())
hist(data.clean$YEAR.DIFF)

## Lots ~Yellowstone, Idaho
map('state', region = c('cali', 'oreg', 'wash','idaho','monta','wyo','utah','ariz','new mex','colo'))
points(data.burned.samp$LON_FS, data.burned.samp$LAT_FS,pch=1,cex=sqrt(data.burned.samp$YEAR.DIFF))
legend("bottomleft",c("time since fire = 4 years","time since fire = 16 years"),pch=c(1,1), pt.cex=c(2,4))

## For consistency with orig code...
data.clean <- data.burned.samp
rm(data, data.burned, data.burned.samp)


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
  select(PLOTID, everything(),-INVYR, -STATECD, -COUNTYCD, -PLOT_NIMS) %>%
  distinct() # still look to be some dupes

prism.1981.2010$PLOTID <- paste(prism.1981.2010$PLOT,
                                prism.1981.2010$STATECD,
                                prism.1981.2010$COUNTYCD,sep='_')
prism <- prism.1981.2010 %>%
  select(PLOTID, everything(), -STATECD, -COUNTYCD, -PLOT) %>%
  distinct()

## Join plot data to climate data
data.all <- data.clean %>%
  left_join(climate, by = "PLOTID") %>%
  left_join(prism, by = "PLOTID")

## N.b., there are still some essentially duplicate records...
# b/c duff/litter values differ subtely. 
# Here, keep distinct UNIQUEID values -- the first row if not distinct.
data.all <- distinct(data.all, UNIQUEID, .keep_all = TRUE)

if(any(duplicated(data.all$UNIQUEID))) cat("YOU'VE BEEN DUPED!!") # 1971

