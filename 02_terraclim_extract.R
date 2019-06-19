currentDate <- Sys.Date()

## Load data.all if necessary
# data.all <- read.csv("DATA_PlotFirePrism-noTerra_PostFireSamp_n1971.csv")
# data.all <- read.csv("DATA_PlotwwoFirePrismClimWNA-noTerra_n20859.csv")
data.all <- read.csv("DATA_PlotwwoFirePrismClimWNA-noTerra_n20543_190424.csv")
# data.all$X <- NULL

## set directory for terraclimate
tc.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/"

## Set directory for climatic deficit z-scores
def.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/terraclimate/def_z"

## Load TerraClime datasets
aet <- raster(paste0(tc.dir,"aet.1981.2010.tif"))
def <- raster(paste0(tc.dir,"def.1981.2010.tif"))
ppt <- raster(paste0(tc.dir,"ppt.1981.2010.tif"))
tmax <- raster(paste0(tc.dir,"tmax.1981.2010.tif"))

## Compile all def z-scores as raster into a list
def.list <- lapply(list.files(def.dir, pattern = ".tif$", full.names = TRUE),
                   raster) # applies FUN (here, raster) to all files; dumps into list; $=end

## Create stack of all def-z rasters
rst = stack(def.list)

## What's Terraclim CRS? 99% sure they're all the same, but jic...
# crs.all <- vector()
# for (i in c(1:74)){
#   crs <- print(crs(def.list[[i]]))
#   rbind(crs.all, crs)
# }
# if(all(duplicated(crs.all))) cat("All CRS are the same")
# rm(crs.all, crs)

crs(def.list[[1]])
crs(aet)
crs(def)
crs(ppt)
crs(tmax)

## Gather FIA plot pts into spatial object
# Per FIA manual, pts in lat/long, NAD83 datum ****** IS THIS CORRECT?? ******
FIA.CRS <- crs("+proj=longlat +datum=NAD83")
coords <- cbind(data.all$LON_FS, data.all$LAT_FS) ## (lon,lat) therefore (x,y)
colnames(coords) <- c("lon", "lat")
pts <- SpatialPoints(coords = coords,
                     proj4string = crs(FIA.CRS))
# def_z data use WGS84 datum; transform these pts
pts.trans <- spTransform(pts, crs(def.list[[1]]))


################################################# EXTRACT AET, DEF, PPT, TMAX

aet.tc <- raster::extract(aet, pts.trans)
def.tc <- raster::extract(def, pts.trans)
ppt.tc <- raster::extract(ppt, pts.trans)
tmax.tc <- raster::extract(tmax, pts.trans)

terraclim <- as.data.frame(cbind(aet.tc, def.tc, ppt.tc, tmax.tc))
terraclim$PLOTID <- data.all$PLOTID


################################################# EXTRACT DEF Z VALUES TO FIA PTS
## collect def_Z into empty list; nb "extract" also in other packages so be explicit.
output <- list()
loop.ready <- c(1:length(def.list))
# loop.ready <- c(1:2)
for(i in loop.ready){
  output[[i]] <- raster::extract(rst[[i]], pts.trans)
  names(output)[[i]] <- paste0(names(rst)[i],"_z")
}

# do.call takes function (first) then LIST of arguments (second)
# Here, list is output (a list) of lapply that converted...
# elements (numeric) in output into dfs and returned a list of those dfs
def.data <-do.call(cbind,lapply(output,data.frame))
# Pull orig output element names and assign as col names in this df
colnames(def.data) <- names(output)

def.data$PLOTID <- data.all$PLOTID
def.data$FIRE.YR <- data.all$FIRE.YR


###################################### GET Z-SCORE TREND
## Grab slope of z-scores for each plot as reflection of climate change.
# If deficit is increasing, z-scores will be increasingly positive.
# Steeper slope means changing faster.
# All z-scores should add to zero, but here values (up to 2018) are past normal period (1981-2010)...
# ... as this would show: mutate(sum.z.scores = rowSums(.))

## Get only MAY-SEPT; rename columns as year.
def59.cols <- grep(pattern="^def......5.9_z$$", x=colnames(def.data), value=TRUE)
goo <- def.data[def59.cols] %>% as.data.frame() # weird class change; force df    
colnames(goo) <- mid(colnames(goo), 5, 4)
goo$PLOTID <- def.data$PLOTID

## Gather data in prep for trendline. Col names are now a variable (def.year).
# Keep only 1984 (mtbs) thru 2017 (no 2018 for terraclim).
zoo <- gather(data = goo, key = 'def.year', value = 'def.z', -PLOTID)
zoo$def.year <- as.numeric(zoo$def.year)
zoo <- zoo %>% filter(def.year > 1983)

## Create trend lines; put into df (do() extracts tidied model outputs)
def.z.lm <- zoo %>%
  group_by(PLOTID) %>%
  do(broom::tidy(lm(def.z ~ def.year, data =.)))
def.z.lm <- as.data.frame(def.z.lm)
def.z.lm[1:10,]
# Alt: this stores each model as a df; not as tidy
# def.z.lm <- zoo %>%
#   group_by(PLOTID) %>%
#   do(mod = lm(def.z ~ def.year, data =.))
# def.z.lm[def.z.lm$PLOTID == "1_16_13",]$mod # gives equation

## Test a few plots: do slope & intercept make sense?
moo <- zoo[zoo$PLOTID == "1_16_13",]
# moo <- zoo[zoo$PLOTID == "41_30_103",]
# moo <- zoo[zoo$PLOTID == "3061_8_71",] 
# moo <- zoo[zoo$PLOTID == "70_16_57",]
def.z.lm[def.z.lm$PLOTID == "1_16_13",]
# def.z.lm[def.z.lm$PLOTID == "41_30_103",]
plot(moo$def.year, moo$def.z)
abline(-34.59488731, 0.01736898); abline(h=0)
# abline(5.179523331, -0.002553094); abline(h=0)
# Alt for plotting:
moo <- filter(zoo, grepl("^10_", PLOTID))
# p <- ggplot(moo, aes(x=def.year, y=def.z, color = PLOTID)) +
#      geom_point()
# p + stat_smooth(aes(group = PLOTID),
#                 method = "lm")


## Keep only slopes
doo <- def.z.lm %>%
  filter(term == "def.year") %>%
  dplyr::select(PLOTID, estimate) %>%
  rename(def59.z.slope = estimate)

## Add onto deficit data.
def.data$def59.z.slope <- doo$def59.z.slope



## Also take slp btwn 1st & last; inappropriate?
## Create trend lines; put into df (do() extracts tidied model outputs)
# def.z.lm84_17 <- zoo %>%
#   group_by(PLOTID) %>%
#   filter(def.year == "1984" | def.year == "2017") %>%
#   do(tidy(lm(def.z ~ def.year, data =.)))
# def.z.lm84_17 <- as.data.frame(def.z.lm84_17)
# def.z.lm84_17[1:10,]
# moo <- filter(zoo, grepl("^10_", PLOTID))
# moo <- filter(moo, def.year == "1984" | def.year == "2017")
# plot(moo$def.year, moo$def.z)
# 
# 
# ## Keep only slopes
# doo <- def.z.lm84_17 %>%
#   filter(term == "def.year") %>%
#   dplyr::select(PLOTID, estimate) %>%
#   rename(def59.z.slope84_17 = estimate)
# 
# ## Add onto deficit data.
# def.data$def59.z.slope84_17 <- doo$def59.z.slope84_17


# # Create quad fit; put into df (do() extracts tidied model outputs)
# def.z.2lm <- zoo %>%
#   group_by(PLOTID) %>%
#   do(tidy(lm(def.z ~ poly(def.year, 2), data =.)))
# def.z.2lm <- as.data.frame(def.z.2lm)
# def.z.2lm[1:10,]
# # Plot a few
# moo <- filter(zoo, grepl("^10_", PLOTID))
# p <- ggplot(moo, aes(x=def.year, y=def.z, color = PLOTID)) +
#   geom_point()
# p + stat_smooth(aes(group = PLOTID),
#                 method = "lm",
#                 formula = y ~ poly(x, 2))
# # ^ Just doesn't make much sense

# rm(foo, boo, moo, doo)


###################################### GET POST-FIRE cONDITIONS
## Create avg z-score for yrs 0-5 post-fire, MAY-SEPT
# Pull out def columns; ^=beginning; .=any character; $=end
# Careful to explicitly select cols w/  summary variables!
# e.g., using select(., starts_with...) may inadvertantly reference existing vars.
def59.cols <- grep(pattern="^def......5.9_z$$", x=colnames(def.data), value=TRUE)
foo <- def.data[def59.cols] %>% as.data.frame() # weird class change; force df    
foo$FIRE.YR <- data.all$FIRE.YR
foo$YEAR.DIFF <- data.all$YEAR.DIFF

# Return of def 0-5 yrs post-fire. Vars get duped if I don't have seq_len()
foo$def59_z_0 <- 
  foo[cbind(
    seq_len(nrow(foo)),
    match(paste0(foo$FIRE.YR),
          substr(names(foo),5,8))
    )]
  
foo$def59_z_1 <- 
  foo[cbind(
    seq_len(nrow(foo)),
    match(paste0(foo$FIRE.YR),
          substr(names(foo),5,8))+1
  )]

foo$def59_z_2 <- 
  foo[cbind(
    seq_len(nrow(foo)),
    match(paste0(foo$FIRE.YR),
          substr(names(foo),5,8))+2
  )]

foo$def59_z_3 <- 
  foo[cbind(
    seq_len(nrow(foo)),
    match(paste0(foo$FIRE.YR),
          substr(names(foo),5,8))+3
  )]

foo$def59_z_4 <- 
  foo[cbind(
    seq_len(nrow(foo)),
    match(paste0(foo$FIRE.YR),
          substr(names(foo),5,8))+4
  )]

foo$def59_z_5 <- 
  foo[cbind(
    seq_len(nrow(foo)),
    match(paste0(foo$FIRE.YR),
          substr(names(foo),5,8))+5
  )]

# Create averages for windows post-fire
foo <- foo %>%
  mutate(def59_z_12 = rowMeans(dplyr::select(.,"def59_z_1", "def59_z_2")))
foo <- foo %>%
  mutate(def59_z_13 = rowMeans(dplyr::select(.,"def59_z_1", "def59_z_2", "def59_z_3")))
foo <- foo %>%
  mutate(def59_z_14 = rowMeans(dplyr::select(.,"def59_z_1", "def59_z_2", "def59_z_3", "def59_z_4")))
foo <- foo %>%
  mutate(def59_z_15 = rowMeans(dplyr::select(.,"def59_z_1", "def59_z_2", "def59_z_3", "def59_z_4", "def59_z_5")))


# Find max post-fire z-score within 5 yrs. 
foop <- foo[(c("YEAR.DIFF", "def59_z_1", "def59_z_2", "def59_z_3", "def59_z_4", "def59_z_5"))]

fmax <- function(x) {
  yr <- (x[1])
  if (is.na(yr)) {
    return(NA)
    } else if (yr < 5) {
    return(max(x[2:(1+yr)]))
    } else {
    return(max(x[2:6]))
  }
}

foo$def59_z_max15 <- apply(foop, 1, function(x) fmax(x)) #do it by row by setting arg2 = 1



## Create avg z-score for yrs 0-5 post-fire, JUNE-AUG
# Pull out def columns; ^=beginning; .=any character; $=end
def68.cols <- grep(pattern="^def......6.8_z$$", x=colnames(def.data), value=TRUE)
boo <- def.data[def68.cols] %>% as.data.frame() # weird class change; force df    
boo$FIRE.YR <- data.all$FIRE.YR
boo$YEAR.DIFF <- data.all$YEAR.DIFF

# Return avg of def 0-5 yrs post-fire. Vars get duped if I don't have seq_len()
boo$def68_z_0 <- 
  boo[cbind(
    seq_len(nrow(boo)),
    match(paste0(boo$FIRE.YR),
          substr(names(boo),5,8))
  )]

boo$def68_z_1 <- 
  boo[cbind(
    seq_len(nrow(boo)),
    match(paste0(boo$FIRE.YR),
          substr(names(boo),5,8))+1
  )]

boo$def68_z_2 <- 
  boo[cbind(
    seq_len(nrow(boo)),
    match(paste0(boo$FIRE.YR),
          substr(names(boo),5,8))+2
  )]

boo$def68_z_3 <- 
  boo[cbind(
    seq_len(nrow(boo)),
    match(paste0(boo$FIRE.YR),
          substr(names(boo),5,8))+3
  )]


boo$def68_z_4 <- 
  boo[cbind(
    seq_len(nrow(boo)),
    match(paste0(boo$FIRE.YR),
          substr(names(boo),5,8))+4
  )]


boo$def68_z_5 <- 
  boo[cbind(
    seq_len(nrow(boo)),
    match(paste0(boo$FIRE.YR),
          substr(names(boo),5,8))+5
  )]


# Create averages for windows post-fire
boo <- boo %>%
  mutate(def68_z_12 = rowMeans(dplyr::select(.,"def68_z_1", "def68_z_2")))
boo <- boo %>%
  mutate(def68_z_13 = rowMeans(dplyr::select(.,"def68_z_1", "def68_z_2", "def68_z_3")))
boo <- boo %>%
  mutate(def68_z_14 = rowMeans(dplyr::select(.,"def68_z_1", "def68_z_2", "def68_z_3", "def68_z_4")))
boo <- boo %>%
  mutate(def68_z_15 = rowMeans(dplyr::select(.,"def68_z_1", "def68_z_2", "def68_z_3", "def68_z_4", "def68_z_5")))


# Find max post-fire z-score within 5 yrs. (see function defined above with foo)
boop <- boo[(c("YEAR.DIFF", "def68_z_1", "def68_z_2", "def68_z_3", "def68_z_4", "def68_z_5"))]
boo$def68_z_max15 <- apply(boop, 1, function(x) fmax(x)) #do it by row by setting arg2 = 1


# Put computed vals back into major dataset
def.data$def59_z_0 <- foo$def59_z_0
def.data$def59_z_1 <- foo$def59_z_1
def.data$def59_z_2 <- foo$def59_z_2
def.data$def59_z_3 <- foo$def59_z_3
def.data$def59_z_4 <- foo$def59_z_4
def.data$def59_z_4 <- foo$def59_z_5
def.data$def59_z_12 <- foo$def59_z_12
def.data$def59_z_13 <- foo$def59_z_13
def.data$def59_z_14 <- foo$def59_z_14
def.data$def59_z_15 <- foo$def59_z_15
def.data$def59_z_max15 <- foo$def59_z_max15

def.data$def68_z_0 <- boo$def68_z_0
def.data$def68_z_1 <- boo$def68_z_1
def.data$def68_z_2 <- boo$def68_z_2
def.data$def68_z_3 <- boo$def68_z_3
def.data$def68_z_4 <- boo$def68_z_4
def.data$def68_z_5 <- boo$def68_z_5
def.data$def68_z_12 <- boo$def68_z_12
def.data$def68_z_13 <- boo$def68_z_13
def.data$def68_z_14 <- boo$def68_z_14
def.data$def68_z_15 <- boo$def68_z_15
def.data$def68_z_max15 <- boo$def68_z_max15


#########################################
## Save as csv
sapply(def.data, class) # make sure all are real cols, not lists
# write.csv(def.data, paste0("def_z_n20543_",currentDate,".csv"))

## Save as csv
sapply(terraclim, class) # make sure all are real cols, not lists
# write.csv(terraclim, paste0("tc_n20543_",currentDate,".csv"))



#########################################  
## Append to existing plot data
data.all <- data.all %>%
  left_join(def.data, by = "PLOTID")
data.all <- data.all %>%
  left_join(terraclim, by = "PLOTID")


######################################### 
## Save as csv
sapply(data.all, class) # make sure all are real cols, not lists
# write.csv(data.all, "DATA_PlotFireClim_PostFireSamp_n1971_",currentDate,".csv"))
# write.csv(data.all, paste0("DATA_PlotwwoFireClim_n20543_",currentDate,".csv"))

## Tidy up
# rm(rst, pts, pts.trans, FIA.CRS, coords, output, i, loop.ready, def.dir, def.list, def.data, def59.cols, def68.cols, temp, foo, boo, moo)



