## Set directory for climatic deficit z-scores
def.dir <- "C:/Users/clittlef/Google Drive/2RMRS/fia-regen/data/def_z"

## Compile all def z-scores as raster into a list
def.list <- lapply(list.files(def.dir, pattern = ".tif$", full.names = TRUE),
              raster) # applies FUN (here, raster) to all files; dumps into list; $=end

# Plot ok?
plot(def.list[[1]]) 

## What's Terraclim CRS? 99% sure they're all the same, but jic...
crs.all <- vector()
for (i in c(1:74)){
  crs <- print(crs(def.list[[i]]))
  rbind(crs.all, crs)
}
if(all(duplicated(crs.all))) cat("All CRS are the same")
rm(crs.all, crs)


## Gather FIA plot pts into spatial object
# Per FIA manual, pts in lat/long, NAD83 datum ****** IS THIS CORRECT?? ******
FIA.CRS <- crs("+proj=longlat +datum=NAD83")
coords <- cbind(data.all$LON_FS, data.all$LAT_FS) ## (lon,lat) therefore (x,y)
colnames(coords) <- c("lon", "lat")
pts <- SpatialPoints(coords = coords,
                     proj4string = crs(FIA.CRS))
## def_z data use WGS84 datum; transform these pts
pts.trans <- spTransform(pts, crs(def.list[[1]]))

## Create stack of all def-z rasters
rst = stack(def.list)



## collect def_Z into empty list
# nb "extract" also in other packages; be explicit.
output <- list()
loop.ready <- c(1:length(def.list))
# loop.ready <- c(1:2)
for(i in loop.ready){
  output[[i]] <- raster::extract(rst[[i]], pts.trans)
  names(output)[[i]] <- paste0(names(rst)[i],"_z")
}


## Convert list --> df, maintaining names
# do.call takes function (first) then LIST of arguments (second)
# Here, list is output (a list) of lapply that converted...
# elements (numeric) in output into dfs and returned a list of those dfs
def.data <-do.call(cbind,lapply(output,data.frame))
# Pull orig output element names and assign as col names in this df
colnames(def.data) <- names(output)

def.data$PLOTID <- data.all$PLOTID
def.data$FIRE.YR <- data.all$FIRE.YR


###################################### Someday put the following in nested loops...
## Create avg z-score for yrs 0-3 post-fire, MAY-SEPT
# Pull out def columns; ^=beginning; .=any character; $=end
def59.cols <- grep(pattern="^def......5.9_z$$", x=colnames(def.data), value=TRUE)
foo <- def.data[def59.cols] %>% as.data.frame() # weird class change; force df    
foo$FIRE.YR <- data.all$FIRE.YR

# Return avg of def 0-3 yrs post-fire. Vars get duped if I don't have seq_len()
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

foo <- foo %>%
  mutate(def59_z_03 = rowMeans(select(.,starts_with("def59")), na.rm = TRUE))


## Create avg z-score for yrs 0-3 post-fire, JUNE-AUG
# Pull out def columns; ^=beginning; .=any character; $=end
def68.cols <- grep(pattern="^def......6.8_z$$", x=colnames(def.data), value=TRUE)
boo <- def.data[def68.cols] %>% as.data.frame() # weird class change; force df    
boo$FIRE.YR <- data.all$FIRE.YR

# Return avg of def 0-3 yrs post-fire. Vars get duped if I don't have seq_len()
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

boo <- boo %>%
  mutate(def68_z_03 = rowMeans(select(.,starts_with("def68")), na.rm = TRUE))

def.data$def59_z_0 <- foo$def59_z_0
def.data$def59_z_1 <- foo$def59_z_1
def.data$def59_z_2 <- foo$def59_z_2
def.data$def59_z_3 <- foo$def59_z_3
def.data$def59_z_03 <- foo$def59_z_03

def.data$def68_z_0 <- boo$def68_z_0
def.data$def68_z_1 <- boo$def68_z_1
def.data$def68_z_2 <- boo$def68_z_2
def.data$def68_z_3 <- boo$def68_z_3
def.data$def68_z_03 <- boo$def68_z_03



#########################################
## Save as csv
sapply(def.data, class) # make sure all are real cols, not lists
# write.csv(def.data,"def_z_n1971.csv")



#########################################  
## Append to existing plot data
data.all <- data.all %>%
  left_join(def.data, by = "PLOTID")


######################################### 
## Save as csv
sapply(data.all, class) # make sure all are real cols, not lists
write.csv(data.all, "DATA_PlotFireClim_PostFireSamp_n1971.csv")

## Tidy up
rm(rst, pts, pts.trans, FIA.CRS, output, i, loop.ready)





## Re-link to PLOT ID********** THIS FEELS SKETCHY **************
def.data$PLOTID <- data.all$PLOTID
def.data$FIRE.YR <- data.all$FIRE.YR




