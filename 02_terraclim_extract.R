## Set directory for climatic deficit z-scores
def.dir <- "C:/Users/clittlef/Dropbox/RMRS/fia-regen/data/def_z"

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



## Re-link to PLOT ID ********** THIS FEELS SKETCHY **************
def.data$PLOTID <- data.all$PLOTID
def.data <- def.data %>%
  select(PLOTID, everything())



## Save as csv
# write.csv(def.data,"def_z_n1971.csv")


  
## Append to existing plot data
data.all <- data.all %>%
  left_join(def.data, by = "PLOTID")



## Save as csv
# write.csv(data.all, "DATA_PlotFireClim_PostFireSamp_n1971.csv")
rm(rst, pts, pts.trans, FIA.CRS, output, i, loop.ready)