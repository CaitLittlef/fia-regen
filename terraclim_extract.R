def.dir <- "C:/Users/clittlef/Dropbox/RMRS/fia-regen/data/def_z"

def.list <- lapply(list.files(def.dir, pattern = ".tif$", full.names = FALSE),
              raster) # applies FUN (here, raster) to all files; dumps into list
# Plot ok?
plot(def.list[[1]]) 

## What's Terraclim CRS? 
# Guaranteed they're all the same, but jic...
crs.all <- vector()
for (i in c(1:74)){
  crs <- print(crs(def.list[[i]]))
  rbind(crs.all, crs)
}
if(all(duplicated(crs.all))) cat("All CRS same for rasters")
rm(crs.all, crs)


## Gather FIA plot pts into spatial object
# Per FIA manual, pts in lat/long, NAD83 datum ****** IS THIS CORRECT?? ******
PROJ.CRS <- crs("+proj=longlat +datum=NAD83")
coords <- cbind(data.all$LAT_FS, data.all$LON_FS)
colnames(coords) <- c("lat", "lon")
pts <- SpatialPoints(coords = coords,
                     proj4string = crs(PROJ.CRS))

rst = stack(def.list)
xtr = raster::extract(rst, pts) ## tidyr has extract, too, which conflicts
# Warning: pts transformed to CRS of raster
out = data.frame(coords, xtr) #### ALL ZEROS WHY?!???!?
out$LAT_FS[1]
data.all$LAT_FS[1]