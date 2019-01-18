def.dir <- "C:/Users/clittlef/Dropbox/RMRS/fia-regen/data/def_z"

def.list <- lapply(list.files(def.dir, pattern = ".tif$", full.names = TRUE),
              raster) # applies FUN (here, raster) to all files; dumps into list
def.list[[1]]
plot(def.list[[1]])
data.all$LAT_FS, data.all$LON_FS