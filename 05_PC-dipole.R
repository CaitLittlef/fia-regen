## Extract and plot PC1 and PC2 data of CMD dipole from John A.

# ref:
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm

# install.packages("ncdf4")
library(ncdf4)


## Open connection to dataset & print info.
ncin <- nc_open(paste0(wd, "/pcdata.nc"))
print(ncin) # Shows variable name is pc2
var <- "pc2"



## Get lat/long
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "lat")
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))



## Get time var and attributes; also number of times w/ dim().
# Given that there are 2 times, it may be PC1 and PC2?
t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)



## Get variable and its attributes; verify size of array.
pc.array <- ncvar_get(ncin, var)
dlname <- ncatt_get(ncin, var, "long_name")
dunits <- ncatt_get(ncin, var, "units")
fillvalue <- ncatt_get(ncin, var, "_FillValue")
dim(pc.array)



## Get the global attributes.
title <- ncatt_get(ncin, 0, "title")
institution <- ncatt_get(ncin, 0, "institution")
datasource <- ncatt_get(ncin, 0, "source")
references <- ncatt_get(ncin, 0, "references")
history <- ncatt_get(ncin, 0, "history")
Conventions <- ncatt_get(ncin, 0, "Conventions")

## Close the NetCDF file.
nc_close(ncin)


## Convert time var from time since to more readable format
# install.packages("chron")
library(chron)
# Split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(t, origin = c(tmonth, tday, tyear))


## Convert NetCDF fill values to NAs (standard for R)
pc.array[pc.array == fillvalue$value] <- NA
# Check for total number of non-missing (non-NA) values
pc1.slice <- pc.array[1, , ] ; class(pc1.slice) ; min(pc1.slice, na.rm=T) ; max(pc1.slice, na.rm=T)
pc2.slice <- pc.array[2, , ] ; class(pc2.slice) ; min(pc2.slice, na.rm=T) ; max(pc2.slice, na.rm=T)


## Transpose to orient correctly b/c netCDF orders from bottom left corner
pc1.r <- raster(pc1.slice, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs)
pc2.r <- raster(pc2.slice, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs)

plot(pc1.r)
plot(pc2.r)

## TADA!