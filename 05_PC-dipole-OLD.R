## Extract and plot PC1 and PC2 data of CMD dipole from John A.
# PC1 explains 37.04% of the total variance, and 2nd 15.7%.

# ref:
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm
# install.packages("ncdf4")
library(ncdf4)


#### LOAD & SETUP #####################################################3
## Open connection to dataset & print info.
ncin <- nc_open(paste0(wd, "/pcdata.nc"))
print(ncin) # Shows variable name is pc2 of May-Sep CWD (1st & 2nd pcs)
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
# FYI, if this was time, convert from time-since (e.g., 1900) to date with chron package.
t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)



## Get variable (pc values) and attributes; verify size of array.
pc.array <- ncvar_get(ncin, var)
dlname <- ncatt_get(ncin, var, "long_name")
dunits <- ncatt_get(ncin, var, "units")
fillvalue <- ncatt_get(ncin, var, "_FillValue")
dim(pc.array)



## Close the NetCDF file and work with array henceforth.
nc_close(ncin)


#### SET TO RASTER #####################################################3

## Convert NetCDF fill values to NAs (standard for R)
pc.array[pc.array == fillvalue$value] <- NA
# Check for total number of non-missing (non-NA) values
pc1.slice <- pc.array[1, , ] ; class(pc1.slice) ; min(pc1.slice, na.rm=T) ; max(pc1.slice, na.rm=T)
pc2.slice <- pc.array[2, , ] ; class(pc2.slice) ; min(pc2.slice, na.rm=T) ; max(pc2.slice, na.rm=T)


## Rasterize
pc1.r <- raster(pc1.slice, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs)
pc2.r <- raster(pc2.slice, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs)

plot(pc1.r) # explains 37.04% of the total variance
plot(pc2.r) # explains 15.7% of the total variance



#### PLOT #####################################################3

## Convert raster data to dataframe for plotting
pc.data <- gplot_data(pc1.r)
pc.data <- gplot_data(pc2.r)


# What should the limits when plotting be?
min(pc.data$value[is.finite(pc.data$value)], na.rm =TRUE) # -0.7 for pc1, -0.7 for pc2
max(pc.data$value[is.finite(pc.data$value)], na.rm =TRUE) # 0.9 for pc1, 0.6 for pc2

display.brewer.pal(8, "Dark2")
dev.off()
par(mfrow=c(1,1))
pc1 <- ggplot() +
# pc2 <- ggplot() +
  geom_raster(data = pc.data, aes(x = x, y = y, fill = value), interpolate = TRUE) +
  geom_sf(data = nonIntWest, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = NA) +
  scale_fill_gradient2("PC1",
                       low = palette[2], mid = "white", high = palette[3],
                       midpoint = 0,
                       limits = c(-1,1), 
                       na.value = NA) +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), # blend lat/long into background
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        panel.background = element_rect(fill = "#EAECEE"),
        axis.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size = 0,5),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) + # top, right, bottom, left
  annotate("text", x = -120.5, y = 49.5, label = "PC1", hjust = 0)
  # annotate("text", x = -120.5, y = 49.5, label = "PC2", hjust = 0)
dev.off()
pc1
pc2


## Save both together as png
# png(paste0(out.dir,"pc_map_",currentDate,".png"),
#     width = 950, height = 600, units = "px")
# grid.arrange(pc1, pc2, ncol = 2)
# dev.off()
  


#### TIME SERIES ############################################
# Source: Jon Abatzoglou generated; captures N-S dipole.
# Positive values: HIGH deficit (dry) in SW; LOW deficit (moist) in N. Rockies
# Negative values: LOW deficit (moist) in SW; HIGH deficit (dry) in N. Rockies
pc1.raw <- read.table("pc1.txt", header = TRUE, sep=",")
pc1.raw <- pc1.raw %>% rename(value = score1)
pc1.raw <- pc1.raw %>% filter(year > 1983 )#& year <2017) 

## How many years had strong dipole?
count(pc1.raw, value > 1)
count(pc1.raw, value < -1)
12/35


## Generate plot
t <- ggplot() +
  geom_hline(yintercept=0, col = palette[8], linetype="dashed") + 
  geom_line(data = pc1.raw, aes(x = year, y = value), cex = 1) +
  geom_point(data = pc1.raw[abs(pc1.raw$value)>1,],
             aes(x = year, y = value), col = palette[2], cex = 3) +
  labs(x = NULL,
       y = "PC1") +
  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification=c(1,0), 
        legend.position = c(1, 0.33),
        # legend.position = c(0.1, 0.75),
        # legend.position = "bottom",
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.text=element_text(size=12), #angle = 90),
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        plot.margin=unit(c(-1.5,0.5,0.5,0.5),"cm")) # shrink margins for adjacency
dev.off()
t

## Save as png
# png(paste0(out.dir,"pc1_time_series_",currentDate,".png"),
#     width = 550, height = 250, units = "px")
# t
# dev.off()





## To stack plots with aligned width, extract max width from each object.
# Ref: https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange

plots <- list(pc1, t)
grobs <- list()
widths <- list()

## Collect the widths for each grob of each plot
for (l in 1:length(plots)){
  grobs[[l]] <- ggplotGrob(plots[[l]])
  widths[[l]] <- grobs[[l]]$widths[2:5]
}

## Use do.call to get the max width
maxwidth <- do.call(grid::unit.pmax, widths)

## Assign the max width to each grob
for (l in 1:length(grobs)){
  grobs[[l]]$widths[2:5] <- as.list(maxwidth)
}

## Plot
png(paste0(out.dir,"pc1_time_series_",currentDate,".png"),
    width = 550, height = 1000, units = "px")
# do.call("grid.arrange", c(grobs, ncol = 1))
grid.arrange(grobs = grobs, ncol = 1, heights = c(3,1))
dev.off()

  







##################################
##################################
##################################
##################################
##################################
## If decide to plot all together:

# # Some suggest switching to more powerful gtable
# library(gtable)
# lh <- rbind(abla_plot, laoc_plot, pico_plot, size = "first")
# rh <- rbind(pien_plot, pipo_plot, psme_plot, size = "first")
# g <- cbind(lh, rh, size = "first")
# grid.newpage()
# grid.draw(g)
# # grid.arrange(g, legend, ncol = 1) # legend space is way too big
# # install.packages("cowplot")
# library(cowplot)
# currentDate <- Sys.Date()
# png(paste0("estab_all_spei_amjj_",currentDate,".png"),
#     width=900, height =643)
# plot_grid(g, legend, ncol = 1, align = "v", rel_heights = c(3, 0.25))
# dev.off()
