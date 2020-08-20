## Extract and plot PC1, PC2, and their linear combo.
# PC1 is region-wide avg. PC2 is dipole and explains ~21%.

# ref:
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm
# install.packages("ncdf4")
library(ncdf4)


#### LOAD & SETUP #####################################################3
## Open connection to dataset & print info to see var names.
# var names for z-scores are diff across .nc files
(pc2_orig <- nc_open(paste0(wd, "/pc12/eightstate_pc2.nc"))) ; var_orig <- "PC"
# New are from 1980-2019
(pc1_new <- nc_open(paste0(wd, "/pc12.v2/pc1.nc"))) ; var_new <- "z"
(pc2_new <- nc_open(paste0(wd, "/pc12.v2/pc2.nc"))) ; var_new <- "z"
(pc12_new <- nc_open(paste0(wd, "/pc12.v2/pc1plus2.nc"))) ; var_new <- "z"


## Get lat/long for rasterizing
lon_orig <- ncvar_get(pc2_orig, "lon") ; dim(lon_orig) ; head(lon_orig)
lat_orig <- ncvar_get(pc2_orig, "lat") ; dim(lat_orig) ; head(lat_orig)

lon_new <- ncvar_get(pc1_new, "lon") ; dim(lon_new) ; head(lon_new) # same for other news
lat_new <- ncvar_get(pc1_new, "lat") ; dim(lat_orig) ; head(lat_orig) # same for other news


## PC2_ORIG
## Get variable (pc values) and attributes; verify size of array.
pc2_orig.array <- ncvar_get(pc2_orig, var_orig)
# dlname <- ncatt_get(pc2_orig, var_orig, "long_name")
# dunits <- ncatt_get(pc2_orig, var_orig, "units")
fillvalue <- ncatt_get(pc2_orig, var_orig, "_FillValue")
dim(pc2_orig.array) # Only spatial dimensions -- no time.
## Close the NetCDF file and work with array henceforth.
nc_close(pc2_orig)
## Convert NetCDF fill values to NAs (standard for R)
pc2_orig.array[pc2_orig.array == fillvalue$value] <- NA

## PC1_NEW
pc1_new.array <- ncvar_get(pc1_new, var_new)
fillvalue <- ncatt_get(pc1_new, var_new, "_FillValue")
dim(pc1_new.array) # 3 dimensions -- inclues time.
nc_close(pc1_new)
pc1_new.array[pc2_orig.array == fillvalue$value] <- NA

## PC2_NEW
pc2_new.array <- ncvar_get(pc2_new, var_new)
fillvalue <- ncatt_get(pc2_new, var_new, "_FillValue")
dim(pc2_new.array) # 3 dimensions -- inclues time.
nc_close(pc2_new)
pc2_new.array[pc2_orig.array == fillvalue$value] <- NA

## PC12_NEW
pc12_new.array <- ncvar_get(pc12_new, var_new)
fillvalue <- ncatt_get(pc12_new, var_new, "_FillValue")
dim(pc12_new.array) # 3 dimensions -- inclues time.
nc_close(pc12_new)
pc12_new.array[pc2_orig.array == fillvalue$value] <- NA


## Rasterize
# orig was on it's side hence t()
# news have time dimension, hence brick, then stack (not sure what diff is, but I know stacks).
pc2_orig.r <- raster(t(pc2_orig.array),
                     xmn=min(lon_orig), xmx=max(lon_orig), ymn=min(lat_orig), ymx=max(lat_orig),
                     crs=crs) 
plot(pc2_orig.r) #; zoom(pc2_orig.r)

## Unclear why, but lat/lon is swapped and it's mirror image.
pc1_new.r <- brick(pc1_new.array,
                   # xmn=min(lon_new), xmx=max(lon_new),
                   # ymn=min(lat_new), ymx=max(lat_new),
                   xmn=min(lat_new), xmx=max(lat_new),
                   ymn=min(lon_new), ymx=max(lon_new),
                   crs=crs) %>% t() %>% stack()
names(pc1_new.r) <- paste0("pc1_", c(1980:2019)) 
plot(pc1_new.r$pc1_1980)

pc2_new.r <- brick(pc2_new.array,
                   # xmn=min(lon_new), xmx=max(lon_new),
                   # ymn=min(lat_new), ymx=max(lat_new),
                   xmn=min(lat_new), xmx=max(lat_new),
                   ymn=min(lon_new), ymx=max(lon_new),
                   crs=crs) %>% t() %>% stack()
names(pc2_new.r) <- paste0("pc2_", c(1980:2019)) 
plot(pc2_new.r$pc2_1980)


pc12_new.r <- brick(pc12_new.array,
                    # xmn=min(lon_new), xmx=max(lon_new),
                    # ymn=min(lat_new), ymx=max(lat_new),
                    xmn=min(lat_new), xmx=max(lat_new),
                    ymn=min(lon_new), ymx=max(lon_new),
                     crs=crs) %>% t() %>% stack()
names(pc12_new.r) <- paste0("pc12_", c(1980:2019)) 
plot(pc12_new.r$pc12_1980)



################KIM'S DATA#####################3

pipo <- read.csv("pipo_recruitment_probabilities_bysite.csv", header = TRUE, sep = ",")
# pipo_nr <- pipo %>% dplyr::filter(region == "NR") %>% dplyr::select(Site, year, recruit_prob) %>% rename(site = Site)
# pipo_sw <- pipo %>% dplyr::filter(region == "SW") %>% dplyr::select(Site, year, recruit_prob) %>% rename(site = Site)
pipo <- pipo %>% dplyr::select(Site, year, recruit_prob) %>% rename(site = Site)

sites <- read.csv("Davis_et_al_recruitment_plot_data.csv", header = TRUE, sep = ",") %>%
  dplyr::filter(region == "NR" | region == "SW") %>%
  dplyr::select(site, Long_WGS84, Lat_WGS84) %>% rename(lon = Long_WGS84, lat = Lat_WGS84) %>%
  unique() # don't need multiple yrs.

pipo <- left_join(pipo, sites, by = "site")
# pipo_nr <- left_join(pipo_nr, deets, by = "site")
# pipo_sw <- left_join(pipo_sw, deets, by = "site")


## Extract values for each site and each year from 3 new rasters.
# create empty vectors to drop values into
pc1.temps <- vector()
pc2.temps <- vector() 
pc12.temps <- vector() 
site.temps <- vector() 
year.temps <- vector() 

for(p in 1:nrow(sites)){ # for each site
  for(y in 1:nlayers(pc1_new.r)){ # number of rasters = number of yrs
    a <- as.numeric(raster::extract(pc1_new.r[[y]], sites[p,2:3])) # extract value at site's lat/lon (4:5)
    pc1.temps <- c(pc1.temps, a)
    b <- as.numeric(raster::extract(pc2_new.r[[y]], sites[p,2:3])) # extract value at site's lat/lon (4:5)
    pc2.temps <- c(pc2.temps, b)
    c <- as.numeric(raster::extract(pc12_new.r[[y]], sites[p,2:3])) # extract value at site's lat/lon (4:5)
    pc12.temps <- c(pc12.temps, c)
    d <- paste(sites[p,1]) # save site name
    site.temps <- c(site.temps, d)
    e <- right(names(pc12_new.r[[y]]),4) # save yr; same for all rasters.
    year.temps <- c(year.temps, e)
  }
}
val <- matrix(data = NA, nrow = nrow(sites)*nlayers(pc1_new.r), ncol = 5) # empty matrix to fill with vals
val[,1] <- pc1.temps
val[,2] <- pc2.temps
val[,3] <- pc12.temps
val[,4] <- site.temps
val[,5] <- year.temps

vals <- as.data.frame(val, col.names = c("pc1","pc2", "pc12", "site", "year"))




################non-ggplot maps#####################3
## Goal: project maps. Folks recommend ggplot-similar tmap.
# refs:
# https://geocompr.robinlovelace.net/adv-map.html
# see colors with this: 
# tmaptools::palette_explorer()

display.brewer.pal(8, "Dark2") ; pal.d2
display.brewer.pal(8, "RdYlBu") ; pal.ryb
display.brewer.pal(8, "PiYG") ; pal.pg
display.brewer.pal(8, "PRGn") ; pal.prgn



# install.packages("tmap")
# install.packages("shinyjs")
library(tmap)
library(shinyjs)

# FIXME: cannot add coordaintes. crs still stuck in m and tm_grid shows as such.
# tm_graticules, which should add coords doesn't seem to exist anymore.


## Pick Albers equal area projection & transform all data.
aea.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83  +units=m"

IntWsts.aea <- st_transform(IntWsts, crs = aea.proj)
nonIntWest.aea <- st_transform(nonIntWest, crs = aea.proj)
pc.aea <- projectRaster(pc.r, crs = aea.proj)


## I'll want to control bounding box of map.
# Use IntW, expanded slightly.
# https://www.jla-data.net/eng/adjusting-bounding-box-of-a-tmap-map/
(bbox <- st_bbox(IntWsts.aea))
bbox_new <- bbox
bbox_new[1] <- (bbox[1] - 20000) #xmin 
bbox_new[3] <- (bbox[3] + 20000) #xmas
bbox_new[2] <- (bbox[2] - 20000) #ymin
bbox_new[4] <- (bbox[4] + 20000) #ymax

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon


## Create map. Cannot figure out how to get negative values on bottom in legend. Fix in pwrpt
# Or not show hill legend -- so no hill.
map <- # by default, set master to establish bbox, projection, etc (default = 1st raster)
  tm_shape(IntWsts.aea, is.master = TRUE, bbox = bbox_new) + # master to rule bbox, proj
  tm_fill("grey40") + # for holes in raster
  # add in hillshade for study area first with continuous grey gradient
  # tm_shape(hill.aea) + tm_raster(palette = "Greys", style = "cont") +
  # add in deficit values with reverse palette; may make transparent with alpha
  tm_shape(pc.aea) + tm_raster(palette = "-RdYlBu",
                              style = "cont",
                              title = "PC1 (reg\naverage)") +
                              # title = "PC2\n(dipole)") +
  # add in non-Interior West states with light grey fill
  tm_shape(nonIntWest.aea) + tm_borders(lwd=1.5) + tm_fill("gray90") +
  # add in Interior West states with no fill
  tm_shape(IntWsts.aea) + tm_borders(lwd=1.5) + 
  tm_layout(legend.bg.color = "white",
            legend.frame = TRUE,
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.show = TRUE,
            # legend.only = TRUE,
            # legend.outside = TRUE,
            legend.position = c(0.01, 0.01)) ; map


# tmap_arrange(map, legend)

## Save as pdf by version
# v <- 1
pdf(paste0(out.dir, "pc_map_v",v, "_", currentDate,".pdf"),
    width = 3, height = 4) ; v <- v+1
map
dev.off()

###########################################################################



################# Time series of PC-dipole

## John's orig (pc1.txt) had not considered region-wide avg and jumped righ to dipole.
# Positive values: HIGH deficit (dry) in SW; LOW deficit (moist) in N. Rockies
# Negative values: LOW deficit (moist) in SW; HIGH deficit (dry) in N. Rockies
# d_orig <- read.table("pc1.txt", header = TRUE, sep=",")
# d_orig <- d_orig %>% rename(value = score1)
# d_orig <- d_orig %>% filter(year > 1980 & year < 2016)

# New version has region-wide avg as PC1 and our dipole as PC2
d_new <- read.csv("pc_8state.csv", header = TRUE, sep = ",")
d_new <- d_new %>% rename(value = pc2) %>% dplyr::select(year,value)
d_new <- d_new %>% filter(year > 1983 )#& year < 2016)

# # Alt: west-wide new version has region-wide avg as PC1 and our dipole ALSO as pc1
# d_new_w <- read.csv("pc_westwide.csv", header = TRUE, sep = ",")
# d_new_w <- d_new_w %>% rename(value = pc1) %>% dplyr::select(year,value)
# d_new_w <- d_new_w %>% filter(year > 1980 & year < 2016)


## How many years had strong dipole?
count(d_new, value > 1) # 7 true
count(d_new, value < -1) # 5 true
12/35

count(d_new, value > 1.5) # 2 true
count(d_new, value < -1.5) # 3 true



## Generate plot
t <- ggplot() +
  geom_hline(yintercept=0, col = pal.d2[8], linetype="dashed") + 
  geom_line(data = d_new, aes(x = year, y = value), cex = 1) +
  geom_point(data = d_new[abs(d_new$value)>1,],
             aes(x = year, y = value), col = pal.prgn[7], cex = 2) +
  labs(x = NULL,
       # y = "PC1") +
       y = expression(PC[dipole])) + # expression to set subscript of d
    scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # axis.title.y = element_text(hjust = 0.5),
        legend.justification=c(1,0), 
        legend.position = c(1, 0.33),
        # legend.position = c(0.1, 0.75),
        # legend.position = "bottom",
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.text=element_text(size=12), #angle = 90),
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        plot.margin=unit(c(0.25,0.5,0.25,0.5),"cm")) #+ #trbl
  # annotate("text", x = 1982, y = 2.25, label = "b", size = 6)
dev.off()
t

## Save as png
# png(paste0(out.dir,"pc1_time_series_",currentDate,".png"),
#     width = 550, height = 250, units = "px")
# t
# dev.off()


## Save as pdf by version
# v <- 1
pdf(paste0(out.dir, "pc_time_v",v, "_", currentDate,".pdf"),
    width = 4, height = 1.5) ; v <- v+1
t
dev.off()




## To stack plots with aligned width, extract max width from each object.
# Ref: https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange

plots <- list(pc, t)
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
png(paste0(out.dir,"pc_time_series_",currentDate,".png"),
    width = 550, height = 1000, units = "px")
pdf(paste0(out.dir,"pc_time_series_",currentDate,".pdf"))
# do.call("grid.arrange", c(grobs, ncol = 1))
grid.arrange(grobs = grobs, ncol = 1, heights = c(5,2))
dev.off()

  

################### Correlate PCdipole w/ Kim's recruitment rates

# New version has region-wide avg as PC1 and our dipole as PC2
d_new <- read.csv("pc_8state.csv", header = TRUE, sep = ",")
d_new <- d_new %>% rename(value = pc2) %>% dplyr::select(year,value)


## Kim's recruitment data.
# Her modeled projections run from '81-'15. Dipole is defined '84-'18 (and otherwise hindcast)
# Clip both to ensure we're only using "real" years for both.
pipo <- read.csv("PIPO_recruitment_prob.csv", header = TRUE, sep = ",")
# pipo_nr <- pipo[,1:3] %>% dplyr::filter(region == "NR") %>% dplyr::select(-region)
pipo_nr <- pipo[,1:3] %>% dplyr::filter(region == "NR", year > 1983) %>% dplyr::select(-region)
# pipo_nr <- pipo[,1:3] %>% dplyr::filter(region == "NR", year > 1983) %>%
#   dplyr::filter(region == "NR", year != 1997) %>%
#   dplyr::select(-region)

range(pipo_nr$year)

pipo <- read.csv("PIPO_recruitment_prob.csv", header = TRUE, sep = ",")
# pipo_sw <- pipo[,1:3] %>% dplyr::filter(region == "SW") %>% dplyr::select(-region)
pipo_sw <- pipo[,1:3] %>% dplyr::filter(region == "SW", year > 1983) %>% dplyr::select(-region)
range(pipo_sw$year)

pipo <- pipo[,1:3] %>% filter(year > 1983)

# d_new <- d_new %>% filter(year>1980 & year<2016)
d_new <- d_new %>% filter(year>1983 & year<2016)
# d_new <- d_new %>% filter(year>1983 & year<2016 & year!=1997)
range(d_new$year)

## Any correlations btwn recruitment & dipoles?
# cor.test(pipo_nr$pr, d_orig$value, method = "spearman")
cor.test(pipo_nr$pr, d_new$value, method = "spearman")
# cor.test(pipo_nr$pr, d_new_w$value, method = "spearman")

# cor.test(pipo_sw$pr, d_orig$value, method = "spearman")
cor.test(pipo_sw$pr, d_new$value, method = "spearman")
# cor.test(pipo_sw$pr, d_new_w$value, method = "spearman") 


## NR break-point 1996
cor.test(pipo_nr$pr[pipo_nr$year < 1996], d_new$value[d_new$year < 1996],
         method = "spearman")
cor.test(pipo_nr$pr[pipo_nr$year > 1995], d_new$value[d_new$year > 1995],
         method = "spearman")
## SW break-point 1991
cor.test(pipo_sw$pr[pipo_sw$year < 1992], d_new$value[d_new$year < 1992],
         method = "spearman")
cor.test(pipo_sw$pr[pipo_sw$year > 1991], d_new$value[d_new$year > 1991],
         method = "spearman")


## Plot both recruitment and dipole
NR <- ggplot() +
    geom_vline(xintercept=d_new$year[abs(d_new$value)>1], col = pal.ryb[4], cex = 0.5, linetype="dotted") + geom_line(data = d_new, aes(x = year, y = value), cex = 1, col = pal.d2[8], linetype="dashed") +
  geom_line(data = pipo[pipo$region == "NR",], aes(x = year, y = pr*4), cex = 1, col = pal.ryb[1]) +
  geom_point(data = d_new[abs(d_new$value)>1,],
             aes(x = year, y = value), col = pal.prgn[7], cex = 2) +

  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  # add secondary axis: must be 1:1 transformation of primary axis.
  scale_y_continuous(limits = c(-2.5,2.5),
                     sec.axis = sec_axis(~./4, name = "Annual recruitment\nprobability")) +
  theme_bw(base_size = 14) +
  labs(x = NULL, y = expression(PC[dipole])) + # expression to set subscript of d
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0.25,0.25,-0.25,0.25),"cm")) +  #trbl; snug bottom towards SW plot
  annotate("text", x = 1984, y = 2.4, label = "NR", size = 6)
dev.off()
NR


SW <- ggplot() +
    geom_vline(xintercept=d_new$year[abs(d_new$value)>1], col = pal.ryb[4], cex = 0.5, linetype="dotted") + geom_line(data = d_new, aes(x = year, y = value), cex = 1, col = pal.d2[8], linetype="dashed") +
  geom_line(data = pipo[pipo$region == "SW",], aes(x = year, y = pr*4), cex = 1, col = pal.ryb[8]) +
  geom_point(data = d_new[abs(d_new$value)>1,],
             aes(x = year, y = value), col = pal.prgn[7], cex = 2) +

  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  # add secondary axis: must be 1:1 transformation of primary axis.
  scale_y_continuous(limits = c(-2.5,2.5),
                     sec.axis = sec_axis(~./4, name = "Annual recruitment\nprobability")) +
  theme_bw(base_size = 14) +
  labs(x = NULL, y = expression(PC[dipole])) + # expression to set subscript of d
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(-0.25,0.25,0.25,0.25),"cm")) +  #trbl; snug top towards NR plot
annotate("text", x = 1984, y = 2.4, label = "SW", size = 6)
dev.off()
SW


# Save as pdf by version
# v <- 1
# png(paste0(out.dir,"pc_vs recruitment_",currentDate,".png"),
#     width = 550, height = 450, units = "px")
pdf(paste0(out.dir,"pc_vs_recruitment_v",v,"_",currentDate,".pdf"),
           width = 6, height = 6) ; v <- v+1
# do.call("grid.arrange", c(grobs, ncol = 1))
grid.arrange(NR, SW)
dev.off()

# Or jut consider exporting from plot viewer; shrink from 8.5x11 to 8.5x6

###########################################################

## Plot both recruitment and dipole, facet by region
# t <- ggplot() +
#   # geom_hline(yintercept=0, col = palette[8], linetype="dashed") + 
#   geom_line(data = pipo, aes(x = year, y = pr*3, col = region), cex = 1) +
#   scale_fill_manual(c(palette[5], palette[6]), lab = NULL) +
#   facet_wrap(~region, ncol = 1) + 
#   geom_line(data = d_new, aes(x = year, y = value), cex = 1, col = palette[8], linetype="dashed") +
#   geom_point(data = d_new[abs(d_new$value)>1,],
#              aes(x = year, y = value), col = palette[2], cex = 2) +
#   labs(x = NULL,
#        # y = "PC1") +
#        y = expression(PC[dipole])) + # expression to set subscript of d
#   scale_x_continuous(breaks = seq(1985, 2015, 5)) +
#   scale_y_continuous(sec.axis = sec_axis(~./3), name = "test") +
#   theme_bw(base_size = 18) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         # axis.title.y = element_text(hjust = 0.5),
#         legend.justification=c(1,0), 
#         legend.position = "none",
#         plot.margin=unit(c(0.25,0.5,0.25,0.5),"cm"))  #trbl
# # annotate("text", x = 1982, y = 2.25, label = "b", size = 6)
# dev.off()
# t
# 

