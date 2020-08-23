##########################################################
##########################################################
## ** COMPARE PC values to recruitment probabilities ** ##
##########################################################
##########################################################





######################
#### SETUP: PCAS #####
######################

# ref:
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm
# install.packages("ncdf4")
library(ncdf4)

## Open connection to dataset & print info to see var names.
# var names for z-scores are diff across .nc files
# John's prior version are 1984-2018.
# (pc2_v2 <- nc_open(paste0(wd, "/pc12/eightstate_pc2.nc"))) ; var_orig <- "PC"
# New are from 1980-2019
(pc1_v3 <- nc_open(paste0(wd, "/pc12.v2/pc1.nc"))) ; var_new <- "z"
(pc2_v3 <- nc_open(paste0(wd, "/pc12.v2/pc2.nc"))) ; var_new <- "z"
(pc12_v3 <- nc_open(paste0(wd, "/pc12.v2/pc1plus2.nc"))) ; var_new <- "z"


## Get lat/long for rasterizing
# lon_orig <- ncvar_get(pc2_v2, "lon") ; dim(lon_orig) ; head(lon_orig)
# lat_orig <- ncvar_get(pc2_v2, "lat") ; dim(lat_orig) ; head(lat_orig)

lon_new <- ncvar_get(pc1_v3, "lon") ; dim(lon_new) ; head(lon_new) # same for other news
lat_new <- ncvar_get(pc1_v3, "lat") ; dim(lat_new) ; head(lat_new) # same for other news

# ## pc2_v2
# # Get variable (pc values) and attributes; verify size of array.
# pc2_v2.array <- ncvar_get(pc2_v2, var_orig)
# # dlname <- ncatt_get(pc2_v2, var_orig, "long_name")
# # dunits <- ncatt_get(pc2_v2, var_orig, "units")
# fillvalue <- ncatt_get(pc2_v2, var_orig, "_FillValue")
# dim(pc2_v2.array) # Only spatial dimensions -- no time.
# ## Close the NetCDF file and work with array henceforth.
# nc_close(pc2_v2)
# ## Convert NetCDF fill values to NAs (standard for R)
# pc2_v2.array[pc2_v2.array == fillvalue$value] <- NA

## pc1_v3
pc1_v3.array <- ncvar_get(pc1_v3, var_new)
fillvalue <- ncatt_get(pc1_v3, var_new, "_FillValue")
dim(pc1_v3.array) # 3 dimensions -- inclues time.
nc_close(pc1_v3)
pc1_v3.array[pc1_v3.array == fillvalue$value] <- NA

## pc2_v3
pc2_v3.array <- ncvar_get(pc2_v3, var_new)
fillvalue <- ncatt_get(pc2_v3, var_new, "_FillValue")
dim(pc2_v3.array) # 3 dimensions -- inclues time.
nc_close(pc2_v3)
pc2_v3.array[pc1_v3.array == fillvalue$value] <- NA

## pc12_v3
pc12_v3.array <- ncvar_get(pc12_v3, var_new)
fillvalue <- ncatt_get(pc12_v3, var_new, "_FillValue")
dim(pc12_v3.array) # 3 dimensions -- inclues time.
nc_close(pc12_v3)
pc12_v3.array[pc1_v3.array == fillvalue$value] <- NA


## Rasterize
# on sides hence t(); v3 have time dimension, hence brick, then stack (not sure what diff is, but I know stacks).
# pc2_v2.r <- raster(t(pc2_v2.array),
#                      xmn=min(lon_orig), xmx=max(lon_orig), ymn=min(lat_orig), ymx=max(lat_orig),
#                      crs=crs) 
# plot(pc2_v2.r) #; zoom(pc2_v2.r)

## Unclear why, but lat/lon is swapped and it's mirror image.
pc1_v3.r <- brick(pc1_v3.array,
                   # xmn=min(lon_new), xmx=max(lon_new),
                   # ymn=min(lat_new), ymx=max(lat_new),
                   xmn=min(lat_new), xmx=max(lat_new),
                   ymn=min(lon_new), ymx=max(lon_new),
                   crs=crs) %>% t() %>% stack()
names(pc1_v3.r) <- paste0("pc1_", c(1980:2019)) 
plot(pc1_v3.r$pc1_1980)

pc2_v3.r <- brick(pc2_v3.array,
                   # xmn=min(lon_new), xmx=max(lon_new),
                   # ymn=min(lat_new), ymx=max(lat_new),
                   xmn=min(lat_new), xmx=max(lat_new),
                   ymn=min(lon_new), ymx=max(lon_new),
                   crs=crs) %>% t() %>% stack()
names(pc2_v3.r) <- paste0("pc2_", c(1980:2019)) 
plot(pc2_v3.r$pc2_1980)


pc12_v3.r <- brick(pc12_v3.array,
                    # xmn=min(lon_new), xmx=max(lon_new),
                    # ymn=min(lat_new), ymx=max(lat_new),
                    xmn=min(lat_new), xmx=max(lat_new),
                    ymn=min(lon_new), ymx=max(lon_new),
                     crs=crs) %>% t() %>% stack()
names(pc12_v3.r) <- paste0("pc12_", c(1980:2019)) 
plot(pc12_v3.r$pc12_1980)



##############################
#### PCA & RECRUIT PROBS #####
##############################

## Load Kim's pipo data
pipo <- read.csv("pipo_recruitment_probabilities_bysite.csv", header = TRUE, sep = ",")
pipo <- pipo %>%
  # dplyr::filter(region == "NR" | region == "SW") %>% # some non-overlap btwn csvs so don't pull out region in
  dplyr::select(Site, year, recruit_prob) %>%
  rename(site = Site, pr = recruit_prob)

## Load Kim's site-specific data.
sites <- read.csv("Davis_et_al_recruitment_plot_data.csv", header = TRUE, sep = ",")
sites <- sites %>%
  dplyr::filter(region == "NR" | region == "SW") %>%
  dplyr::select(site, region, Long_WGS84, Lat_WGS84) %>%
  rename(lon = Long_WGS84, lat = Lat_WGS84) %>%
  unique() # don't need multiple yrs.

pipo <- left_join(sites, pipo, by = "site")
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
  for(y in 1:nlayers(pc1_v3.r)){ # number of rasters = number of yrs
    a <- as.numeric(raster::extract(pc1_v3.r[[y]], sites[p,3:4])) # extract value at site's lat/lon (4:5)
    pc1.temps <- c(pc1.temps, a)
    b <- as.numeric(raster::extract(pc2_v3.r[[y]], sites[p,3:4])) # extract value at site's lat/lon (4:5)
    pc2.temps <- c(pc2.temps, b)
    c <- as.numeric(raster::extract(pc12_v3.r[[y]], sites[p,3:4])) # extract value at site's lat/lon (4:5)
    pc12.temps <- c(pc12.temps, c)
    d <- paste(sites[p,1]) # save site name
    site.temps <- c(site.temps, d)
    e <- right(names(pc12_v3.r[[y]]),4) # save yr; same for all rasters.
    year.temps <- c(year.temps, e)
  }
}
val <- matrix(data = NA, nrow = nrow(sites)*nlayers(pc1_v3.r), ncol = 5) # empty matrix to fill with vals
val[,1] <- pc1.temps
val[,2] <- pc2.temps
val[,3] <- pc12.temps
val[,4] <- site.temps
val[,5] <- year.temps

# Convert to df, set numbers to numeric
vals <- as.data.frame(val) ; colnames(vals) <- c("pc1","pc2", "pc12", "site", "year")
cols.num <- c("pc1","pc2", "pc12", "year")
vals[cols.num] <- sapply(vals[cols.num], as.numeric)


## Re-join with recruitment probabilities, only where there's overlap (in site and year)
data <- vals %>% inner_join(pipo, by = c("site", "year"))

data.avg <- data %>%
  # filter(region == "NR") %>% # NR n = 35
  # filter(region == "SW") %>% # SW n = 35
  group_by(year, region) %>%
  summarise(pr.avg = mean(pr),
            pc1.avg = mean(pc1),
            pc2.avg = mean(pc2),
            pc12.avg = mean(pc12)) %>%
  ungroup()



#######################################
#### TEST CORRS BTWN RECRUIT & PCS ####
#######################################

## Any correlations btwn recruitment & p1, pc2 or pc12 at individ sites?

## NR
corrs_nr <- data %>%
  filter(region == "NR") %>%
  # filter(region == "NR", year > 1983, year < 2016) %>%
  group_by(site) %>%
  summarise(cor_pc1 = cor.test(pc1, pr, method = "spearman")[[4]], # corr coeff given as 4th in list from cor.test
            p.cor_pc1 = cor.test(pc1, pr, method = "spearman")[[3]], # p-val given as 3rd in list from cor.test
            cor_pc2 = cor.test(pc2, pr, method = "spearman")[[4]],
            p.cor_pc2 = cor.test(pc2, pr, method = "spearman")[[3]],
            cor_pc12 = cor.test(pc12, pr, method= "spearman")[[4]],
            p.cor_pc12 = cor.test(pc12, pr, method= "spearman")[[3]]) %>%
  ungroup()

## Avg of correlation coefficients across all sites in region
mean(corrs_nr$cor_pc1) ; mean(corrs_nr$p.cor_pc1)
mean(corrs_nr$cor_pc2) ; mean(corrs_nr$p.cor_pc2)
mean(corrs_nr$cor_pc12) ; mean(corrs_nr$p.cor_pc12)

## Plots of recruitment against PCs at all sites in region
# plot(data$pc1[data$region == "NR"], data$pr[data$region == "NR"])
# plot(data$pc2[data$region == "NR"], data$pr[data$region == "NR"])
# plot(data$pc12[data$region == "NR"], data$pr[data$region == "NR"])



## SW
corrs_sw <- data %>%
  filter(region == "NR") %>%
  # filter(region == "SW", year > 1983, year < 2016) %>%
  group_by(site) %>%
  summarise(cor_pc1 = cor.test(pc1, pr, method = "spearman")[[4]], # corr coeff given as 4th in list from cor.test
            p.cor_pc1 = cor.test(pc1, pr, method = "spearman")[[3]], # p-val given as 3rd in list from cor.test
            cor_pc2 = cor.test(pc2, pr, method = "spearman")[[4]],
            p.cor_pc2 = cor.test(pc2, pr, method = "spearman")[[3]],
            cor_pc12 = cor.test(pc12, pr, method= "spearman")[[4]],
            p.cor_pc12 = cor.test(pc12, pr, method= "spearman")[[3]]) %>%
  ungroup()

## Avg of correlation coefficients across all sites in region
mean(corrs_sw$cor_pc1) ; mean(corrs_sw$p.cor_pc1)
mean(corrs_sw$cor_pc2) ; mean(corrs_sw$p.cor_pc2)
mean(corrs_sw$cor_pc12) ; mean(corrs_sw$p.cor_pc12)

## Plots of recruitment against PCs at all sites in region
# plot(data$pc1[data$region == "SW"], data$pr[data$region == "SW"])
# plot(data$pc2[data$region == "SW"], data$pr[data$region == "SW"])
# plot(data$pc12[data$region == "SW"], data$pr[data$region == "SW"])



#############################################################
#### TEST RELATIONSIHP BTWN AVG RECRUIT & AVG PCS & PLOT ####
#############################################################

## Any relationship btwn average recruitment per region and average pc values??

## Test correlations & plot between avg pr across sites and avg pc values; incl. prior dipole version.
# # d_v2 is John's prior PCA based '84-'18 (otherwise hindcast) w no linear combo of 1&2.
# This time series is domain-wide and we do NOT extract values to individual sites.
d_v2 <- read.csv("pc_8state.csv", header = TRUE, sep = ",") %>%
  filter(year >= min(data.avg$year),  year <= max(data.avg$year)) %>% dplyr::select(year,pc1, pc2)

## Log-transform to spread out; add simple linear model to show trend. Show r^2 from mod summary.
# Save by exporting what's in view.

par(mfrow=c(2,3))

##*********
## NR
(c <- cor.test(data.avg$pr.avg[data.avg$region == "NR"], data.avg$pc1.avg[data.avg$region == "NR"]))
(m <- lm(pc1.avg ~ log(pr.avg), data = data.avg[data.avg$region == "NR",]))
nr1 <- plot(log(data.avg$pr.avg[data.avg$region == "NR"]), data.avg$pc1.avg[data.avg$region == "NR"],
            col = alpha(pal.ryb[1], 0.6), pch = 19, cex = 2,
            main = "NR recruitment vs. PC1", xlab = "log(recruitment probability)", ylab = "PC1")
abline(m)
# text(0.4, max(data.avg$pc1.avg[data.avg$region == "NR"])-0.2,
#      paste0("r = ",round(c[[4]],4),"\np val = ",round(c[[3]],4)))
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")
# old, domain-wide pc values
cor.test(data.avg$pr.avg[data.avg$region == "NR"], d_v2$pc1)


(c <- cor.test(data.avg$pr.avg[data.avg$region == "NR"], data.avg$pc2.avg[data.avg$region == "NR"]))
(m <- lm(pc2.avg ~ log(pr.avg), data = data.avg[data.avg$region == "NR",]))
nr2 <- plot(log(data.avg$pr.avg[data.avg$region == "NR"]), data.avg$pc2.avg[data.avg$region == "NR"],
            col = alpha(pal.ryb[1], 0.6), pch = 19, cex = 2,
            main = "NR recruitment vs. PC2", xlab = "log(recruitment probability)", ylab = "PC2")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")
# old, domain-wide pc values
cor.test(data.avg$pr.avg[data.avg$region == "NR"], d_v2$pc2)


(c <- cor.test(data.avg$pr.avg[data.avg$region == "NR"], data.avg$pc12.avg[data.avg$region == "NR"]))
(m <- lm(pc12.avg ~ log(pr.avg), data = data.avg[data.avg$region == "NR",]))
nr12 <- plot(log(data.avg$pr.avg[data.avg$region == "NR"]), data.avg$pc12.avg[data.avg$region == "NR"],
             col = alpha(pal.ryb[1], 0.6), pch = 19, cex = 2,
             main = "NR recruitment vs. PC1&2", xlab = "log(recruitment probability)", ylab = "PC1&2")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")



##**##**##**##**##
##SW
(c <- cor.test(data.avg$pr.avg[data.avg$region == "SW"], data.avg$pc1.avg[data.avg$region == "SW"]))
(m <- lm(pc1.avg ~ log(pr.avg), data = data.avg[data.avg$region == "SW",]))
sw1 <- plot(log(data.avg$pr.avg[data.avg$region == "SW"]), data.avg$pc1.avg[data.avg$region == "SW"],
            col = alpha(pal.ryb[8], 0.6), pch = 19, cex = 2,
            main = "SW recruitment vs. PC1", xlab = "log(recruitment probability)", ylab = "PC1")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")
# old, domain-wide pc values
cor.test(data.avg$pr.avg[data.avg$region == "SW"], d_v2$pc1)


(c <- cor.test(data.avg$pr.avg[data.avg$region == "SW"], data.avg$pc2.avg[data.avg$region == "SW"]))
(m <- lm(pc2.avg ~ log(pr.avg), data = data.avg[data.avg$region == "SW",]))
sw2 <- plot(log(data.avg$pr.avg[data.avg$region == "SW"]), data.avg$pc2.avg[data.avg$region == "SW"],
            col = alpha(pal.ryb[8], 0.6), pch = 19, cex = 2,
            main = "SW recruitment vs. PC2", xlab = "log(recruitment probability)", ylab = "PC2")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")
# old, domain-wide pc values
cor.test(data.avg$pr.avg[data.avg$region == "SW"], d_v2$pc2)


(c <- cor.test(data.avg$pr.avg[data.avg$region == "SW"], data.avg$pc12.avg[data.avg$region == "SW"]))
(m <- lm(pc12.avg ~ log(pr.avg), data = data.avg[data.avg$region == "SW",]))
sw12 <- plot(log(data.avg$pr.avg[data.avg$region == "SW"]), data.avg$pc12.avg[data.avg$region == "SW"],
             col = alpha(pal.ryb[8], 0.6), pch = 19, cex = 2,
             main = "SW recruitment vs. PC1&2", xlab = "log(recruitment probability)", ylab = "PC1&2")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")

dev.off()
par(mfrow=c(1,1))





############################
#### PLOT RECRUIT & PCS ####
############################


## Plot both recruitment and dipole. Incl. domain-wide (defined for 1984-2018) AND site-specifi PC vals averaged
d_v2 <- read.csv("pc_8state.csv", header = TRUE, sep = ",") %>%
  filter(year >= min(data.avg$year),  year <= max(data.avg$year)) %>% dplyr::select(year,pc1, pc2)

display.brewer.pal(8, "Dark2")
display.brewer.pal(8, "RdYlBu")
display.brewer.pal(8, "PRGn")

g <- ggplot() +
  
  ## Include or don't include original (only have pc1, pc2, not pc12)
  geom_line(data = d_v2,
            aes(x = year, y = pc1), cex = 0.5, col = pal.d2[8], alpha = 0.25, linetype="dotted") +
            # aes(x = year, y = pc2), cex = 0.5, col = pal.d2[8], alpha = 0.25, linetype="dotted") +
  
  ########### Select NR or NR
  # geom_line(data = data[data$region == "NR",],
  geom_line(data = data[data$region == "SW",],
            
            ########### Select which PC
            aes(x = year, y = pc1, group = site), cex = 0.5, col = "light grey", alpha = 0.25) +
            # aes(x = year, y = pc2, group = site), cex = 0.5, col = "light grey", alpha = 0.25) +
            # aes(x = year, y = pc12, group = site), cex = 0.5, col = "light grey", alpha = 0.25) +
  
  ########### Select NR or NR
  # geom_line(data = data.avg[data.avg$region == "NR",],
  geom_line(data = data.avg[data.avg$region == "SW",],
            
            ########### Select which PC
            aes(x = year, y = pc1.avg), cex = 0.75, col = "dark grey") +
            # aes(x = year, y = pc2.avg), cex = 0.75, col = "dark grey") +
            # aes(x = year, y = pc12.avg), cex = 0.75, col = "dark grey") +
  
  ########### Select NR or NR
  # geom_line(data = data[data$region == "NR",], aes(x = year, y = pr, group = site), cex = 0.5, col = pal.ryb[3], alpha = 0.25) +
  geom_line(data = data[data$region == "SW",], aes(x = year, y = pr, group = site), cex = 0.5, col = pal.ryb[6], alpha = 0.25) +
  
  ########### Select NR or NR
  # geom_line(data = data.avg[data.avg$region == "NR",], aes(x = year, y = pr.avg), cex = 0.75, col = pal.ryb[1]) +
  geom_line(data = data.avg[data.avg$region == "SW",], aes(x = year, y = pr.avg), cex = 0.75, col = pal.ryb[8]) +
  
  
  scale_x_continuous(breaks = seq(1980, 2016, 5)) +
  # add secondary axis: must be 1:1 transformation of primary axis.
  scale_y_continuous(limits = c(-2,2.5),
                     sec.axis = sec_axis(~./4, name = "Annual recruitment probability")) +
  theme_bw(base_size = 14) +
  labs(x = NULL, y = "PC") + #expression(PC[dipole])) + # expression to set subscript of d
  theme(axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm")) + #trbl; snug bottom towards SW plot
  
  ########### Select NR or NR and which PC
  # annotate("text", x = 1985, y = 2.4, label = "NR & PC1", size = 6) ; name <- "NRvPC1"
  # annotate("text", x = 1985, y = 2.4, label = "NR & PC2", size = 6) ; name <- "NRvPC2"
  # annotate("text", x = 1985, y = 2.4, label = "NR & PC12", size = 6) ; name <- "NRvPC12"
  annotate("text", x = 1985, y = 2.4, label = "SW & PC1", size = 6) ; name <- "SWvPC1"
  # annotate("text", x = 1985, y = 2.4, label = "SW & PC2", size = 6) ; name <- "SWvPC2"
  # annotate("text", x = 1985, y = 2.4, label = "SW & PC12", size = 6) ; name <- "SWvPC12"
  dev.off()
g
# pdf(paste0(out.dir,name,".pdf"), width = 6, height = 3.5)
g
dev.off()



#######################
#### MAP PC1 & PC2 ####
#######################

## Goal: project maps (Vs planar ggplot). Folks recommend ggplot-similar tmap.
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


################################################
#### PLOT TIME SERIES OF DOMAIN-WIDE DIPOLE ####
################################################

## This has region-wide avg as PC1 and our dipole as PC2; clip to Kim's dates
d_v2 <- read.csv("pc_8state.csv", header = TRUE, sep = ",")
d_v2 <- d_v2 %>% rename(value = pc1) %>% dplyr::select(year,value)
d_v2 <- d_v2 %>% rename(value = pc1) %>% dplyr::select(year,value)
d_v2 <- d_v2 %>% filter(year > 1980 )#& year < 2016)

# # Alt: west-wide new version has region-wide avg as PC1 and our dipole ALSO as pc1
# d_v2_w <- read.csv("pc_westwide.csv", header = TRUE, sep = ",")
# d_v2_w <- d_v2_w %>% rename(value = pc1) %>% dplyr::select(year,value)
# d_v2_w <- d_v2_w %>% filter(year > 1980 & year < 2016)


## How many years had strong dipole?
count(d_v2, value > 1) # 7 true
count(d_v2, value < -1) # 5 true
12/35

count(d_v2, value > 1.5) # 2 true
count(d_v2, value < -1.5) # 3 true



## Generate plot
t <- ggplot() +
  geom_hline(yintercept=0, col = pal.d2[8], linetype="dashed") + 
  geom_line(data = d_v2, aes(x = year, y = value), cex = 1) +
  geom_point(data = d_v2[abs(d_v2$value)>1,],
             aes(x = year, y = value), col = pal.prgn[7], cex = 2) +
  labs(x = NULL,
       # y = "PC1") +
       y = expression(PC[dipole])) + # expression to set subscript of d
    scale_x_continuous(breaks = seq(1980, 2015, 5)) +
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
d_v2 <- read.csv("pc_8state.csv", header = TRUE, sep = ",")
# d_v2 <- d_v2 %>% rename(value = pc1) %>% dplyr::select(year,value)
d_v2 <- d_v2 %>% rename(value = pc2) %>% dplyr::select(year,value)


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

# d_v2 <- d_v2 %>% filter(year>1980 & year<2016)
d_v2 <- d_v2 %>% filter(year>1983 & year<2016)
# d_v2 <- d_v2 %>% filter(year>1983 & year<2016 & year!=1997)
range(d_v2$year)

## Any correlations btwn recruitment & dipoles?
# cor.test(pipo_nr$pr, d_v1$value, method = "spearman")
cor.test(pipo_nr$pr, d_v2$value, method = "spearman")
# cor.test(pipo_nr$pr, d_v2_w$value, method = "spearman")

temp <- data.avg %>% filter(region == "NR", year > 1983, year < 2016)
cor.test(temp$pr.avg, d_v2$value, method = "spearman")

# cor.test(pipo_sw$pr, d_v1$value, method = "spearman")
cor.test(pipo_sw$pr, d_v2$value, method = "spearman")
# cor.test(pipo_sw$pr, d_v2_w$value, method = "spearman") 

temp <- data.avg %>% filter(region == "SW", year > 1983, year < 2016)
cor.test(temp$pr.avg, d_v2$value, method = "spearman")

# ## NR break-point 1996
# cor.test(pipo_nr$pr[pipo_nr$year < 1996], d_v2$value[d_v2$year < 1996],
#          method = "spearman")
# cor.test(pipo_nr$pr[pipo_nr$year > 1995], d_v2$value[d_v2$year > 1995],
#          method = "spearman")
# ## SW break-point 1991
# cor.test(pipo_sw$pr[pipo_sw$year < 1992], d_v2$value[d_v2$year < 1992],
#          method = "spearman")
# cor.test(pipo_sw$pr[pipo_sw$year > 1991], d_v2$value[d_v2$year > 1991],
#          method = "spearman")





####################################### BOOTSTRAP ###################################
# ref: https://www.datacamp.com/community/tutorials/bootstrap-r


## Define function for correlatio that we want to bootstrap
fun.cor <- function(data, indices){ # dunno what indices is for
  d <-data[indices,]
  c(cor.test(d[,1], d[,2], method = "spearman")[[4]], # corr coeff given as 4th in list from cor.test
    cor.test(d[,1], d[,2], method = "spearman")[[3]]) # p val given as 3rd in list from cor.test
}



############
## NR
data <- as.data.frame(cbind(pipo_nr, d_v2)) %>% dplyr::select(pr, value)
booty <- boot(data, fun.cor, R=1000)

booty

## orig gives orig (with full dataset), bias gives diff btwn mean of bootstrap realizations & orig. 
# Bootstrap Statistics :
#     original    bias          std. error
# t1* 0.34310850 -0.006477764   0.1830347
# t2* 0.05515087  0.107969570   0.2340709

booty$t0 # gives orig
booty$t # gives all new

plot(booty, index = 1) # here, index referes to first output (corr coeff, here)
plot(booty, index = 2) # here, index refers to p-val

boot.ci(booty, index = 1, type = "perc")



############
## SW
data <- as.data.frame(cbind(pipo_sw, d_v2)) %>% dplyr::select(pr, value)
booty <- boot(data, fun.cor, R=1000)

booty

## orig gives orig (with full dataset), bias gives diff btwn mean of bootstrap realizations & orig. 
# Bootstrap Statistics :
#   original     bias    std. error
# t1* -0.540689150 0.01027947  0.13975005
# t2*  0.001644087 0.02136227  0.07223205

booty$t0 # gives orig
booty$t # gives all new

plot(booty, index = 1) # here, index referes to first output (corr coeff, here)
plot(booty, index = 2) # here, index refers to p-val

boot.ci(booty, index = 1, type = "perc")















##########################################################################################


## Plot both recruitment and dipole
NR <- ggplot() +
  geom_vline(xintercept=d_v2$year[abs(d_v2$value)>1], col = pal.ryb[4], cex = 0.5, linetype="dotted") +
  geom_line(data = d_v2, aes(x = year, y = value), cex = 1, col = pal.d2[8], linetype="dashed") +
  geom_line(data = pipo[pipo$region == "NR",], aes(x = year, y = pr*4), cex = 1, col = pal.ryb[1]) +
  geom_point(data = d_v2[abs(d_v2$value)>1,],
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
    geom_vline(xintercept=d_v2$year[abs(d_v2$value)>1], col = pal.ryb[4], cex = 0.5, linetype="dotted") + geom_line(data = d_v2, aes(x = year, y = value), cex = 1, col = pal.d2[8], linetype="dashed") +
  geom_line(data = pipo[pipo$region == "SW",], aes(x = year, y = pr*4), cex = 1, col = pal.ryb[8]) +
  geom_point(data = d_v2[abs(d_v2$value)>1,],
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
#   geom_line(data = d_v2, aes(x = year, y = value), cex = 1, col = palette[8], linetype="dashed") +
#   geom_point(data = d_v2[abs(d_v2$value)>1,],
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

