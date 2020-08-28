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
# John's prior version (v2) are 1984-2018. Only loading pc2 in prior version
(pc1_v2 <- nc_open(paste0(wd, "/pc12/eightstate_pc1.nc"))) ; var_v2 <- "PC"
(pc2_v2 <- nc_open(paste0(wd, "/pc12/eightstate_pc2.nc"))) ; var_v2 <- "PC"
# Newer version (v3) are from 1980-2019
(pc1_v3 <- nc_open(paste0(wd, "/pc12.v2/pc1.nc"))) ; var_v3 <- "z"
(pc2_v3 <- nc_open(paste0(wd, "/pc12.v2/pc2.nc"))) ; var_v3 <- "z"
(pc12_v3 <- nc_open(paste0(wd, "/pc12.v2/pc1plus2.nc"))) ; var_v3 <- "z"


## Get lat/long for rasterizing
lon_v2 <- ncvar_get(pc1_v2, "lon") ; dim(lon_v2) ; head(lon_v2)
lat_v2 <- ncvar_get(pc1_v2, "lat") ; dim(lat_v2) ; head(lat_v2)

lon_v3 <- ncvar_get(pc1_v3, "lon") ; dim(lon_v3) ; head(lon_v3) # same for other news
lat_v3 <- ncvar_get(pc1_v3, "lat") ; dim(lat_v3) ; head(lat_v3) # same for other news


## pc2_v2
# Get variable (pc values) and attributes; verify size of array.
pc2_v2.array <- ncvar_get(pc2_v2, var_v2)
# dlname <- ncatt_get(pc2_v2, var_v2, "long_name")
# dunits <- ncatt_get(pc2_v2, var_v2, "units")
fillvalue <- ncatt_get(pc2_v2, var_v2, "_FillValue")
dim(pc2_v2.array) # Only spatial dimensions -- no time.
## Close the NetCDF file and work with array henceforth.
nc_close(pc2_v2)
## Convert NetCDF fill values to NAs (standard for R)
pc2_v2.array[pc2_v2.array == fillvalue$value] <- NA

## pc1_v2
pc1_v2.array <- ncvar_get(pc1_v2, var_v2)
fillvalue <- ncatt_get(pc1_v2, var_v2, "_FillValue")
dim(pc1_v2.array) # Only spatial dimensions -- no time.
nc_close(pc1_v2)
pc1_v2.array[pc1_v2.array == fillvalue$value] <- NA

## pc1_v3
pc1_v3.array <- ncvar_get(pc1_v3, var_v3)
fillvalue <- ncatt_get(pc1_v3, var_v3, "_FillValue")
dim(pc1_v3.array) # 3 dimensions -- inclues time.
nc_close(pc1_v3)
pc1_v3.array[pc1_v3.array == fillvalue$value] <- NA

## pc2_v3
pc2_v3.array <- ncvar_get(pc2_v3, var_v3)
fillvalue <- ncatt_get(pc2_v3, var_v3, "_FillValue")
dim(pc2_v3.array) # 3 dimensions -- inclues time.
nc_close(pc2_v3)
pc2_v3.array[pc1_v3.array == fillvalue$value] <- NA

## pc12_v3
pc12_v3.array <- ncvar_get(pc12_v3, var_v3)
fillvalue <- ncatt_get(pc12_v3, var_v3, "_FillValue")
dim(pc12_v3.array) # 3 dimensions -- inclues time.
nc_close(pc12_v3)
pc12_v3.array[pc1_v3.array == fillvalue$value] <- NA


## Rasterize
# on sides hence t()
# v3 have time dimension, hence brick, then stack (not sure what diff is, but I know stacks).

pc1_v2.r <- raster(t(pc1_v2.array),
                   xmn=min(lon_v2), xmx=max(lon_v2), ymn=min(lat_v2), ymx=max(lat_v2),
                   crs=crs)
plot(pc1_v2.r) #; zoom(pc1_v2.r)

pc2_v2.r <- raster(t(pc2_v2.array),
                     xmn=min(lon_v2), xmx=max(lon_v2), ymn=min(lat_v2), ymx=max(lat_v2),
                     crs=crs)
plot(pc2_v2.r) #; zoom(pc2_v2.r)

## Unclear why, but lat/lon is swapped and it's mirror image.
pc1_v3.r <- brick(pc1_v3.array,
                   # xmn=min(lon_v3), xmx=max(lon_v3),
                   # ymn=min(lat_v3), ymx=max(lat_v3),
                   xmn=min(lat_v3), xmx=max(lat_v3),
                   ymn=min(lon_v3), ymx=max(lon_v3),
                   crs=crs) %>% t() %>% stack()
names(pc1_v3.r) <- paste0("pc1_", c(1980:2019)) 
plot(pc1_v3.r$pc1_1980)

pc2_v3.r <- brick(pc2_v3.array,
                   # xmn=min(lon_v3), xmx=max(lon_v3),
                   # ymn=min(lat_v3), ymx=max(lat_v3),
                   xmn=min(lat_v3), xmx=max(lat_v3),
                   ymn=min(lon_v3), ymx=max(lon_v3),
                   crs=crs) %>% t() %>% stack()
names(pc2_v3.r) <- paste0("pc2_", c(1980:2019)) 
plot(pc2_v3.r$pc2_1980)


pc12_v3.r <- brick(pc12_v3.array,
                    # xmn=min(lon_v3), xmx=max(lon_v3),
                    # ymn=min(lat_v3), ymx=max(lat_v3),
                    xmn=min(lat_v3), xmx=max(lat_v3),
                    ymn=min(lon_v3), ymx=max(lon_v3),
                     crs=crs) %>% t() %>% stack()
names(pc12_v3.r) <- paste0("pc12_", c(1980:2019)) 
plot(pc12_v3.r$pc12_1980)


##########################################################################################
##############################
#### PCA & RECRUIT PROBS #####
##############################

## Load Kim's pipo data; 1981-2015
pipo <- read.csv("pipo_recruitment_probabilities_bysite.csv", header = TRUE, sep = ",")
pipo <- pipo %>% # some non-overlap btwn csvs so only pull out regions here. 
  dplyr::select(Site, year, recruit_prob) %>%
  rename(site = Site, pr = recruit_prob)
range(pipo$year)

## Load Kim's site-specific data.
sites <- read.csv("Davis_et_al_recruitment_plot_data.csv", header = TRUE, sep = ",")
sites <- sites %>%
  dplyr::filter(region == "NR" | region == "SW") %>%
  dplyr::select(site, region, Long_WGS84, Lat_WGS84) %>%
  rename(lon = Long_WGS84, lat = Lat_WGS84) %>%
  unique() # don't need multiple yrs.

pipo <- left_join(sites, pipo, by = "site")

# How many in each region?
pipo %>% dplyr::select(site, region) %>% unique() %>% count(region)

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

## Create averages across sites, too. 
data.avg <- data %>%
  # filter(region == "NR") %>% # NR n = 35
  # filter(region == "SW") %>% # SW n = 35
  group_by(year, region) %>%
  summarise(pr.avg = mean(pr),
            pc1.avg = mean(pc1),
            pc2.avg = mean(pc2),
            pc12.avg = mean(pc12)) %>%
  ungroup()


##########################################################################################
#######################################
#### CORR: RECRUIT & SITE-LEVEL PC ####
#######################################

## Any correlations btwn recruitment & p1, pc2 or pc12 at individ sites?

## NR
corrs_nr <- data %>%
  filter(region == "NR") %>%
  # filter(region == "NR", year > 1983, year < 2016) %>%
  group_by(site) %>%
  summarise(cor_pc1 = cor.test(pc1, pr, method = "spearman")[[4]], # corr coeff is 4th in output list
            p.cor_pc1 = cor.test(pc1, pr, method = "spearman")[[3]], # p-val is 3rd in output list
            cor_pc2 = cor.test(pc2, pr, method = "spearman")[[4]],
            p.cor_pc2 = cor.test(pc2, pr, method = "spearman")[[3]],
            cor_pc12 = cor.test(pc12, pr, method= "spearman")[[4]],
            p.cor_pc12 = cor.test(pc12, pr, method= "spearman")[[3]]) %>%
  ungroup()

## Avg of correlation coefficients across all sites in region
mean(corrs_nr$cor_pc1) ; mean(corrs_nr$p.cor_pc1)
mean(corrs_nr$cor_pc2) ; mean(corrs_nr$p.cor_pc2)
mean(corrs_nr$cor_pc12) ; mean(corrs_nr$p.cor_pc12)



## SW
corrs_sw <- data %>%
  filter(region == "NR") %>%
  # filter(region == "SW", year > 1983, year < 2016) %>%
  group_by(site) %>%
  summarise(cor_pc1 = cor.test(pc1, pr, method = "spearman")[[4]], # corr coeff is 4th in output list
            p.cor_pc1 = cor.test(pc1, pr, method = "spearman")[[3]], # p-val is 3rd in output list
            cor_pc2 = cor.test(pc2, pr, method = "spearman")[[4]],
            p.cor_pc2 = cor.test(pc2, pr, method = "spearman")[[3]],
            cor_pc12 = cor.test(pc12, pr, method= "spearman")[[4]],
            p.cor_pc12 = cor.test(pc12, pr, method= "spearman")[[3]]) %>%
  ungroup()

## Avg of correlation coefficients across all sites in region
mean(corrs_sw$cor_pc1) ; mean(corrs_sw$p.cor_pc1)
mean(corrs_sw$cor_pc2) ; mean(corrs_sw$p.cor_pc2)
mean(corrs_sw$cor_pc12) ; mean(corrs_sw$p.cor_pc12)



##########################################################################################
#####################################################
#### CORR: RECRUIT & AVG'D SITE & DOMAIN-WIDE PC ####
#####################################################

## What's correlation between AVERAGED recruit probs and AVERAGED PC vals across sites?
## What about AVERAGED recruit probs and DOMAIN-WIDE PC vals?

# d_v2 is domain-wide PCA vals based '84-'18 (otherwise hindcast) w/o linear combo of 1&2.
d_v2 <- read.csv("pc_8state.csv", header = TRUE, sep = ",") %>%
  filter(year >= min(data.avg$year),  year <= max(data.avg$year)) %>% dplyr::select(year,pc1, pc2)


## MAke sure they hold up with bootstrapping?
# ref: https://www.datacamp.com/community/tutorials/bootstrap-r

## Define function for correlatio that we want to bootstrap
fun.cor <- function(data, indices){ # dunno what indices is for
  d <-data[indices,]
  c(cor.test(d[,1], d[,2], method = "spearman")[[4]], # corr coeff given as 4th in list from cor.test
    cor.test(d[,1], d[,2], method = "spearman")[[3]]) # p val given as 3rd in list from cor.test
}


dev.off()
par(mfrow=c(2,3))

##**##**##**##**##**##**##**##**##**##**##**##**##**##**##**##**##
## Select NR
temp <- data.avg[data.avg$region == "NR",]

################ NR PC1
# Test correlation; save as object
(c <- cor.test(temp$pr.avg, temp$pc1.avg, method = "spearman"))
# Create linear model of log-transformed recruit probs (b/s right skewed)
(m <- lm(pc1.avg ~ log(pr.avg), data = temp))
# Plot recruit probs and PC vals against one another; add line and r^2
nr1 <- plot(log(temp$pr.avg), temp$pc1.avg,
            col = alpha(pal.ryb[1], 0.6), pch = 19, cex = 2,
            main = "NR recruitment vs. PC1", xlab = "log(recruitment probability)", ylab = "PC1")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")
# Test corr between avg recruit probs and domain-wide pc values
cor.test(temp$pr.avg, d_v2$pc1, method = "spearman")

# bootstrap for PC vals averaged across sites
boo <- as.data.frame(cbind(temp$pr.avg, temp$pc1.avg)) 
(booty <- boot(boo, fun.cor, R=1000)) ; mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# Original gives orig (with full dataset); get with booty$t0; get al new with booty$t
# Bias gives diff btwn mean of bootstrap realizations & original with full dataset.
# Bootstrap Statistics :
#   original      bias    std. error
# t1* -0.6078431373 0.014752475  0.11644482
# t2*  0.0001469972 0.005678678  0.02995994
# [1] -0.5981699
# 95%   (-0.7863, -0.3399 ) 
plot(booty, index = 1) # here, index referes to first output (corr coeff, here)

# # bootstrap for domain-wide pc values
# boo <- as.data.frame(cbind(temp$pr.avg, d_v2$pc1))
# (booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# # Bootstrap Statistics :
# #   original      bias    std. error
# # t1* -0.63529411765 0.014785404  0.10781713
# # t2*  0.00006074087 0.002539554  0.01298061
# # 95%   (-0.8035, -0.3807 )  
# plot(booty, index = 1) # here, index referes to first output (corr coeff, here)




################ NR PC2
### *** NOTE THIS IS MULTIPLIED BY -! *** ###
(c <- cor.test(temp$pr.avg, temp$pc2.avg*-1, method = "spearman"))
(m <- lm(pc2.avg ~ log(pr.avg), data = temp))
nr2 <- plot(log(temp$pr.avg), temp$pc2.avg,
            col = alpha(pal.ryb[1], 0.6), pch = 19, cex = 2,
            main = "NR recruitment vs. PC2", xlab = "log(recruitment probability)", ylab = "PC2")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")
# Test corr between avg recruit probs and domain-wide pc values
cor.test(temp$pr.avg, d_v2$pc2, method = "spearman") # Signate is positive. Corr is weaker.

# bootstrap for PC vals averaged across sites
boo <- as.data.frame(cbind(temp$pr.avg, temp$pc2.avg*-1)) 
(booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
plot(booty, index = 1) # here, index referes to first output (corr coeff, here)
# Bootstrap Statistics :
#   original     bias    std. error
# t1* -0.38543417 0.01577910   0.1520655
# t2*  0.02289188 0.08095964   0.1744236
# [1] -0.3780082
# 95%   (-0.6440, -0.0767 )

# # bootstrap for domain-wide pc values
# boo <- as.data.frame(cbind(temp$pr.avg, d_v2$pc2))
# (booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# # Bootstrap Statistics :
# #   original      bias    std. error
# # t1* 0.2705882 -0.001089782   0.1721986
# # t2* 0.1158756  0.110537209   0.2678702
# # 95%   (-0.1084,  0.5651 )
# plot(booty, index = 1) # here, index referes to first output (corr coeff, here)

################ NR PC12
(c <- cor.test(temp$pr.avg, temp$pc12.avg, method = "spearman"))
(m <- lm(pc12.avg ~ log(pr.avg), data = temp))
nr12 <- plot(log(temp$pr.avg), temp$pc12.avg,
             col = alpha(pal.ryb[1], 0.6), pch = 19, cex = 2,
             main = "NR recruitment vs. PC1&2", xlab = "log(recruitment probability)", ylab = "PC1&2")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")

# bootstrap for PC vals averaged across sites (don't have domain-ide for pc12)
boo <- as.data.frame(cbind(temp$pr.avg, temp$pc12.avg)) 
(booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# Bootstrap Statistics :
#   original     bias    std. error
# t1* -0.718207282913 0.020029709 0.090522876
# t2*  0.000002837194 0.000319954 0.001834278
# [1] -0.701892
# 95%   (-0.8437, -0.4933 )
plot(booty, index = 1) # here, index referes to first output (corr coeff, here)




##**##**##**##**##**##**##**##**##**##**##**##**##**##**##**##**##
## Select SW
temp <- data.avg[data.avg$region == "SW",]

################ SW PC1
(c <- cor.test(temp$pr.avg, temp$pc1.avg, method = "spearman"))
(m <- lm(pc1.avg ~ log(pr.avg), data = temp))
sw1 <- plot(log(temp$pr.avg), temp$pc1.avg,
            col = alpha(pal.ryb[8], 0.6), pch = 19, cex = 2,
            main = "SW recruitment vs. PC1", xlab = "log(recruitment probability)", ylab = "PC1")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")
# Test corr between avg recruit probs and domain-wide pc values
cor.test(temp$pr.avg, d_v2$pc1, method = "spearman")

# bootstrap for PC vals averaged across sites
boo <- as.data.frame(cbind(temp$pr.avg, temp$pc1.avg)) 
(booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# Bootstrap Statistics :
#   original      bias    std. error
# t1* -0.38739496 0.01294236   0.1498376
# t2*  0.02216301 0.07541543   0.1685742
# [1] -0.3683832
# 95%   (-0.6425, -0.0600 ) 
plot(booty, index = 1) # here, index referes to first output (corr coeff, here)

# # bootstrap for domain-wide pc values
# boo <- as.data.frame(cbind(temp$pr.avg, d_v2$pc1))
# (booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# # Bootstrap Statistics :
# #   original      bias    std. error
# # t1* -0.35882353 0.01362393   0.1548825
# # t2*  0.03493142 0.09868127   0.2079373
# # 95%   (-0.6271, -0.0312 )
# plot(booty, index = 1) # here, index referes to first output (corr coeff, here)

################ SW PC2
(c <- cor.test(temp$pr.avg, temp$pc2.avg, method = "spearman"))
(m <- lm(pc2.avg ~ log(pr.avg), data = temp))
sw2 <- plot(log(temp$pr.avg), temp$pc2.avg,
            col = alpha(pal.ryb[8], 0.6), pch = 19, cex = 2,
            main = "SW recruitment vs. PC2", xlab = "log(recruitment probability)", ylab = "PC2")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")
# Test corr between avg recruit probs and domain-wide pc values
cor.test(temp$pr.avg, d_v2$pc2, method = "spearman")

# bootstrap for PC vals averaged across sites
boo <- as.data.frame(cbind(temp$pr.avg, temp$pc2.avg)) 
(booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# Bootstrap Statistics :
#   original     bias    std. error
# t1* -0.455182073 0.003996809   0.1546509
# t2*  0.006475758 0.050959701   0.1379573
# [1] -0.4310694
# 95%   (-0.7093, -0.1191 )
plot(booty, index = 1)

# # bootstrap for domain-wide pc values
# boo <- as.data.frame(cbind(temp$pr.avg, d_v2$pc2))
# (booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# # Bootstrap Statistics :
# #   original      bias    std. error
# # t1* -0.5490196078 0.01278176  0.13954785
# # t2*  0.0007724593 0.01854940  0.07164676
# # 95%   (-0.7720, -0.2177 )  
# plot(booty, index = 1) # here, index referes to first output (corr coeff, here)




################ SW PC12
(c <- cor.test(temp$pr.avg, temp$pc12.avg, method = "spearman"))
(m <- lm(pc12.avg ~ log(pr.avg), data = temp))
sw12 <- plot(log(temp$pr.avg), temp$pc12.avg,
             col = alpha(pal.ryb[8], 0.6), pch = 19, cex = 2,
             main = "SW recruitment vs. PC1&2", xlab = "log(recruitment probability)", ylab = "PC1&2")
abline(m)
legend("topright", legend = paste0("R^2 = ",round(summary(m)$adj.r.squared,4)), bty = "n")

# bootstrap for PC vals averaged across sites (don't have domain-ide for pc12)
boo <- as.data.frame(cbind(temp$pr.avg, temp$pc12.avg)) 
(booty <- boot(boo, fun.cor, R=1000)) ;  mean(booty$t[,1]); boot.ci(booty, index = 1, type = "perc")
# Bootstrap Statistics :
#   original     bias    std. error
# t1* -0.700000000000 0.011167786 0.099842266
# t2*  0.000005674013 0.001009157 0.009965444
# [1] -0.6801437
# 95%   (-0.8413, -0.4429 ) 
plot(booty, index = 1) # here, index referes to first output (corr coeff, here)


# EXPORT FROM PLOT VIEWER TO SAVE.

dev.off()
par(mfrow=c(1,1))






##########################################################################################
###################################
#### MAP DOMAIN-WIDE PC1 & PC2 ####
###################################

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
pc.aea <- projectRaster(pc2_v2.r, crs = aea.proj)


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
                              # title = "PC1 (reg\naverage)") +
                              title = "PC2\n(dipole)") +
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


## What's the trend in PC1?
m <- lm(data=d_v2, pc1 ~ year) ; summary(m)

# install.packages("trend")
library(trend)
sens.slope(d_v2$pc1, conf.level = 0.95)
# z = 0.9657, n = 35, p-value = 0.3342
# Sen's slope 
#  0.02371952 

install.packages("zyp")
library(zyp)
zyp.sen(pc1 ~ year, data=d_v2)
# Theil-Sen slope# -47.67897    0.02372 


##########################################################################################
################################################
#### PLOT TIME SERIES OF DOMAIN-WIDE DIPOLE ####
################################################

## This has region-wide avg as PC1 and our dipole as PC2; clip to min of Kim's dates.
d_v2 <- read.csv("pc_8state.csv", header = TRUE, sep = ",")
d_v2 <- d_v2 %>% filter(year >= min(data.avg$year))

# # Alt: west-wide new version has region-wide avg as PC1 and our dipole ALSO as pc1
# d_v2_w <- read.csv("pc_westwide.csv", header = TRUE, sep = ",")
# d_v2_w <- d_v2_w %>% rename(value = pc1) %>% dplyr::select(year,value)
# d_v2_w <- d_v2_w %>% filter(year > 1980 & year < 2016)


## How many years had strong dipole?
count(d_v2, pc2 > 1) # 7 true
count(d_v2, pc2 < -1) # 5 true
12/35


## Generate plot
# t1 <- ggplot() +
t2 <- ggplot() +
  geom_hline(yintercept=0, col = pal.d2[8], alpha = 0.2, linetype="dashed") +
  
  # For PC1
  # geom_smooth(data = d_v2, aes(x = year, y = pc1), method = "lm",
  #             se = F, col = pal.prgn[7], cex = 0.5, linetype="dashed", alpha = 0.2)  +
  # geom_line(data = d_v2, aes(x = year, y = pc1), cex = 1) +
  

  # For PC2  
  geom_line(data = d_v2, aes(x = year, y = pc2), cex = 1) +
  geom_point(data = d_v2[abs(d_v2$pc2)>1,],
             aes(x = year, y = pc2), col = pal.prgn[7], cex = 1.5) +
  
  labs(x = NULL,
       # y = "PC1") +
       y = "PC2") + # alt: y = expression(PC[dipole])) to get subscript
           
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
        plot.margin=unit(c(0.25,0.5,0.25,0.5),"cm"))  #trbl
dev.off()
t1
t2

## Save as pdf by version
# v <- 1
pdf(paste0(out.dir, "pc1_time_v",v, "_", currentDate,".pdf"),
    width = 4, height = 1.5) ; v <- v+1
t1 ; dev.off()

# v <- 1
pdf(paste0(out.dir, "pc2_time_v",v, "_", currentDate,".pdf"),
    width = 4, height = 1.5) ; v <- v+1
t2
dev.off()





##########################################################################################
################################################
#### PLOT TIME SERIES OF DOMAIN-WIDE DIPOLE ####
################################################

## Note margins are snugged for stacking.

NR <- ggplot() +
  geom_hline(yintercept=0, col = pal.d2[8], alpha = 0.2, linetype="dashed") +
  geom_vline(xintercept=d_v2$year[abs(d_v2$pc2)>1], col = pal.ryb[4], cex = 0.5, linetype="dotted") +
  geom_line(data = d_v2, aes(x = year, y = pc2), cex = 1, col = pal.d2[8], linetype="dashed") +
  geom_line(data = data.avg[data.avg$region == "NR",], aes(x = year, y = pr.avg*4), cex = 1, col = pal.ryb[1]) +
  geom_point(data = d_v2[abs(d_v2$pc2)>1,],
             aes(x = year, y = pc2), col = pal.prgn[7], cex = 2) +
  
  # scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  # add secondary axis: must be 1:1 transformation of primary axis.
  scale_y_continuous(limits = c(-2.25,2.5),
                     sec.axis = sec_axis(~./4, name = "Annual recruitment\nprobability")) +
  theme_bw(base_size = 14) +
  labs(x = NULL, y = "PC2 (dipole)") + #expression(PC[dipole])) + # expression to set subscript of d
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(0.25,0.25,-0.25,0.25),"cm")) +  #trbl; snug top towards NR plot
  annotate("text", x = 1982, y = 2.4, label = "NR", size = 6)
dev.off()
NR




SW <- ggplot() +
  geom_hline(yintercept=0, col = pal.d2[8], alpha = 0.2, linetype="dashed") +
    geom_vline(xintercept=d_v2$year[abs(d_v2$pc2)>1], col = pal.ryb[4], cex = 0.5, linetype="dotted") +
  geom_line(data = d_v2, aes(x = year, y = pc2), cex = 1, col = pal.d2[8], linetype="dashed") +
  geom_line(data = data.avg[data.avg$region == "SW",], aes(x = year, y = pr.avg*4), cex = 1, col = pal.ryb[8]) +
  geom_point(data = d_v2[abs(d_v2$pc2)>1,],
             aes(x = year, y = pc2), col = pal.prgn[7], cex = 2) +

  scale_x_continuous(breaks = seq(1980, 2015, 5)) +
  # add secondary axis: must be 1:1 transformation of primary axis.
  scale_y_continuous(limits = c(-2.25,2.5),
                     sec.axis = sec_axis(~./4, name = "Annual recruitment\nprobability")) +
  theme_bw(base_size = 14) +
  labs(x = NULL, y = "PC2 (dipole)") + #expression(PC[dipole])) + # expression to set subscript of d
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(-0.25,0.25,0.25,0.25),"cm")) +  #trbl; snug top towards NR plot
annotate("text", x = 1982, y = 2.4, label = "SW", size = 6)
dev.off()
SW


# Save as pdf by version
# v <- 1
pdf(paste0(out.dir,"pc_vs_recruitment_v",v,"_",currentDate,".pdf"),
           width = 6, height = 6) ; v <- v+1
# do.call("grid.arrange", c(grobs, ncol = 1))
grid.arrange(NR, SW)
dev.off()


# ##########################################################################################
# #####################################################
# #### ALT PLOT RECRUIT & PCS W/ SITE-LEVEL TRACES ####
# #####################################################
# 
# 
# ## Plot both recruitment and PC with site-specific traces, avgs, AND domain-wide.
# d_v2 <- read.csv("pc_8state.csv", header = TRUE, sep = ",") %>%
#   filter(year >= min(data.avg$year),  year <= max(data.avg$year)) %>% dplyr::select(year,pc1, pc2)
# 
# display.brewer.pal(8, "Dark2")
# display.brewer.pal(8, "RdYlBu")
# display.brewer.pal(8, "PRGn")
# 
# g <- ggplot() +
#   
#   ## Include or don't include original (only have pc1, pc2, not pc12)
#   geom_line(data = d_v2,
#             # aes(x = year, y = pc1), cex = 0.5, col = pal.d2[8], alpha = 0.25, linetype="dotted") +
#             aes(x = year, y = pc2), cex = 0.5, col = pal.d2[8], alpha = 0.25, linetype="dotted") +
#   
#   ########### Select NR or NR
#   geom_line(data = data[data$region == "NR",],
#   # geom_line(data = data[data$region == "SW",],
#             
#             ########### Select which PC
#             # aes(x = year, y = pc1, group = site), cex = 0.5, col = "light grey", alpha = 0.25) +
#             aes(x = year, y = pc2, group = site), cex = 0.5, col = "light grey", alpha = 0.25) +
#             # aes(x = year, y = pc12, group = site), cex = 0.5, col = "light grey", alpha = 0.25) +
#   
#   ########### Select NR or NR
#   geom_line(data = data.avg[data.avg$region == "NR",],
#   # geom_line(data = data.avg[data.avg$region == "SW",],
#             
#             ########### Select which PC
#             # aes(x = year, y = pc1.avg), cex = 0.75, col = "dark grey") +
#             aes(x = year, y = pc2.avg), cex = 0.75, col = "dark grey") +
#             # aes(x = year, y = pc12.avg), cex = 0.75, col = "dark grey") +
#   
#   ########### Select NR or NR
#   geom_line(data = data[data$region == "NR",], aes(x = year, y = pr, group = site), cex = 0.5, col = pal.ryb[3], alpha = 0.25) +
#   # geom_line(data = data[data$region == "SW",], aes(x = year, y = pr, group = site), cex = 0.5, col = pal.ryb[6], alpha = 0.25) +
#   
#   ########### Select NR or NR
#   geom_line(data = data.avg[data.avg$region == "NR",], aes(x = year, y = pr.avg), cex = 0.75, col = pal.ryb[1]) +
#   # geom_line(data = data.avg[data.avg$region == "SW",], aes(x = year, y = pr.avg), cex = 0.75, col = pal.ryb[8]) +
#   
#   
#   scale_x_continuous(breaks = seq(1980, 2016, 5)) +
#   # add secondary axis: must be 1:1 transformation of primary axis.
#   scale_y_continuous(limits = c(-2,2.5),
#                      sec.axis = sec_axis(~./4, name = "Annual recruitment probability")) +
#   theme_bw(base_size = 14) +
#   labs(x = NULL, y = "PC") + #expression(PC[dipole])) + # expression to set subscript of d
#   theme(axis.ticks.x=element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.margin=unit(c(0.25,0.25,0.25,0.25),"cm")) + #trbl; snug bottom towards SW plot
#   
#   ########### Select NR or NR and which PC
#   # annotate("text", x = 1985, y = 2.4, label = "NR & PC1", size = 6) ; name <- "NRvPC1"
#   annotate("text", x = 1985, y = 2.4, label = "NR & PC2", size = 6) ; name <- "NRvPC2"
#   # annotate("text", x = 1985, y = 2.4, label = "NR & PC12", size = 6) ; name <- "NRvPC12"
#   # annotate("text", x = 1985, y = 2.4, label = "SW & PC1", size = 6) ; name <- "SWvPC1"
# # annotate("text", x = 1985, y = 2.4, label = "SW & PC2", size = 6) ; name <- "SWvPC2"
# # annotate("text", x = 1985, y = 2.4, label = "SW & PC12", size = 6) ; name <- "SWvPC12"
# dev.off()
# g
# # pdf(paste0(out.dir,name,".pdf"), width = 6, height = 3.5)
# g
# dev.off()


