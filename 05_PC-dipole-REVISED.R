## Extract and plot PC1 and PC2 data of CMD dipole from John A.
# PC1 is region-wide avg. PC2 is dipole and explains ~21%.

# ref:
# http://geog.uoregon.edu/bartlein/courses/geog607/Rmd/netCDF_01.htm
# install.packages("ncdf4")
library(ncdf4)


#### LOAD & SETUP #####################################################3
## Open connection to dataset & print info.
ncin <- nc_open(paste0(wd, "/pc12/eightstate_pc2.nc"))
print(ncin) # Shows variable name is pc2 of May-Sep CWD
var <- "PC"


## Get lat/long
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "lat")
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))


## Time variable is not included in this package. So skip below. 
# ## Get time var and attributes; also number of times w/ dim().
# # Given that there are 2 times, it may be PC1 and PC2?
# # FYI, if this was time, convert from time-since (e.g., 1900) to date with chron package.
# t <- ncvar_get(ncin, "time")
# tunits <- ncatt_get(ncin, "time", "units")
# nt <- dim(t)



## Get variable (pc values) and attributes; verify size of array.
pc.array <- ncvar_get(ncin, var)
dlname <- ncatt_get(ncin, var, "long_name")
dunits <- ncatt_get(ncin, var, "units")
fillvalue <- ncatt_get(ncin, var, "_FillValue")
dim(pc.array) # Only spatial dimensions -- no time.



## Close the NetCDF file and work with array henceforth.
nc_close(ncin)


#### SET TO RASTER #####################################################3

## Convert NetCDF fill values to NAs (standard for R)
pc.array[pc.array == fillvalue$value] <- NA
# Check for total number of non-missing (non-NA) values
class(pc.array)
min(pc.array, na.rm=T)
max(pc.array, na.rm=T)


## Rasterize; # was on it's side hence t()
pc.r <- raster(t(pc.array), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs)

plot(pc.r)



#### PLOT #####################################################3

## Convert raster data to dataframe for plotting
pc.data <- gplot_data(pc.r)


# What should the limits when plotting be?
min(pc.data$value[is.finite(pc.data$value)], na.rm =TRUE) # -0.6473524
max(pc.data$value[is.finite(pc.data$value)], na.rm =TRUE) # 0.8484244

display.brewer.pal(8, "Dark2")
dev.off()
par(mfrow=c(1,1))
pc <- ggplot() +
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
  annotate("text", x = -120.5, y = 49.5, label = "a", size = 6)
dev.off()
pc





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
d_new <- d_new %>% filter(year > 1980 )#& year < 2016)

# # Alt: west-wide new version has region-wide avg as PC1 and our dipole ALSO as pc1
# d_new_w <- read.csv("pc_westwide.csv", header = TRUE, sep = ",")
# d_new_w <- d_new_w %>% rename(value = pc1) %>% dplyr::select(year,value)
# d_new_w <- d_new_w %>% filter(year > 1980 & year < 2016)


## How many years had strong dipole?
count(d_new, value > 1)
count(d_new, value < -1)
12/38

count(d_new, value > 1.5)
count(d_new, value < -1.5)
12/38


## Generate plot
t <- ggplot() +
  geom_hline(yintercept=0, col = palette[8], linetype="dashed") + 
  geom_line(data = d_new, aes(x = year, y = value), cex = 1) +
  geom_point(data = d_new[abs(d_new$value)>1,],
             aes(x = year, y = value), col = palette[2], cex = 2) +
  labs(x = NULL,
       # y = "PC1") +
       y = expression(PC[dipole])) + # expression to set subscript of d
    scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  theme_bw(base_size = 18) +
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
        plot.margin=unit(c(0.25,0.5,0.25,0.5),"cm")) + #trbl
  annotate("text", x = 1982, y = 2.25, label = "b", size = 6)
dev.off()
t

## Save as png
# png(paste0(out.dir,"pc1_time_series_",currentDate,".png"),
#     width = 550, height = 250, units = "px")
# t
# dev.off()





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
# Her modeled projections run from '81-'15. That yields weak/no correlations.
# BUT dipole is defined '84-'18. If I clip both to only overlap, get correlations.
pipo <- read.csv("PIPO_recruitment_prob.csv", header = TRUE, sep = ",")
# pipo_nr <- pipo[,1:3] %>% dplyr::filter(region == "NR") %>% dplyr::select(-region)
pipo_nr <- pipo[,1:3] %>% dplyr::filter(region == "NR", year > 1983) %>% dplyr::select(-region)
range(pipo_nr$year)

pipo <- read.csv("PIPO_recruitment_prob.csv", header = TRUE, sep = ",")
# pipo_sw <- pipo[,1:3] %>% dplyr::filter(region == "SW") %>% dplyr::select(-region)
pipo_sw <- pipo[,1:3] %>% dplyr::filter(region == "SW", year > 1983) %>% dplyr::select(-region)
range(pipo_sw$year)

pipo <- pipo[,1:3] %>% filter(year > 1983)

# d_new <- d_new %>% filter(year>1980 & year<2016)
d_new <- d_new %>% filter(year>1983 & year<2016)
range(d_new$year)

## Any correlations btwn recruitment & dipoles?
# cor.test(pipo_nr$pr, d_orig$value, method = "spearman")
cor.test(pipo_nr$pr, d_new$value, method = "spearman")
# cor.test(pipo_nr$pr, d_new_w$value, method = "spearman")

# cor.test(pipo_sw$pr, d_orig$value, method = "spearman")
cor.test(pipo_sw$pr, d_new$value, method = "spearman")
# cor.test(pipo_sw$pr, d_new_w$value, method = "spearman") 


## NR break-point 1996
# cor.test(pipo_nr$pr[pipo_nr$year < 1996], d_orig$value[d_orig$year < 1996], method = "spearman")
# cor.test(pipo_nr$pr[pipo_nr$year < 1996], d_new$value[d_orig$year < 1996], method = "spearman")
# cor.test(pipo_nr$pr[pipo_nr$year < 1996], d_new_w$value[d_orig$year < 1996], method = "spearman")
# 
# cor.test(pipo_nr$pr[pipo_nr$year > 1995], d_orig$value[d_orig$year > 1995], method = "spearman")
# cor.test(pipo_nr$pr[pipo_nr$year > 1995], d_new$value[d_orig$year > 1995], method = "spearman")
# cor.test(pipo_nr$pr[pipo_nr$year > 1995], d_new_w$value[d_orig$year > 1995], method = "spearman")
# 
# 
# 
# ## SW break-point 1991
# cor.test(pipo_sw$pr[pipo_sw$year < 1992], d_orig$value[d_orig$year < 1992], method = "spearman")
# cor.test(pipo_sw$pr[pipo_sw$year < 1992], d_new$value[d_orig$year < 1992], method = "spearman")
# cor.test(pipo_sw$pr[pipo_sw$year < 1992], d_new_w$value[d_orig$year < 1992], method = "spearman")
# 
# cor.test(pipo_sw$pr[pipo_sw$year > 1991], d_orig$value[d_orig$year > 1991], method = "spearman")
# cor.test(pipo_sw$pr[pipo_sw$year > 1991], d_new$value[d_orig$year > 1991], method = "spearman")
# cor.test(pipo_sw$pr[pipo_sw$year > 1991], d_new_w$value[d_orig$year > 1991], method = "spearman")


## Plot both recruitment and dipole
NR <- ggplot() +
  geom_line(data = d_new, aes(x = year, y = value), cex = 1, col = palette[8], linetype="dashed") +
  geom_line(data = pipo[pipo$region == "NR",], aes(x = year, y = pr*4), cex = 1, col = palette[5]) +
  geom_point(data = d_new[abs(d_new$value)>1,],
             aes(x = year, y = value), col = palette[2], cex = 2) +
  geom_vline(xintercept=d_new$year[abs(d_new$value)>1], col = palette[2], cex = 0.5, linetype="dotted") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  # add secondary axis: must be 1:1 transformation of primary axis.
  scale_y_continuous(limits = c(-2.5,2.5),
                     sec.axis = sec_axis(~./4, name = "Annual recruitment\nprobability")) +
  theme_bw(base_size = 18) +
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
  geom_line(data = d_new, aes(x = year, y = value), cex = 1, col = palette[8], linetype="dashed") +
  geom_line(data = pipo[pipo$region == "SW",], aes(x = year, y = pr*4), cex = 1, col = palette[6]) +
  geom_point(data = d_new[abs(d_new$value)>1,],
             aes(x = year, y = value), col = palette[2], cex = 2) +
  geom_vline(xintercept=d_new$year[abs(d_new$value)>1], col = palette[2], cex = 0.5, linetype="dotted") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5)) +
  # add secondary axis: must be 1:1 transformation of primary axis.
  scale_y_continuous(limits = c(-2.5,2.5),
                     sec.axis = sec_axis(~./4, name = "Annual recruitment\nprobability")) +
  theme_bw(base_size = 18) +
  labs(x = NULL, y = expression(PC[dipole])) + # expression to set subscript of d
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(-0.25,0.25,0.25,0.25),"cm")) +  #trbl; snug top towards NR plot
annotate("text", x = 1984, y = 2.4, label = "SW", size = 6)
dev.off()
SW




png(paste0(out.dir,"pc_vs recruitment_",currentDate,".png"),
    width = 550, height = 450, units = "px")
pdf(paste0(out.dir,"pc_vs recruitment_",currentDate,".pdf"))
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

