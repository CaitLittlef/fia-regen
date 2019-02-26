data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
data.pipo$YEAR.DIFF <- as.numeric(data.pipo$YEAR.DIFF)
data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
levels(data.pipo$regen_pipo)[2] # gives 1 for yes regen

moo <- data.pipo[data.pipo$YEAR.DIFF > 4,]
moo <- moo[moo$BALive_pipo > 0,]

p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = moo,
             aes(x = LON_FS, y = LAT_FS, col = regen_pipo)) +
  scale_color_manual(values = c("blue", "yellow"),
                     labels = c("no regen", "regen"))
p

# tiff(paste0(out.dir,"pipo_fail_map_",currentDate,".tiff"),
#       width = 640, height = 480, units = "px")
# p
# dev.off()



#########################################PSME
data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]
data.psme$YEAR.DIFF <- as.numeric(data.psme$YEAR.DIFF)
data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
levels(data.psme$regen_psme)[2] # gives 1 for yes regen

doo <- data.psme[data.psme$YEAR.DIFF > 4,]
doo <- doo[doo$BALive_psme > 0,]

p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = doo,
             aes(x = LON_FS, y = LAT_FS, col = regen_psme)) +
  scale_color_manual(values = c("blue", "yellow"),
                     labels = c("no regen", "regen"))
p
# tiff(paste0(out.dir,"psme_fail_map_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
# p
# dev.off()















sts = c('cali', 'oreg', 'wash','idaho','monta','wyo','utah','ariz','new mex','colo')
moo <- data.pipo %>%
  filter(regen_pipo == 0,
         YEAR.DIFF > 5) %>%
  mutate(prop_ba_pipo = BALive_pipo/BALiveTot)
moo$prop_ba_pipo[is.na(moo$prop_ba_pipo)] <- 0
map('state', region = sts)
points(moo$LON_FS, moo$LAT_FS, cex = 3*moo$prop_ba_pipo)
legend("bottomleft",c("100% PIPO BA","10% PIPO BA"),pch=c(1,1), pt.cex=c(3,0.3),bty='n')


## Fancy from https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#general-ggplot2-theme-for-map


## Get background of W states
NAmer <- st_read(dsn = "//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/boundaries/NorthAmer_StatesProvinces.shp") %>% 
  st_buffer(dist = 0) # fix invalid geometries (warning re: lat/long vs. dd)
sts = c('California', 'Oregon', 'Washington','Idaho', 'Nevada',
        'Montana','Wyoming','Utah','Arizona','New Mexico','Colorado')
Wsts <- NAmer[NAmer$NAME %in% sts, ] # not NAmer@data$NAME
plot(Wsts) # Get multiple b/c multiple attributes
plot(st_geometry(Wsts)) # Just outline
crs(Wsts) # "+proj=longlat +datum=NAD83 +no_defs"


## geom_sf seems to work.... others online suggest geom_polygon...
# but that requires fortification of data first to make it "tidy"
# see fortify() which is now deprecated... new: broom::tidy()
# ^ n.b., broom::tidy() should make model outputs simpler
# http://varianceexplained.org/r/broom-intro/
p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = as.data.frame(coords), aes(x = lon, y = lat))
p

## Try to animate based on decline in regen likelihood
install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)


p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = as.data.frame(moo), aes(x = LON_FS, y = LAT_FS,
                                            size = prop_ba_pipo))
p


p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = as.data.frame(moo), aes(x = LON_FS, y = LAT_FS,
                                            size = prop_ba_pipo))+
  transition_time(YEAR.DIFF) + 
  ease_aes("linear")
p

p <- ggplot() +
  geom_sf(data = Wsts) +
  geom_point(data = as.data.frame(moo), aes(x = LON_FS, y = LAT_FS,
                                            size = prop_ba_pipo),
                                            alpha = 0.25)
p + transition_time(YEAR.DIFF) +
  labs(title = "Time since fire: {frame_time}") +
  ease_aes("linear") +
  shadow_mark(alpha = 0.3, size = 0.5) # Shadow of mark remains
p



install.packages("magick")
library(magick)
#> Linking to ImageMagick 6.9.9.39
#> Enabled features: cairo, fontconfig, freetype, lcms, pango, rsvg, webp
#> Disabled features: fftw, ghostscript, x11
image <- animate(p)
image_write(image, paste0(out.dir,"animation_",currentDate,".gif"))
# ^ doesn't work
animate(p, nframes = 24, renderer = gifski_renderer(paste0(out.dir,"animation_",currentDate,".gif")))


https://stackoverflow.com/questions/49155038/how-to-save-frames-of-gif-created-using-gganimate-package


https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/#general-ggplot2-theme-for-map
  
  https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
  



map should have all plots to start that then gradually fade away as regen occurs, time being years since fire (not actual year). What remains are all plots that have no regen.