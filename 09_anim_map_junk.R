######################################################## MAP IT
# Map should have all plots then gradually fade as regen occurs, with time since fire (not actual year).
# What remains are all plots that have no regen.
# Alt: Alternate between plots that have and don't have regen after 10 years.
# Trying to animate... to no avail.

## Try to animate based on decline in regen likelihood
install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)




p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = as.data.frame(coords), aes(x = lon, y = lat)) +
  theme_map()
p

moo$time <- as.numeric(as.character(moo$regen_pipo))+1
p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts) +
  geom_point(data = as.data.frame(moo), aes(x = LON_FS, y = LAT_FS,
                                            # group = regen_pipo,
                                            color = regen_pipo),
             size = 5, alpha = 0.7) +
  theme_map()
p


## Trying to slow frames down
anim <- p + transition_states(regen_pipo, # will be based on group, here regen_pipo
                              transition_length = 0, # nix this so labels line up with state
                              state_length = 1) +
  # shadow_mark(alpha = 0.3) + # would leave shadow, but 2-state wrap makes weird 
  # enter_fade() + 
  # exit_shrink() + # would make points shrink
  ggtitle('Now showing {closest_state}')


animate(anim, fps=50, renderer= gifski_renderer(paste0(out.dir,"animation_",currentDate,".gif"))) # Transition/state lenghts are relative. Here, specify frames/sec... 




## Saving stuff
install.packages("magick")
library(magick)
#> Linking to ImageMagick 6.9.9.39
#> Enabled features: cairo, fontconfig, freetype, lcms, pango, rsvg, webp
#> Disabled features: fftw, ghostscript, x11
image <- animate(p)
image_write(image, paste0(out.dir,"animation_",currentDate,".gif"))
# ^ doesn't work
animate(p, nframes = 24, renderer = gifski_renderer(paste0(out.dir,"animation_",currentDate,".gif")))



## Possible mtbs map
fire <- st_read("//goshawk.sefs.uw.edu/Space_Lawler/Shared/BackedUp/Caitlin/NW CASC/Dir/WA_Large_Fires/wa_lrg_fires.shp")
fire <- fire[,c("FIRENAME", "YEAR")]
class(fire) # sfdf
plot(st_geometry(fire))
plot(st_geometry(fire[fire$FIRENAME == "Tripod (Tripod Complex)" ,]))

min(fire$YEAR)
max(fire$YEAR)

yr.vec <- c(1973, 2005, 2010, 2015, 2020)
fire$FIRE.BIN <- findInterval(fire$YEAR, vec=yr.vec, rightmost.closed=TRUE)


p <- ggplot() +
  # state outlines
  geom_sf(data = Wsts[Wsts$NAME == "Washington",]) +
  geom_sf(data = fire, aes(fill = FIRE.BIN)) +
  scale_fill_gradient(low = "#56B1F7", high = "#f79c56")
p

?scale_color_brewer 

scale_color_manual(values = c("yellow", "orange", "light green", "dark green"),
                   labels = c("3: <BA <CMD", "4: <BA >CMD", "6: >BA <CMD", "7: >BA >CMD"))
p