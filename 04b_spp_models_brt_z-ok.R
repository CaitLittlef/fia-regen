## Show that 3-year max deficit anomaly, as retained in final model, doesn't have trend.

colnames(data.pipo) # 521
colnames(data.psme) # 692

# Select relevant columns
temp.pipo <- data.pipo %>%
  dplyr::select(UNIQUEID, def59_z_max13, FIRE.YR.x, YEAR.DIFF, INVYR)
temp.psme <- data.psme %>%
  dplyr::select(UNIQUEID, def59_z_max13, FIRE.YR.x, YEAR.DIFF, INVYR)

# Join into one dataset; remove extraneous cols (w/ dupe values)
goo <- full_join(temp.pipo, temp.psme, by = "UNIQUEID") # 989
goo <- goo %>%
  transmute(def59_z_max13 = ifelse(is.na(def59_z_max13.x), def59_z_max13.y, def59_z_max13.x),
         FIRE.YR = ifelse(is.na(FIRE.YR.x.x), FIRE.YR.x.y, FIRE.YR.x.x),
         YEAR.DIFF = ifelse(is.na(YEAR.DIFF.x), YEAR.DIFF.y, YEAR.DIFF.x),
         INVYR = ifelse(is.na(INVYR.x), INVYR.y, INVYR.x),
         UNIQUEID = UNIQUEID) # to retain b/c transmute drops

summary(goo$YEAR.DIFF)
summary(goo$FIRE.YR)
summary(goo$INVYR)


# Plot
yrVz <- ggplot(data = goo, aes(x = YEAR.DIFF, y = def59_z_max13)) +
  geom_jitter(width = 0.5, height = 0.0, size = 0.5) +
  # geom_point() +
  labs (x = "Years since fire", y = "3-year maximum\n deficit anomaly") +
  scale_x_continuous(breaks=seq(0,30,5)) + 
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
yrVz

frVz <- ggplot(data = goo, aes(x = FIRE.YR, y = def59_z_max13)) +
  geom_jitter(width = 0.5, height = 0.0, size = 0.5) +
  # geom_point() +
  labs (x = "Year of fire", y = "3-year maximum\n deficit anomaly") +
  scale_x_continuous(breaks=seq(1980,2015,5)) + 
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
frVz

plots <- list(yrVz, frVz)
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
png(paste0(out.dir, "z_vs_yrs_", currentDate, ".png"),
    width = 7, height = 5, units = "in", res = 300)
do.call("grid.arrange", c(grobs, ncol = 1, nrow = 2))
dev.off()
