## Show that 3-year max deficit anomaly, as retained in final model, doesn't have trend.


currentDate <- Sys.Date()


colnames(data.pipo) # 521
colnames(data.psme) # 692

# Select relevant columns
temp.pipo <- data.pipo %>%
  dplyr::select(UNIQUEID, def59_z_0, def59_z_1, def59_z_max13, FIRE.YR.x, YEAR.DIFF, INVYR)
temp.psme <- data.psme %>%
  dplyr::select(UNIQUEID, def59_z_0, def59_z_1, def59_z_max13, FIRE.YR.x, YEAR.DIFF, INVYR)

# Join into one dataset; remove extraneous cols (w/ dupe values)
goo <- full_join(temp.pipo, temp.psme, by = "UNIQUEID") # 989
goo <- goo %>%
  transmute(def59_z_0 = ifelse(is.na(def59_z_0.x), def59_z_0.y, def59_z_0.x),
         def59_z_1 = ifelse(is.na(def59_z_1.x), def59_z_1.y, def59_z_1.x),
         def59_z_max13 = ifelse(is.na(def59_z_max13.x), def59_z_max13.y, def59_z_max13.x),
         FIRE.YR = ifelse(is.na(FIRE.YR.x.x), FIRE.YR.x.y, FIRE.YR.x.x),
         YEAR.DIFF = ifelse(is.na(YEAR.DIFF.x), YEAR.DIFF.y, YEAR.DIFF.x),
         INVYR = ifelse(is.na(INVYR.x), INVYR.y, INVYR.x),
         UNIQUEID = UNIQUEID) # to retain b/c transmute drops

summary(goo$YEAR.DIFF)
summary(goo$FIRE.YR)
summary(goo$INVYR)




##### Trend?

# All z-scores (yr of fire) thru time
plot(goo$def59_z_0 ~ goo$FIRE.YR)
abline(lm(goo$def59_z_0 ~ goo$FIRE.YR))
lm(goo$def59_z_0 ~ goo$FIRE.YR) %>% summary()


# All z-scores (1 yr post-fire) thru time
plot(goo$def59_z_1 ~ goo$FIRE.YR)
abline(lm(goo$def59_z_1 ~ goo$FIRE.YR))
lm(goo$def59_z_1 ~ goo$FIRE.YR) %>% summary()
a <- lm(goo$def59_z_1 ~ goo$FIRE.YR)
a$coefficients[1]
a$coefficients[2]


# Max def z-scores thru time
plot(goo$def59_z_max13 ~ goo$FIRE.YR)
abline(lm(goo$def59_z_max13 ~ goo$FIRE.YR))
lm(goo$def59_z_max13 ~ goo$FIRE.YR) %>% summary()
b <- lm(goo$def59_z_max13 ~ goo$FIRE.YR)
b$coefficients[1]
b$coefficients[2]


# Max def z-scores by time since fire
plot(goo$def59_z_max13 ~ goo$YEAR.DIFF)
abline(lm(goo$def59_z_max13 ~ goo$YEAR.DIFF))
lm(goo$def59_z_max13 ~ goo$YEAR.DIFF) %>% summary()
c <- lm(goo$def59_z_max13 ~ goo$YEAR.DIFF)
c$coefficients[1]
c$coefficients[2]


# Remove those artificially lower b/c fewer than 3 chances to get high max.
zoo <- goo[goo$YEAR.DIFF>2,]
plot(zoo$def59_z_max13 ~ zoo$YEAR.DIFF)
abline(lm(zoo$def59_z_max13 ~ zoo$YEAR.DIFF))
lm(zoo$def59_z_max13 ~ zoo$YEAR.DIFF) %>% summary()





# Plot

frVz1 <- ggplot(data = goo, aes(x = FIRE.YR, y = def59_z_1)) +
  geom_jitter(width = 0.5, height = 0.0, size = 0.5) +
  geom_abline(intercept = a$coefficients[1], slope = a$coefficients[2]) + 
  # geom_point() +
  labs (x = "Year of fire", y = "Deficit anomaly\n one year post-fire") +
  scale_x_continuous(breaks=seq(1980,2015,5)) + 
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 1983, y = 2.87, label = "a")
frVz1



frVz13 <- ggplot(data = goo, aes(x = FIRE.YR, y = def59_z_max13)) +
  geom_jitter(width = 0.5, height = 0.0, size = 0.5) +
  geom_abline(intercept = b$coefficients[1], slope = b$coefficients[2]) + 
  # geom_point() +
  labs (x = "Year of fire", y = "3-year maximum\n deficit anomaly") +
  scale_x_continuous(breaks=seq(1980,2015,5)) + 
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 1983, y = 2.6, label = "b")
frVz13



yrVz13 <- ggplot(data = goo, aes(x = YEAR.DIFF, y = def59_z_max13)) +
  geom_jitter(width = 0.5, height = 0.0, size = 0.5) +
  geom_abline(intercept = c$coefficients[1], slope = c$coefficients[2]) +
  # geom_point() +
  labs (x = "Years since fire", y = "3-year maximum\n deficit anomaly") +
  scale_x_continuous(breaks=seq(0,30,5)) + 
  theme_bw(base_size = 14) +
  theme(axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate("text", x = 0, y = 2.6, label = "c")
yrVz13







plots <- list(frVz1, frVz13, yrVz13)
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
    width = 7, height = 7, units = "in", res = 300)
do.call("grid.arrange", c(grobs, ncol = 1, nrow = 3))
dev.off()

pdf(paste0(out.dir, "z_vs_yrs_", currentDate, ".pdf"))#,
    # width = 7, height = 5, units = "in", res = 300)
do.call("grid.arrange", c(grobs, ncol = 1, nrow = 3))
dev.off()

