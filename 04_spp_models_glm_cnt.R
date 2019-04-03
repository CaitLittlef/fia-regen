## How do regen counts compare between burned plots and unburned plots?

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo.csv") ; data.pipo$X <- NULL
identical(data.pipo$FIRE.YR.x, data.pipo$FIRE.YR.y)
data.pipo$FIRE.YR <- data.pipo$FIRE.YR.x
data.pipo$FIRE.YR.x <- NULL ; data.pipo$FIRE.YR.y <- NULL

data.psme <- read.csv("data.psme.csv") ; data.psme$X <- NULL
identical(data.psme$FIRE.YR.x, data.psme$FIRE.YR.y)
data.psme$FIRE.YR <- data.psme$FIRE.YR.x
data.psme$FIRE.YR.x <- NULL ; data.psme$FIRE.YR.y <- NULL

## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
# data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
# data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]

## Correct class of variables
# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) 
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) 
# data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
# data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV)
data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV)

data.pipo$regen_pipo_tpha <- round(data.pipo$regen_pipo_tpa * 2.47105, 0)
data.psme$regen_psme_tpha <- round(data.psme$regen_psme_tpa * 2.47105, 0)
#######################################################################
## Set bins for year.diff so include unburned (mtbs only back til 1984)
# Or maybe not necessary... only if doing stats
# yr.vec <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30)
# yr.vec <- c(1, 2, 5, 10, 15, 20, 25, 30) # first bin will LTE 1
yr.vec <- c(1, 5, 10, 15, 20, 25, 30) # first bin will LTE 1
data.pipo$YEAR.DIFF.BIN <- findInterval(data.pipo$YEAR.DIFF, vec=yr.vec, rightmost.closed=TRUE)
cbind(data.pipo$YEAR.DIFF.BIN, data.pipo$YEAR.DIFF)
data.pipo$YEAR.DIFF.BIN[which(is.na(data.pipo$YEAR.DIFF))] <- (max(data.pipo$YEAR.DIFF.BIN, na.rm=TRUE)+1)
cbind(data.pipo$YEAR.DIFF.BIN, data.pipo$YEAR.DIFF)
# data.pipo$YEAR.DIFF.BIN <- as.numeric(data.pipo$YEAR.DIFF.BIN) # if modelling
data.pipo$YEAR.DIFF.BIN <- factor(data.pipo$YEAR.DIFF.BIN, ordered = TRUE) # if plotting

## Only examining sites with > 0 BA & > 0 regen
moo <- data.pipo[data.pipo$BALive_pipo>0 & data.pipo$regen_pipo == 1,]
# Estabish BA bins (low, med, high) to show
hist(moo$BALive_pipo, 50)
hist(moo$TPALive_122, 50)
classIntervals(moo$BALive_pipo, 3, style = "quantile")
# ^ quantiles (with equal # sites) break at ~ 30, 60, up
ba.vec <- c(0,30,60,400)
moo$BA.BIN <- findInterval(moo$BALive_pipo, vec=ba.vec, rightmost.closed=TRUE)
moo$BA.BIN <- as.factor(moo$BA.BIN)
boxplot(moo$TPALive_122 ~ moo$BA.BIN)
moo %>%
  group_by(BA.BIN) %>%
  summarize_at("TPALive_122", funs(min, max, median))
# Below is summary of TPA by BA bins. Recall TPA is of trees sampled prob in macroplot
# BA.BIN   min   max median
# <fct>  <dbl> <dbl>  <dbl>
# 1 1       6.01  855.   36.1
# 2 2       6.01 1586.  114.
# 3 3      18.0  1895.  209.



# labels = c(paste0(3*1:10),"no fire in record")
# labels = c("1", "2-5", "6-10", "11-15", "16-20", "21-25", "26-30", "no fire or \nfire beyond record")
labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30", "no fire or fire\nbeyond record")

# THESE ARE OF SITES THAT HAVE REGEN
data.pipo$YEAR.DIFF.BIN <- factor(data.pipo$YEAR.DIFF.BIN, ordered = TRUE) # if plotting
p <- ggplot(moo, aes(x = YEAR.DIFF.BIN, y=log10(regen_pipo_tpha))) +
  geom_boxplot(fill = "#00A08A") + # #00A08A teal, #EAECEE light grey +
  labs(x = "Years since fire", y = expression(log[10]~phantom()~juveniles/ha),
       title = "ponderosa pine") + 
  scale_x_discrete(labels = labels) +
  scale_y_log10() + 
  coord_cartesian(ylim=c(2,4.5)) + # use this not scale else lose datapts
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())
p
# dev.off()
# tiff(paste0(out.dir,"pipo_cnts_yr_",currentDate,".tiff"),
#       width = 640, height = 480, units = "px")
# p
# dev.off()
#################################################### PSME
## Set bins for year.diff so include unburned (mtbs only back til 1984)
# Or maybe not necessary... only if doing stats
# vec <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30)
# vec <- c(1, 2, 5, 10, 15, 20, 25, 30, 35) # first bin will LTE 1
vec <- c(1, 5, 10, 15, 20, 25, 30, 35) # first bin will LTE 1
data.psme$YEAR.DIFF.BIN <- findInterval(data.psme$YEAR.DIFF, vec=vec, rightmost.closed=TRUE)
cbind(data.psme$YEAR.DIFF.BIN, data.psme$YEAR.DIFF)
data.psme$YEAR.DIFF.BIN[which(is.na(data.psme$YEAR.DIFF))] <- (max(data.psme$YEAR.DIFF.BIN, na.rm=TRUE)+1)
cbind(data.psme$YEAR.DIFF.BIN, data.psme$YEAR.DIFF)
data.psme$YEAR.DIFF.BIN <- as.numeric(data.psme$YEAR.DIFF.BIN) # if modelling
# data.psme$YEAR.DIFF.BIN <- factor(data.psme$YEAR.DIFF.BIN, ordered = TRUE) # if plotting

# labels = c(paste0(3*1:10),"no fire in record")
# labels = c("1", "2-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "no fire or fire\nbeyond record")
labels = c("1-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "no fire or fire\nbeyond record")

# THESE ARE OF SITES THAT HAVE REGEN
data.psme$YEAR.DIFF.BIN <- factor(data.psme$YEAR.DIFF.BIN, ordered = TRUE) # if plotting
p <- ggplot(data.psme[data.psme$BALive_psme>0,],
            aes(x = YEAR.DIFF.BIN, y=log10(regen_psme_tpha))) + 
  geom_boxplot(fill = "#00A08A") + # #00A08A teal, #EAECEE light grey
  labs(x = "Years since fire", y = expression(log[10]~phantom()~juveniles/ha),
       title = "Douglas-fir") + 
  scale_x_discrete(labels = labels) +
  scale_y_log10() + 
  coord_cartesian(ylim=c(2,4.5)) + # use this not scale else lose datapts
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())
p
# dev.off()
# tiff(paste0(out.dir,"psme_cnts_yr_",currentDate,".tiff"),
#       width = 640, height = 480, units = "px")
# p
# dev.off()
