currentDate <- Sys.Date()


## Load data; remove extraneous column if necessary
data.pied <- read.csv("data.pied_wwoburn.csv") ; data.pied$X <- NULL
data.pipo <- read.csv("data.pipo_wwoburn.csv") ; data.pipo$X <- NULL
data.psme <- read.csv("data.psme_wwoburn.csv") ; data.psme$X <- NULL

## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
# data.pied <- data.pied[! is.na(data.pied$FIRE.SEV) ,]
# data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]
# data.psme <- data.psme[! is.na(data.psme$FIRE.SEV) ,]

## Set variable classes
# data.pied$regen_pied <- factor(data.pied$regen_pied, ordered = FALSE)
# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo, ordered = FALSE)
# data.psme$regen_psme <- factor(data.psme$regen_psme, ordered = FALSE)
data.pied$regen_pied <- as.numeric(as.character(data.pied$regen_pied)) # else gives factor level position
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo)) # else gives factor level position
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme)) # else gives factor level position
data.pied$regen_pied_tpa <- as.integer(round(data.pied$regen_pied_tpa,0))
data.pipo$regen_pipo_tpa <- as.integer(round(data.pipo$regen_pipo_tpa,0))
data.psme$regen_psme_tpa <- as.integer(round(data.psme$regen_psme_tpa,0))
# data.pied$FIRE.SEV <- factor(data.pied$FIRE.SEV, ordered = TRUE)
# data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
# data.psme$FIRE.SEV <- factor(data.psme$FIRE.SEV, ordered = TRUE)
data.pied$FIRE.SEV <- as.numeric(data.pied$FIRE.SEV) # else generates diff var for each factor level
data.pipo$FIRE.SEV <- as.numeric(data.pipo$FIRE.SEV) # else generates diff var for each factor level
data.psme$FIRE.SEV <- as.numeric(data.psme$FIRE.SEV) # else generates diff var for each factor level
data.pied$REBURN <- factor(data.pied$REBURN, ordered = TRUE)
data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)
data.psme$REBURN <- factor(data.psme$REBURN, ordered = TRUE)

# Create CMD relative chng
data.pied$CMD_CHNG <- (data.pied$CMD_2025 - data.pied$CMD_1995)/data.pied$CMD_1995
data.pipo$CMD_CHNG <- (data.pipo$CMD_2025 - data.pipo$CMD_1995)/data.pipo$CMD_1995
data.psme$CMD_CHNG <- (data.psme$CMD_2025 - data.psme$CMD_1995)/data.psme$CMD_1995


## Set bins for year.diff so include unburned (mtbs only back til 1984)
# Or maybe not necessary... only if doing stats
# vec <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30)
vec <- c(1, 2, 5, 10, 15, 20, 25, 30) # first bin will LTE 1
data.pipo$YEAR.DIFF.BIN <- findInterval(data.pipo$YEAR.DIFF, vec=vec, rightmost.closed=TRUE)
cbind(data.pipo$YEAR.DIFF.BIN, data.pipo$YEAR.DIFF)
data.pipo$YEAR.DIFF.BIN[which(is.na(data.pipo$YEAR.DIFF))] <- (max(data.pipo$YEAR.DIFF.BIN, na.rm=TRUE)+1)
cbind(data.pipo$YEAR.DIFF.BIN, data.pipo$YEAR.DIFF)
data.pipo$YEAR.DIFF.BIN <- as.numeric(data.pipo$YEAR.DIFF.BIN) # if modelling
# data.pipo$YEAR.DIFF.BIN <- factor(data.pipo$YEAR.DIFF.BIN, ordered = TRUE) # if plotting


## glm of pipo regen; 
mean(data.pipo$regen_pipo_tpa) ; sd(data.pipo$regen_pipo_tpa)
# ^ suggests overdisprsion. Try poissoin & negative binomial
# Poisson
mod.pipo.cnt.ps <- glm(regen_pipo_tpa ~ YEAR.DIFF.BIN
                       + BALive_pipo
                       + MAP_1995 
                       + CMD_CHNG
                       + FIRE.SEV,
                       data = data.pipo, poisson(link = "log"))
summary(mod.pipo.cnt.ps)

# Negative binomial
mod.pipo.cnt.nb <- glm.nb(regen_pipo_tpa ~ YEAR.DIFF.BIN
                          + BALive_pipo
                          + MAP_1995 
                          + CMD_CHNG,
                          + FIRE.SEV,
                          data = data.pipo, link = "log")
summary(mod.pipo.cnt.nb)
pchisq(2 * (logLik(mod.pipo.cnt.nb) - logLik(mod.pipo.cnt.ps)), df = 1, lower.tail = FALSE)
# Negative binomial models assume the conditional means are not equal to the conditional variances. This inequality is captured by estimating a dispersion parameter (not shown in the output) that is held constant in a Poisson model. Thus, the Poisson model is actually nested in the negative binomial model. We can then use a likelihood ratio test to compare these two and test this model assumption.
# Here, reject null hypothesis that additional parameter theta (dispersion) does NOT improve fit


## log won't take 0 so set slightly above 
# data.pipo$regen_pipo_tpa_no0 <- data.pipo$regen_pipo_tpa
# data.pipo$regen_pipo_tpa_no0[data.pipo$regen_pipo_tpa_no0 == 0] <- 1
data.pipo %>%
  count(regen_pipo_tpa)
# data.pipo %>%
#   count(regen_pipo_tpa_no0)


labels = c(paste0(3*1:10),"no fire in record")
labels = c("1", "2-5", "6-10", "11-15", "16-20", "21-25", "26-30", "no fire in record")

# THESE ARE OF SITES THAT HAVE REGEN
data.pipo$YEAR.DIFF.BIN <- factor(data.pipo$YEAR.DIFF.BIN, ordered = TRUE) # if plotting
p <- ggplot(data.pipo[data.pipo$BALive_pipo>0,],
            aes(x = YEAR.DIFF.BIN, y=log(regen_pipo_tpa))) +
  geom_boxplot() +
  labs(x = "Years since fire", y = "log(seedlings/acre)") + 
  scale_x_discrete(labels = labels)
p
tiff(paste0(out.dir,"pipo_cnts_yr_",currentDate,".tiff"),
      width = 640, height = 480, units = "px")
p
dev.off()



#################################################### PSME
## Set bins for year.diff so include unburned (mtbs only back til 1984)
# Or maybe not necessary... only if doing stats
# vec <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30)
vec <- c(1, 2, 5, 10, 15, 20, 25, 30, 35) # first bin will LTE 1
data.psme$YEAR.DIFF.BIN <- findInterval(data.psme$YEAR.DIFF, vec=vec, rightmost.closed=TRUE)
cbind(data.psme$YEAR.DIFF.BIN, data.psme$YEAR.DIFF)
data.psme$YEAR.DIFF.BIN[which(is.na(data.psme$YEAR.DIFF))] <- (max(data.psme$YEAR.DIFF.BIN, na.rm=TRUE)+1)
cbind(data.psme$YEAR.DIFF.BIN, data.psme$YEAR.DIFF)
data.psme$YEAR.DIFF.BIN <- as.numeric(data.psme$YEAR.DIFF.BIN) # if modelling
# data.psme$YEAR.DIFF.BIN <- factor(data.psme$YEAR.DIFF.BIN, ordered = TRUE) # if plotting

# labels = c(paste0(3*1:10),"no fire in record")
labels = c("1", "2-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "no fire in record")

# THESE ARE OF SITES THAT HAVE REGEN
data.psme$YEAR.DIFF.BIN <- factor(data.psme$YEAR.DIFF.BIN, ordered = TRUE) # if plotting
p <- ggplot(data.psme[data.psme$BALive_psme>0,],
            aes(x = YEAR.DIFF.BIN, y=log(regen_psme_tpa))) + 
  geom_boxplot() +
  labs(x = "Years since fire", y = "log(seedlings/acre)") + 
  scale_x_discrete(labels = labels)
p

tiff(paste0(out.dir,"psme_cnts_yr_",currentDate,".tiff"),
     width = 640, height = 480, units = "px")
p
dev.off()






#################################################### PIED
## Set bins for year.diff so include unburned (mtbs only back til 1984)
# Or maybe not necessary... only if doing stats
hist(data.pied$YEAR.DIFF)
# vec <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30)
vec <- c(1, 2, 5, 10, 15, 20, 25, 30) # first bin will LTE 1
data.pied$YEAR.DIFF.BIN <- findInterval(data.pied$YEAR.DIFF, vec=vec, rightmost.closed=TRUE)
cbind(data.pied$YEAR.DIFF.BIN, data.pied$YEAR.DIFF)
data.pied$YEAR.DIFF.BIN[which(is.na(data.pied$YEAR.DIFF))] <- (max(data.pied$YEAR.DIFF.BIN, na.rm=TRUE)+1)
cbind(data.pied$YEAR.DIFF.BIN, data.pied$YEAR.DIFF)
data.pied$YEAR.DIFF.BIN <- as.numeric(data.pied$YEAR.DIFF.BIN) # if modelling
# data.pied$YEAR.DIFF.BIN <- factor(data.pied$YEAR.DIFF.BIN, ordered = TRUE) # if plotting

# labels = c(paste0(3*1:10),"no fire in record")
labels = c("1", "2-5", "6-10", "11-15", "16-20", "21-30", "no fire in record")

# THESE ARE OF SITES THAT HAVE REGEN
data.pied$YEAR.DIFF.BIN <- factor(data.pied$YEAR.DIFF.BIN, ordered = TRUE) # if plotting
p <- ggplot(data.pied[data.pied$BALive_pied>0,],
            aes(x = YEAR.DIFF.BIN, y=log(regen_pied_tpa))) + 
  geom_boxplot() +
  labs(x = "Years since fire", y = "log(seedlings/acre)") + 
  scale_x_discrete(labels = labels)
p

# tiff(paste0(out.dir,"pied_cnts_yr_",currentDate,".tiff"),
#      width = 640, height = 480, units = "px")
# p
# dev.off()
