library(dplyr)

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo_2019-06-12.csv") ; data.pipo$X <- NULL


## Select relevant vars
data <- data.pipo %>%
  dplyr::select(PLOTID, STATECD.x, COUNTYCD.x, PLOT.x, INVYR,
                SLOPE, ASPECT, DUFF_DEPTH_cm, LITTER_DEPTH_cm,
                BALiveTot_m, BALive_pipo_m,
                regen_pipo, regen_pipo_tpa,
                FIRE.YR.x, FIRE.SEV, REBURN,
                LAT_FS, LON_FS)

head(data)    

## Exclude sites that either have increased greenness (5) or weren't mapped (6).
# Nb these cats are already coded as na. Exclude them with this:
data <- data[! is.na(data$FIRE.SEV) ,]

data %>% count(FIRE.SEV)
FIRE.SEV     n
# 1        1   131
# 2        2   205
# 3        3   117
# 4        4    68


write_csv(data, "data.pipo.2019-06-12.SLIM.csv")
