data <- read.csv("DATA_PlotFireClim_PostFireSamp_n1971.csv")
data <- read.csv("DATA_PlotwwoFireClim_n20859.csv")
data$X <- NULL # In case there's weird X col added

## Spp in data 
# 106 Pinus edulis (2-needle)
# 122 Pinus ponderosa
# 133 Pinus monophylla (1-needle pinyon)
# 140 Pinus cembroides (Mex pinyon)
# 202 Pseudotsuga menziesii
# 803 Quercus arizonica
# 810 Quercus emoryi
# 814 Quercus gambelii
# 843 Quercus hypoleucoides
# 846 Quercus grisea
# 847 Quercus rugosa


## Convert all NAs in BA or TPA to zero
# Use caret ^ to specify start of string
BA.cols <- grep(pattern="^BA", x=colnames(data), value=TRUE)
TPA.cols <- grep(pattern="^TPA", x=colnames(data), value=TRUE)
data[BA.cols][is.na(data[BA.cols])] <- 0
data[TPA.cols][is.na(data[TPA.cols])] <- 0
data$DUFF_DEPTH[is.na(data$DUFF_DEPTH)] <- 0
data$LITTER_DEPTH[is.na(data$LITTER_DEPTH)] <- 0



## Create p/a regen var for maj spp
data <- data %>%
  mutate(regen_pied = ifelse(data$TPASeed106Ac >0, 1, 0),
         regen_pipo = ifelse(data$TPASeed122Ac >0, 1, 0),
         regen_psme = ifelse(data$TPASeed202Ac >0, 1, 0)) %>%
  rename(regen_pied_tpa = TPASeed106Ac,
         regen_pipo_tpa = TPASeed122Ac,
         regen_psme_tpa = TPASeed202Ac)
mean(data$regen_pied) # 0.01826484
mean(data$regen_pipo) # 0.06950786
mean(data$regen_psme) # 0.1212582

## Will want as factor for modelling
data$regen_pied <- factor(data$regen_pied)
data$regen_pipo <- factor(data$regen_pipo)
data$regen_psme <- factor(data$regen_psme)



## Keep records where adult present -- even if dead. 
# Mort trees are dead since last inventory
data.pied <- data %>%
  filter(BALive_106 >0 | BADeadStanding_106 >0 | BAMortStanding_106 >0 | BAMortDown_106 >0) %>%
  dplyr::rename(BALive_pied = BALive_106)
data.pipo <- data %>%
  filter(BALive_122 >0 | BADeadStanding_122 >0 | BAMortStanding_122 >0 | BAMortDown_122 >0) %>%
  dplyr::rename(BALive_pipo = BALive_122)  
data %>%
  filter(BALive_122 >0) %>%
  dplyr::rename(BALive_pipo = BALive_122) # 3222 records if exclude dead
data.psme <- data %>%
  filter(BALive_202 >0 | BADeadStanding_202 >0 | BAMortStanding_202 >0 | BAMortDown_202 >0) %>%
  dplyr::rename(BALive_psme = BALive_202)



## Check for outliers; BA sq ft/acre; TPA
max(data.pied$BALive_pied) # 373 seems high
hist(data.pied$BALive_pied)
data.pied <- data.pied[data.pied$BALive_pied < 300,]
max(data.pied$regen_pied_tpa)
hist(data.pied$regen_pied_tpa)

max(data.pipo$BALive_pipo) # 570 seems high
hist(data.pipo$BALive_pipo)
data.pipo <- data.pipo[data.pipo$BALive_pipo < 400,]
max(data.pipo$regen_pipo_tpa) #17992 is lots even for PICO
hist(data.pipo$regen_pipo_tpa)
data.pipo <- data.pipo[data.pipo$regen_pipo_tpa < 10000,]

max(data.psme$BALive_psme) # 228
hist(data.psme$BALive_psme)
max(data.psme$regen_psme_tpa)


# Save
write.csv(data.pied, "data.pied_wwoburn.csv")
write.csv(data.pipo, "data.pipo_wwoburn.csv")
write.csv(data.psme, "data.psme_wwoburn.csv")

rm(BA.cols, TPA.cols)
