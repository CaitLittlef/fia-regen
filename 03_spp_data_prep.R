data <- read.csv("DATA_PlotFireClim_PostFireSamp_n1971.csv")
data$X <- NULL # In case there's weird X col added

## Spp in data ********* WHY THESE?? GET PICO?? ***********

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
  mutate(PIEDregen = ifelse(data$TPASeed106Ac >0, 1, 0),
         PIPOregen = ifelse(data$TPASeed122Ac >0, 1, 0),
         PSMEregen = ifelse(data$TPASeed202Ac >0, 1, 0))
mean(data$PIEDregen) # 0.01826484
mean(data$PIPOregen) # 0.06950786
mean(data$PSMEregen) # 0.1212582



## Keep records where adult present
data.pied <- data %>%
  filter(BALive_106 >0 | BADeadStanding_106 >0 | BAMortStanding_106 >0 | BAMortDown_106 >0) %>%
  dplyr::rename(BALive_pied = BALive_106)
data.pipo <- data %>%
  filter(BALive_122 >0 | BADeadStanding_122 >0 | BAMortStanding_122 >0 | BAMortDown_122 >0) %>%
  dplyr::rename(BALive_pipo = BALive_122)  
data.psme <- data %>%
  filter(BALive_202 >0 | BADeadStanding_202 >0 | BAMortStanding_202 >0 | BAMortDown_202 >0) %>%
  dplyr::rename(BALive_psme = BALive_202)



## Check for outliers; BA sq ft/acre
max(data.pied$BALive_pied) # 124
max(data.pipo$BALive_pipo) # 458 seems high
hist(data.pipo$BALive_pipo)
data.pipo  <-data.pipo[data.pipo$BALive_pipo < 300,]
max(data.psme$BALive_psme) # 228



# Save
write.csv(data.pied, "data.pied.csv")
write.csv(data.pipo, "data.pipo.csv")
write.csv(data.psme, "data.psme.csv")

