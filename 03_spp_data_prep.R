# data <- read.csv("DATA_PlotFireClim_PostFireSamp_n1971.csv") # w/o fire
data <- read.csv("DATA_PlotwwoFireClim_n20859_2019-02-27.csv")
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
hist(data.pipo$BALive_pipo,50)
max(data.pipo$BALive_pipo) #570 seems wicked high
hist(data.pipo$TPALive_122,50)
max(data.pipo$TPALive_122) # 5460
max(data.pipo$regen_pipo_tpa) #17992 is lots even if it were PICO
data.pipo <- data.pipo[data.pipo$BALive_pipo < 350,]
data.pipo <- data.pipo[data.pipo$regen_pipo_tpa < 10000,]


hist(data.psme$BALive_psme,50)
max(data.psme$BALive_psme) #345
hist(data.psme$TPALive_202,50)
max(data.psme$TPALive_202) # 3664
data.psme <- data.psme[data.psme$BALive_psme < 350,]


hist(data.pied$BALive_pied)
max(data.pied$BALive_pied) # 373 seems high
data.pied <- data.pied[data.pied$BALive_pied < 350,]
max(data.pied$regen_pied_tpa)
hist(data.pied$regen_pied_tpa)




# Save
write.csv(data.pied, "data.pied.csv")
write.csv(data.pipo, "data.pipo.csv")
write.csv(data.psme, "data.psme.csv")

rm(BA.cols, TPA.cols)
