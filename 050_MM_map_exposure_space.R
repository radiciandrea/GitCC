# code to plot map (T winter vs T avg) under different climate change conditiosn

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)
library(metR)
library(ggrepel)
library(ggpubr)

folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
folderSim = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Synthetic_Exposure_Space"

# # we choose places with ~ inhabitants
# # scenarios <- c("Cn70", "Cn55", "Cn35", "Hs99", "Hg35", "Hg55", "Hg70")
# scenarios <- c("Cn70", "Hs99","Hg70")
# cities <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice")
# IDsSubSet <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249)
# 
# MapDT <- data.table(city = rep(cities, length(scenarios)),
#                     ID = rep(IDsSubSet, length(scenarios)),
#                     scenario = rep(scenarios,length(cities)),
#                     tasAvgYea = 0, #average annual T
#                     tasMinWin = 0) #average minimal in J
# 
# for(k in 1:length(scenarios)){
#   
#   scenariox = scenarios[k]
#   files = list.files(paste0(folderDrias,"/"), paste0("Drias_", scenariox))
#   
#   for(i in  1:length(files)){
#     
#     file = files[i]
#     year <- substr(file, nchar(file)-7, nchar(file)-4)
#     
#     WTotDT <- readRDS(paste0(folderDrias, "/Drias_", scenariox, "_", year, ".rds")) %>%
#       filter(ID %in% IDsSubSet)
#     
#     for(j in 1:length(IDsSubSet)){
#       
#       IDx = IDsSubSet[j]
#       WTotDTtemp = WTotDT %>% filter(ID == IDx)
#       
#       MapDT[scenario == scenariox & ID == IDx, tasAvgYea := (tasAvgYea*(j-1)+mean(WTotDTtemp[,tas]))/j]
#       MapDT[scenario == scenariox & ID == IDx, tasMinWin := (tasMinWin*(j-1)+mean(WTotDTtemp[DOS <= 31, tasMin]))/j]
#       cat(mean(WTotDTtemp$tas), "\n")
#     }
#     
#   }
# }
# 
# # Sort DF
# 
# MapDT <- MapDT %>%
#   mutate(scenario = factor(scenario, levels = c("Cn70", "Hs99", "Hg70"))) %>%
#   arrange(city, scenario)
# 
# MapDT$cityLabel[MapDT$scenario == "Hs99"] = unique(MapDT$city)
# 
# # Save and Load
# 
# saveRDS(MapDT, file = paste0(folderSim, "/MapDT.rds"))

MapDT <- readRDS(paste0(folderSim, "/MapDT.rds"))

# load background Indicators

IndDT <- readRDS(paste0(folderSim, "/IndDT.rds"))

# load simulations
Adults <- readRDS(file = paste0(folderSim, "/040e_Adults_SEIS.rds"))
SH <- readRDS(file = paste0(folderSim, "/040e_SH_SEIS.rds"))
WTotDT <- readRDS(paste0(folderSim, "/WTotDT.rds"))
E0 <- readRDS(file = paste0(folderSim, "/020_E0_synthetic.rds"))
A0 <- readRDS(file = paste0(folderSim, "/020_A0_synthetic.rds"))

#recalulcate, for each month, the lowest temperature
tasMinWinPostDT = WTotDT%>%
  select(c(ID, tas, date))%>%
  filter(date < as.Date("2026-02-01"))%>%
  group_by(ID)%>%
  summarise(tasMinWinPost = mean(tas))%>%
  ungroup()

# we correct tasMinWin with tasMinWinPost (so that the deviation in 0)
IndDT <- left_join(IndDT, tasMinWinPostDT)%>%
  mutate(tasMinWin = tasMinWin + mean(tasMinWinPost- tasMinWin))

#epidem parameters
phiAU = 0.9 #human biting preference (urban)
phiAR = 0.5 #human biting preference (rural) #Caminade 2016
RTh = 50 #defines density over km2 below which an area is "rural", with little phi
IIPdengue = 5 #Benkimoun 2021
bV2H = 0.5 # b Blagrove 2020
bH2Vdengue = 0.31 # beta Metelmann 2021

#determine mjjaso
nD <- nrow(Adults)
FMay <- yday(as.Date(paste0(year, "-05-01"))) 
LOct <- yday(as.Date(paste0(year, "-10-31"))) 

### Elaborate adults ----
Amjjaso <- colMeans(Adults[FMay:LOct,], na.rm =T)

#weather
tas = matrix(WTotDT$tas, nrow = nD)

#reshape human matrix
H = matrix(WTotDT$pop, nrow = nD, byrow = T )
SurfHaM = matrix(WTotDT$surfHa, nrow = nD, byrow = T )

#vector to host ratio
m = Adults*100/H

#demographic parameters
muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI

#epidemiological parameters
A = (0.0043*tas + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
phiA = phiAU*(H>RTh)+phiAR*(H<=RTh)

EIPdengue = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021

## LTSR0----
R0dengueM = (A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue

# elaborate LTS
IndDT$LTS = colSums(R0dengueM>1, na.rm =T) #R0dengueM
IndDT$E0 <- E0 
IndDT$E0[is.nan(E0)] = 10^-8
IndDT$E0[E0<10^-8] = 10^-8
IndDT$E0[E0>10^8] = 10^8
IndDT$A0 <- A0 
IndDT$A0[is.nan(A0)] = 10^-8
IndDT$A0[A0<10^-8] = 10^-8
IndDT$A0[A0>10^8] = 10^8

IndDT <- IndDT %>%
  mutate(status = case_when(
    (E0 > 1)&(LTS > 360) ~ "8) Suitable, homodynamic trnamission",
    (E0 > 1)&(LTS > 270) ~ "7) Suitable, < 12 months of transmission",
    (E0 > 1)&(LTS > 180) ~ "6) Suitable, < 9 m of transmission",
    (E0 > 1)&(LTS > 90) ~ "5) Suitable, < 6 m of transmission",
    (E0 > 1)&(LTS > 30) ~ "4) Suitable, < 3 m of transmission",
    (E0 > 1)&(LTS > 1) ~ "3) Suitable, < 1 m of transmission",
    (E0 > 1)&(LTS < 1) ~ "2) Suitable, no transmission",
    (E0 < 1) ~ "1) Unsuitable"))

IndDT <- IndDT %>%
  mutate(statusSuitability = case_when(
    (E0 > 1)&(A0 > 1) ~ 5,
    (E0 > 1)&(A0 > 0.1) ~ 4,
    (E0 > 1)&(A0 < 0.1) ~ 3,
    (E0 > 0.1)&(A0 < 0.1) ~ 2,
    (E0 < 0.1)&(A0 < 0.1) ~ 1))

IndDT <- IndDT %>%
  mutate(status = case_when(
    (E0 > 1)&(LTS > 360) ~ 8,
    (E0 > 1)&(LTS > 270) ~ 7,
    (E0 > 1)&(LTS > 180) ~ 6,
    (E0 > 1)&(LTS > 90) ~ 5,
    (E0 > 1)&(LTS > 30) ~ 4,
    (E0 > 1)&(LTS > 1) ~ 3,
    (E0 > 1)&(LTS < 1) ~ 2,
    (E0 < 1) ~ 1))


ggplot() +
  geom_contour_fill(
    data = IndDT,
    aes(x = tasAvgYea, y = tasMinWin, z = log(E0)) #
  ) +
  ggtitle("Suitability")+ #Suitability
  geom_contour(
    data = IndDT,
    aes(x = tasAvgYea, y = tasMinWin, z = log(E0)),
    color = "black", breaks = c(1))+
  theme_test()+
  geom_path(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, group = city)) + # , color= "white"
  geom_point(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, shape = scenario), size = 2) + #, color= "white", #shape = MapDT$pointShape, 
  guides(size = "legend", colour = "none")+
  scale_color_grey()+
  geom_label_repel(data = MapDT ,
                   aes(x = tasAvgYea, y = tasMinWin, label = cityLabel),
                   label.padding = 0.15, segment.color = NA, size = 3) #size = 4

ggplot() +
  geom_contour_fill(
    data = IndDT,
    aes(x = tasAvgYea, y = tasMinWin, z = log(A0)) #
  ) +
  ggtitle("Suitability of a non-diapausing population")+ #Suitability
  geom_contour(
    data = IndDT,
    aes(x = tasAvgYea, y = tasMinWin, z = log(A0)),
    color = "black", breaks = c(1))+
  theme_test()+
  geom_path(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, group = city)) + # , color= "white"
  geom_point(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, shape = scenario), size = 2) + #, color= "white", #shape = MapDT$pointShape, 
  guides(size = "legend", colour = "none")+
  scale_color_grey()+
  geom_label_repel(data = MapDT ,
                   aes(x = tasAvgYea, y = tasMinWin, label = cityLabel),
                   label.padding = 0.15, segment.color = NA, size = 3) #size = 4


ggplot() +
  geom_contour_fill(
    data = IndDT,
    aes(x = tasAvgYea, y = tasMinWin, z = LTS) #
  ) +
  ggtitle("LTS")+ #Suitability
  geom_contour(
    data = IndDT,
    aes(x = tasAvgYea, y = tasMinWin, z =  LTS),
    color = "black", breaks = c(1))+
  theme_test()+
  geom_path(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, group = city)) + # , color= "white"
  geom_point(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, shape = scenario), size = 2) + #, color= "white", #shape = MapDT$pointShape, 
  guides(size = "legend", colour = "none")+
  scale_color_grey()+
  geom_label_repel(data = MapDT ,
                   aes(x = tasAvgYea, y = tasMinWin, label = cityLabel),
                   label.padding = 0.15, segment.color = NA, size = 3) #size = 4

ggplot() +
  geom_contour_fill(
    data = IndDT,
    aes(x = tasAvgYea, y = tasMinWin, z = statusSuitability) #
  ) +
  ggtitle("Suitability status")+
  theme_test()+
  geom_path(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, group = city)) + # , color= "white"
  geom_point(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, shape = scenario), size = 2) + #, color= "white", #shape = MapDT$pointShape, 
  guides(size = "legend", colour = "none")+
  scale_color_grey()+
  geom_label_repel(data = MapDT ,
                   aes(x = tasAvgYea, y = tasMinWin, label = cityLabel),
                   label.padding = 0.15, segment.color = NA, size = 3) #size = 4
