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

mod = ""
# sim = "AvgMin"
sim = "MaxMin"

folderDrias = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod,"_elab")
folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod,"_sim_050")
folderSimSin = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_050")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod,"_sim_050")

# we choose places with 3000-5500 inhabitants
# scenarios <- c("Cn70", "Cn55", "Cn35", "Hs99", "Hg35", "Hg55", "Hg70")
scenarios <- c("Cn70", "Hs99","Hg70")
cities <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice", "Strasbourg", "Clermont-Ferrand")
IDsSubSet <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249, 7379, 3564)

# MapDT <- data.table(city = rep(cities, length(scenarios)),
#                     ID = rep(IDsSubSet, length(scenarios)),
#                     scenario = rep(scenarios,length(cities)),
#                     tasAvgYea = 0, #average annual T
#                     tasMinWin = 0, #average in January
#                     tasMaxSum = 0) #average in July
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
#       MapDT[scenario == scenariox & ID == IDx, tasMinWin := (tasMinWin*(j-1)+mean(WTotDTtemp[DOS <= 31, tas]))/j]
#       MapDT[scenario == scenariox & ID == IDx, tasMaxSum := (tasMaxSum*(j-1)+mean(WTotDTtemp[DOS %in% 182:212, tas]))/j]
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

IndDT <- readRDS(paste0(folderSimSin, "/IndDT", sim, ".rds"))

# load simulations
Adults <- readRDS(file = paste0(folderSimSin, "/040e_Adults_SEIS_", sim, ".rds"))
SH <- readRDS(file = paste0(folderSimSin, "/040e_SH_SEIS_", sim, ".rds"))
WTotDT <- readRDS(paste0(folderSimSin, "/WTotDT", sim, ".rds"))
E0 <- readRDS(file = paste0(folderSimSin, "/020_E0_synthetic_", sim, ".rds"))
A0 <- readRDS(file = paste0(folderSimSin, "/020_A0_synthetic_", sim, ".rds"))

year = year(WTotDT$date[1])

#recalulate, for each month, the lowest and the highest temperature
tasMinWiPostDT = WTotDT%>%
  select(c(ID, tas, date))%>%
  filter(date < as.Date("2026-02-01"))%>%
  group_by(ID)%>%
  summarise(tasMinWinPost = mean(tas))%>%
  ungroup()

tasMaxSumPostDT = WTotDT%>%
  select(c(ID, tas, date))%>%
  filter(date %in% (as.Date("2026-07-01")+0:30))%>%
  group_by(ID)%>%
  summarise(tasMaxSumPost = mean(tas))%>%
  ungroup()

# we correct tasMinWin with tasMinWinPost (so that the deviation in 0), same for tasMasx
IndDT <- left_join(left_join(IndDT, tasMinWiPostDT), tasMaxSumPostDT)%>%
  mutate(tasMinWinCor = tasMinWin + mean(tasMinWinPost- tasMinWin))%>%
  mutate(tasMaxSumCor = tasMaxSum + mean(tasMaxSumPost- tasMaxSum))

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

# MapDT

MapDTtemp <- MapDT%>%
  filter(city %in% c("Bordeaux", "Grenoble", "Montpellier", "Lille", "Rennes"))

IndDT <- IndDT %>%
  mutate(statusSuitability = case_when(
    (E0 > 10)&(A0 > 10) ~ "7", # Stronguitability of homodynamic population
    (E0 > 10)&(A0 > 1) ~ "6", # Suitability of homodynamic population
    (E0 > 10)&(A0 > 0.1) ~ "5", # Weak suitability of homodynamic population
    (E0 > 10)&(A0 < 0.1) ~ "4", # strong suitability of diapausing population
    (E0 > 1)&(A0 > 0.1) ~ "5", # Weak suitability of homodynamic population
    (E0 > 1)&(A0 < 0.1) ~ "3", # Suitability of diapausing population
    (E0 > 0.1)&(A0 < 0.1) ~ "2", # Weak suitability of diapausing population
    (E0 < 0.1)&(A0 < 0.1) ~ "1")) # Strong unsuitability for Aedes albopictus

IndDT$statusSuitability <- as.factor(IndDT$statusSuitability)

colPal= c("#384AB4", "#8EB0FE", "#F29878",  "#B00026", "#AD5CFF", "#5200A3", "#0D001A")

plotCut <- ggplot() +
  geom_tile( #contour_fill
    data = IndDT,
    aes(x = tasMaxSumCor, y = tasMinWinCor, fill = statusSuitability) #  tasAvgYeaCor
  ) +
  ggtitle("Suitability status")+
  scale_fill_manual(values = colPal)+
  theme_test()+
  geom_path(data = MapDTtemp, aes(x = tasMaxSum, y = tasMinWin, group = city), #AvgYea
            arrow = arrow(ends = "both", type = "closed", length = unit(0.05, "inches")), color = "grey70") + # , color= "white"
  geom_point(data = MapDTtemp, aes(x =tasMaxSum, y = tasMinWin, shape = scenario), size = 1) + #, color= "white", #shape = MapDT$pointShape, 
  guides(size = "legend", colour = "none")+
  scale_color_grey()

plotCut+
  geom_label_repel(data = MapDTtemp,
                   aes(x = tasMaxSum, y = tasMinWin, label = cityLabel),
                   label.padding = 0.15, segment.color = NA, size = 4) #size = 4

plotCut <- plotCut +
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))


ggsave(file = 
         paste0(folderPlot, "/SuitabilityStatus.png"),
       plot= plotCut, units="cm", height=5.2, width = 8.2, dpi=300) #units="in", height=4,


IndDT <- IndDT %>%
  mutate(statusDengue = case_when(
    (E0 > 1)&(LTS > 161) ~ "6",
    (E0 > 1)&(LTS > 105) ~ "5",
    (E0 > 1)&(LTS > 56) ~ "4",
    (E0 > 1)&(LTS > 21) ~ "3",
    (E0 > 1)&(LTS > 1) ~ "2",
    (E0 > 1)&(LTS < 1) ~ "1",
    (E0 < 1) ~ "0"))


colPal<- c("#0D001A", "#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724", "#F7f5bc")

plotCut <- ggplot() +
  geom_tile(
    data = IndDT,
    aes(x = tasMaxSumCor, y = tasMinWinCor, fill = statusDengue) #
  ) +
  ggtitle("Suitability for Dengue")+ #Suitability
  scale_fill_manual(values = colPal)+
  theme_test()+
  geom_path(data = MapDTtemp, aes(x = tasMaxSum, y = tasMinWin, group = city),
            arrow = arrow(ends = "both", type = "closed", length = unit(0.05, "inches")), color= "grey50") + # , color= "white"
   geom_point(data = MapDTtemp, aes(x = tasMaxSum, y = tasMinWin, shape = scenario), color= "red", size = 1) + #, color= "white", #shape = MapDT$pointShape, 
  guides(size = "legend", colour = "none")+
  scale_color_grey()

plotCut <- plotCut +
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        line = element_blank(), 
        rect = element_blank(), 
        text = element_blank(), 
        plot.background = element_rect(fill = "transparent", color = "transparent"))

ggsave(file = 
         paste0(folderPlot, "/DengueStatus.png"),
       plot= plotCut, units="cm", height=5.2, width = 8.2, dpi=300) #units="in", height=4,

# +
#   geom_label_repel(data = MapDTtemp,
#                    aes(x = tasAvgYea, y = tasMinWin, label = cityLabel),
#                    label.padding = 0.15, segment.color = NA, size = 3) #size = 4
