# code to prot abundances of adults and R0

# inspired by both MM_pis_matrix_EOBS_cycle_consec_compute_plot_R0.R
# and MM_pis_matrix_EOBS_cycle_consec_01_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

folderSim = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_03"
folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
folderPlotA = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/Adults"
folderPlotR0 = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/R0"

## initial settings----

# load 1 for dimension
name = "Hist"
files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_", name))
Sim <- readRDS(paste0(folderSim, "/", files[1]))
nC = 5 # number of classes
nIDs = (ncol(Sim)-1)/nC # number of regions
IDs = 1:nIDs
IDsSubSet = IDs

# scenarios

scenariosDF= data.frame(name = c("Hist", "ssp245", "ssp245", "ssp585", "ssp585"),
                          yearStart = c(1996, 2050, 2080, 2050, 2080),
                          yearEnd = c(1996, 2050, 2080, 2050, 2080)+10)


# create meta matrices for each scenario (hist, ssp2, ssp5) for both adults and MTS for dengue

AmjjasoMM <- matrix(NA, ncol = nIDs, nrow = 5)
LTSdengueMM <- matrix(NA, ncol = nIDs, nrow = 5)

#fixed epimelogical (meta)perameters

#host preference
phiAU = 0.9 #human biting preference (urban)
phiAR = 0.5 #human biting preference (rural) #Caminade 2016
RTh = 50 #defines density over km2 below which an area is "rural", with little phi

IIPdengue = 5 #Benkimoun 2021

bV2H = 0.5 # b Blagrove 2020
bH2Vdengue = 0.31 # beta Metelmann 2021

# Compute indicators per scenario----

##HIST----

name = "Hist"
years = 1996:2005 #:2005

files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_", name))

# matrices of indicators: average adults and R0

AmjjasoM = matrix(NA, nrow = length(years), ncol = nIDs)
LTSdengueM = matrix(NA, nrow = length(years), ncol = nIDs)

for(i in  1:length(years)){
  
  file = files[i]
  
  Sim <- readRDS(paste0(folderSim, "/", file))
  year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
  
  #determine mjjaso
  nD <- nrow(Sim)
  FMay <- yday(as.Date(paste0(year, "-05-01"))) 
  LOct <- yday(as.Date(paste0(year, "-10-31"))) 
  
  Adults <- Sim[,3*nIDs + 1:nIDs]
  
  Amjjaso <- colMeans(Adults[FMay:LOct,], na.rm =T)
  AmjjasoM[i, ] <- Amjjaso
  
  # load weather for R0
  
  WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
    filter(ID %in% IDsSubSet) 
  
  tas = matrix(WTotDT$tas, nrow = nD)
  
  # load H (just once) for R0
  if(!exists("IDsDT")){
    IDsDT <- WTotDT %>%
      distinct(ID, .keep_all = TRUE) %>%
      dplyr::select(c("ID", "pop")) %>%
      filter(ID %in% IDsSubSet)
  }
  
  H = matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T )
  
  #vector to host ratio
  m = Adults*100/H
  
  #demographic parameters
  muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
  muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  #epidemiological parameters
  A = (0.0043*tas + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
  phiA = phiAU*(H>RTh)+phiAR*(H<=RTh)
  
  EIPdengue = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021
  
  # Compute epidemiological risk
  R0dengueM = (A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue
  LTSdengueM[i,] = colSums(R0dengueM>1, na.rm =T)
}

rm(IDsDT)

AmjjasoMM[1,] <- colMeans(AmjjasoM, na.rm =T)
LTSdengueMM[1,] <- colMeans(LTSdengueM, na.rm =T)

## SSP2 RCP 4.5 2055----

name = "ssp245"
years = 2050:2059

files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_", name))
files = files[1:length(years)]

# matrices of indicators: average adults and R0

AmjjasoM = matrix(NA, nrow = length(years), ncol = nIDs)
LTSdengueM = matrix(NA, nrow = length(years), ncol = nIDs)

for(i in  1:length(years)){
  
  file = files[i]
  
  Sim <- readRDS(paste0(folderSim, "/", file))
  year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
  
  #determine mjjaso
  nD <- nrow(Sim)
  FMay <- yday(as.Date(paste0(year, "-05-01"))) 
  LOct <- yday(as.Date(paste0(year, "-10-31"))) 
  
  Adults <- Sim[,3*nIDs + 1:nIDs]
  
  Amjjaso <- colMeans(Adults[FMay:LOct,], na.rm =T)
  AmjjasoM[i, ] <- Amjjaso
  
  # load weather for R0
  
  WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
    filter(ID %in% IDsSubSet) 
  
  tas = matrix(WTotDT$tas, nrow = nD)
  
  # load H (just once) for R0
  if(!exists("IDsDT")){
    IDsDT <- WTotDT %>%
      distinct(ID, .keep_all = TRUE) %>%
      dplyr::select(c("ID", "pop")) %>%
      filter(ID %in% IDsSubSet)
  }
  
  H = matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T )
  
  #vector to host ratio
  m = Adults*100/H
  
  #demographic parameters
  muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
  muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  #epidemiological parameters
  A = (0.0043*tas + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
  phiA = phiAU*(H>RTh)+phiAR*(H<=RTh)
  
  EIPdengue = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021
  
  # Compute epidemiological risk
  R0dengueM = (A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue
  LTSdengueM[i,] = colSums(R0dengueM>1, na.rm =T)
}

rm(IDsDT)

AmjjasoMM[2,] <- colMeans(AmjjasoM, na.rm =T)
LTSdengueMM[2,] <- colMeans(LTSdengueM, na.rm =T)

## SSP2 RCP 4.5 2085----

name = "ssp245"
years = 2080:2089

files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_", name))
files = files[length(years)+1:length(years)]

# matrices of indicators: average adults and R0

AmjjasoM = matrix(NA, nrow = length(years), ncol = nIDs)
LTSdengueM = matrix(NA, nrow = length(years), ncol = nIDs)

for(i in  1:length(years)){
  
  file = files[i]
  
  Sim <- readRDS(paste0(folderSim, "/", file))
  year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
  
  #determine mjjaso
  nD <- nrow(Sim)
  FMay <- yday(as.Date(paste0(year, "-05-01"))) 
  LOct <- yday(as.Date(paste0(year, "-10-31"))) 
  
  Adults <- Sim[,3*nIDs + 1:nIDs]
  
  Amjjaso <- colMeans(Adults[FMay:LOct,], na.rm =T)
  AmjjasoM[i, ] <- Amjjaso
  
  # load weather for R0
  
  WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
    filter(ID %in% IDsSubSet) 
  
  tas = matrix(WTotDT$tas, nrow = nD)
  
  # load H (just once) for R0
  if(!exists("IDsDT")){
    IDsDT <- WTotDT %>%
      distinct(ID, .keep_all = TRUE) %>%
      dplyr::select(c("ID", "pop")) %>%
      filter(ID %in% IDsSubSet)
  }
  
  H = matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T )
  
  #vector to host ratio
  m = Adults*100/H
  
  #demographic parameters
  muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
  muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  #epidemiological parameters
  A = (0.0043*tas + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
  phiA = phiAU*(H>RTh)+phiAR*(H<=RTh)
  
  EIPdengue = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021
  
  # Compute epidemiological risk
  R0dengueM = (A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue
  LTSdengueM[i,] = colSums(R0dengueM>1, na.rm =T)
}

rm(IDsDT)

AmjjasoMM[3,] <- colMeans(AmjjasoM, na.rm =T)
LTSdengueMM[3,] <- colMeans(LTSdengueM, na.rm =T)


## SSP5 RCP 8.5 2055----

name = "ssp585"
years = 2050:2059

files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_", name))
files = files[1:length(years)]

# matrices of indicators: average adults and R0

AmjjasoM = matrix(NA, nrow = length(years), ncol = nIDs)
LTSdengueM = matrix(NA, nrow = length(years), ncol = nIDs)

for(i in  1:length(years)){
  
  file = files[i]
  
  Sim <- readRDS(paste0(folderSim, "/", file))
  year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
  
  #determine mjjaso
  nD <- nrow(Sim)
  FMay <- yday(as.Date(paste0(year, "-05-01"))) 
  LOct <- yday(as.Date(paste0(year, "-10-31"))) 
  
  Adults <- Sim[,3*nIDs + 1:nIDs]
  
  Amjjaso <- colMeans(Adults[FMay:LOct,], na.rm =T)
  AmjjasoM[i, ] <- Amjjaso
  
  # load weather for R0
  
  WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
    filter(ID %in% IDsSubSet) 
  
  tas = matrix(WTotDT$tas, nrow = nD)
  
  # load H (just once) for R0
  if(!exists("IDsDT")){
    IDsDT <- WTotDT %>%
      distinct(ID, .keep_all = TRUE) %>%
      dplyr::select(c("ID", "pop")) %>%
      filter(ID %in% IDsSubSet)
  }
  
  H = matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T )
  
  #vector to host ratio
  m = Adults*100/H
  
  #demographic parameters
  muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
  muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  #epidemiological parameters
  A = (0.0043*tas + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
  phiA = phiAU*(H>RTh)+phiAR*(H<=RTh)
  
  EIPdengue = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021
  
  # Compute epidemiological risk
  R0dengueM = (A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue
  LTSdengueM[i,] = colSums(R0dengueM>1, na.rm =T)
}

rm(IDsDT)

AmjjasoMM[4,] <- colMeans(AmjjasoM, na.rm =T)
LTSdengueMM[4,] <- colMeans(LTSdengueM, na.rm =T)

## SSP5 RCP 8.5 2085----

name = "ssp585"
years = 2080:2089

files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_", name))
files = files[length(years)+1:length(years)]

# matrices of indicators: average adults and R0

AmjjasoM = matrix(NA, nrow = length(years), ncol = nIDs)
LTSdengueM = matrix(NA, nrow = length(years), ncol = nIDs)

for(i in  1:length(years)){
  
  file = files[i]
  
  Sim <- readRDS(paste0(folderSim, "/", file))
  year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
  
  #determine mjjaso
  nD <- nrow(Sim)
  FMay <- yday(as.Date(paste0(year, "-05-01"))) 
  LOct <- yday(as.Date(paste0(year, "-10-31"))) 
  
  Adults <- Sim[,3*nIDs + 1:nIDs]
  
  Amjjaso <- colMeans(Adults[FMay:LOct,], na.rm =T)
  AmjjasoM[i, ] <- Amjjaso
  
  # load weather for R0
  
  WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
    filter(ID %in% IDsSubSet) 
  
  tas = matrix(WTotDT$tas, nrow = nD)
  
  # load H (just once) for R0
  if(!exists("IDsDT")){
    IDsDT <- WTotDT %>%
      distinct(ID, .keep_all = TRUE) %>%
      dplyr::select(c("ID", "pop")) %>%
      filter(ID %in% IDsSubSet)
  }
  
  H = matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T )
  
  #vector to host ratio
  m = Adults*100/H
  
  #demographic parameters
  muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
  muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  #epidemiological parameters
  A = (0.0043*tas + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
  phiA = phiAU*(H>RTh)+phiAR*(H<=RTh)
  
  EIPdengue = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021
  
  # Compute epidemiological risk
  R0dengueM = (A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue
  LTSdengueM[i,] = colSums(R0dengueM>1, na.rm =T)
}

rm(IDsDT)

AmjjasoMM[5,] <- colMeans(AmjjasoM, na.rm =T)
LTSdengueMM[5,] <- colMeans(LTSdengueM, na.rm =T)

# Save and load----
saveRDS(AmjjasoMM, file = paste0(folderSim, "/AmjjasoMM.rds"))
saveRDS(LTSdengueMM, file = paste0(folderSim, "/LTSdengueMM.rds"))

AmjjasoMM <- readRDS(file = paste0(folderSim, "/AmjjasoMM.rds"))
LTSdengueMM <- readRDS(file = paste0(folderSim, "/LTSdengueMM.rds"))

# Plot----

# Load map
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

## Adults ----

cutPal = c(10^3, 10^2, 10^1, 1, 1)
cutPalLab = c("e > 1000", "d > 100", "c > 10", "b > 1", "a < 1")
colPal<- c( "#a63603", "#a63603", "#e6550d", "#fd8d3c", "#fdbe85", "#feedde")

# Cycle

for(i in 1:nrow(scenariosDF)){
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  AmjjasoCut <- case_when(AmjjasoMM[i,] >= cutPal[1] ~ cutPalLab[1],
                         AmjjasoMM[i,] >= cutPal[2] ~ cutPalLab[2],
                         AmjjasoMM[i,] >= cutPal[3] ~ cutPalLab[3],
                         AmjjasoMM[i,] >= cutPal[4] ~ cutPalLab[4],
                         AmjjasoMM[i,] <= cutPal[4] ~ cutPalLab[5])
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = AmjjasoCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("LTS (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))
  
  ggsave(file = 
           paste0(folderPlotA, "/Amjjaso_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut , units="in", width=5.5, height=7, dpi=300)

}

## R0 ----

cutPal = c(105, 56, 21, 1, 0)
cutPalLab = c("e 15 or more", "d 8 to 15", "c 3 to 8", "b 0 to 3", "a 0")
colPal<- c("#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724")

# Cycle

for(i in 1:nrow(scenariosDF)){
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  LTSSelCut <- case_when(LTSdengueMM[i,] >= cutPal[1] ~ cutPalLab[1],
                         LTSdengueMM[i,] >= cutPal[2] ~ cutPalLab[2],
                         LTSdengueMM[i,] >= cutPal[3] ~ cutPalLab[3],
                         LTSdengueMM[i,] >= cutPal[4] ~ cutPalLab[4],
                         LTSdengueMM[i,] <= cutPal[4] ~ cutPalLab[5])
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = LTSSelCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("LTS (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))
  
  ggsave(file = 
           paste0(folderPlotR0, "/LTS_dengue_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut , units="in", width=5.5, height=7, dpi=300)
  
}

