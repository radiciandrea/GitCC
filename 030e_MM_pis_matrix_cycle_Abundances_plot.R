# code to prot abundances of adults and R0

# inspired by both MM_pis_matrix_EOBS_cycle_consec_compute_plot_R0.R
# and MM_pis_matrix_EOBS_cycle_consec_01_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

mod = "" # "" = CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63, cold = MPI-M-MPI-ESM-LR_MPI-CSC-REMO2009, hot = MOHC-HadGEM2-ES_CLMcom-CCLM4-8-17

folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_030")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod, "_sim_030")
folderDrias = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
folderShape = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"

## initial settings----

# load 1 for dimension
name = "Hs"

filesAdults = list.files(paste0(folderSim,"/"), pattern = "Adults")
Adults <- readRDS(paste0(folderSim, "/", filesAdults[1]))
nC = 5 # number of classes
nIDs = ncol(Adults) # number of regions
IDs = 1:nIDs
IDsSubSet = IDs

# scenarios

scenariosDF= data.frame(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(1986, 2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(1986, 2026, 2046, 2066, 2026, 2046, 2066)+19)

# create meta matrices for each scenario (hist, ssp2, ssp5) for both adults and MTS for dengue

AmjjasoMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
LTSdengueMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
DayHighestR07MM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))

#fixed epimelogical (meta)perameters

#host preference
phiAU = 0.9 #human biting preference (urban)
phiAR = 0.5 #human biting preference (rural) #Caminade 2016
RTh = 50 #defines density over km2 below which an area is "rural", with little phi

IIPdengue = 5 #Benkimoun 2021

bV2H = 0.5 # b Blagrove 2020
bH2Vdengue = 0.31 # beta Metelmann 2021

# Compute indicators per scenario----

## cycle

for(k in 1:nrow(scenariosDF)){
  
  name = scenariosDF$name[k]
  years = scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  filesAdults <- list.files(paste0(folderSim,"/"), paste0("030a_Adults_Drias_", name))
  
  # matrices of indicators: average adults and R0
  
  AmjjasoM = matrix(NA, nrow = length(years), ncol = nIDs)
  LTSdengueM = matrix(NA, nrow = length(years), ncol = nIDs)
  DayHighestR07M = matrix(NA, nrow = length(years), ncol = nIDs)
  
  for(i in  1:length(years)){
    
    file = filesAdults[i]
    
    Adults <- readRDS(paste0(folderSim, "/", file))
    year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
    
    #determine mjjaso
    nD <- nrow(Adults)
    FMay <- yday(as.Date(paste0(year, "-05-01"))) 
    LOct <- yday(as.Date(paste0(year, "-10-31"))) 
    
    Amjjaso <- colMeans(Adults[FMay:LOct,], na.rm =T)
    AmjjasoM[i, ] <- Amjjaso
    
    # load weather for R0
    
    WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
      filter(ID %in% IDsSubSet) 
    
    tas = matrix(WTotDT$tas, nrow = nD)
    
    IDsDT <- WTotDT %>%
      distinct(ID, .keep_all = TRUE) %>%
      dplyr::select(c("ID", "pop")) %>%
      filter(ID %in% IDsSubSet)
    
    H = matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T )
    
    #vector to host ratio
    m = Adults*100/H
    
    #demographic parameters
    muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
    muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
    
    #epidemiological parameters
    A = (0.0043*tas + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
    phiA = phiAU*(H>RTh)+phiAR*(H<=RTh)
    
    EIPdengue = 1.03*(4+exp(5.15 - 0.123*tas)) #Metelmann 2021
    
    # Compute epidemiological risk
    R0dengueM = (A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue
    LTSdengueM[i,] = colSums(R0dengueM>1, na.rm =T)
    
    R07M = t(sapply(2:nD, function(x){return(colMeans(R0dengueM[max(1,(x-6)):x,], na.rm = T))}))
    R07M[which(is.na(R07M))] = 0
    DayHighestR07M[i,] = apply(R07M, 2, which.max)
    
  }
  
  rm(IDsDT)
  
  AmjjasoMM[k,] <- colMeans(AmjjasoM, na.rm =T)
  LTSdengueMM[k,] <- colMeans(LTSdengueM, na.rm =T)
  DayHighestR07MM[k,] <- colMeans(DayHighestR07M, na.rm =T)
  
}

# Save and load----
saveRDS(AmjjasoMM, file = paste0(folderSim, "/AmjjasoMM.rds"))
saveRDS(LTSdengueMM, file = paste0(folderSim, "/LTSdengueMM.rds"))
saveRDS(DayHighestR07MM, file = paste0(folderSim, "/DayHighestR07MM.rds"))

AmjjasoMM <- readRDS(file = paste0(folderSim, "/AmjjasoMM.rds"))
LTSdengueMM <- readRDS(file = paste0(folderSim, "/LTSdengueMM.rds"))
DayHighestR07MM <- readRDS(file = paste0(folderSim, "/DayHighestR07MM.rds"))

# Plot----

# Load map
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

## Adults ----

cutPal = c(10^3, 10^2, 10^1, 1, 1)
cutPalLab = c("e > 1000", "d > 100", "c > 10", "b > 1", "a < 1")
colPal<- c("#feedde", "#fdbe85", "#fd8d3c", "#e6550d", "#a63603")

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
    ggtitle(paste0("Adult density, scenario: ", name, "; period: ", min(years), "-", max(years)))+
    theme(plot.background  = element_blank(),
          aspect.ratio = 1)
  
  if(!(name %in% c("Cn70", "Hg70"))){
    plotCut <- plotCut +
      theme(legend.position = "none")
  }
  
  ggsave(file = 
           paste0(folderPlot, "/Amjjaso_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", Adults>1: ", round(100*sum(AmjjasoMM[i,]>1, na.rm = T)/8981, 0), "\n")
  
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
    ggtitle(paste0("LTS (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))+
    theme(plot.background  = element_blank(),
          aspect.ratio = 1)
  
  if(!(name %in% c("Cn70", "Hg70"))){
    plotCut <- plotCut +
      theme(legend.position = "none")
  }
  
  ggsave(file = 
           paste0(folderPlot, "/LTS_dengue_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", LTS>1: ", round(100*sum(LTSdengueMM[i,]>1, na.rm = T)/8981, 0), "\n")
  
}

### Date at highest R0 ----
# Min. 1st Qu. Median    Mean   3rd Qu.    Max. 
# 10    218     225      225     233      249

cutPal = c(235, 227, 220, 213, 1)
cutPalLab = c("a Later than the 23 august", "b 15 - 22 August", "c 8 - 14 August", "d 1 - 7 August", "e Before the 1st of August")
colPal<- c("#ceefd6",  "#4dc2ad", "#347fa4", "#41478b", "#3c3063") #"#140910", "#e5f7ea"

# Cycle

for(i in 1:nrow(scenariosDF)){
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  DayHighestR07Cut <- case_when(DayHighestR07MM[i,] > cutPal[1] ~ cutPalLab[1],
                                DayHighestR07MM[i,] > cutPal[2] ~ cutPalLab[2],
                                DayHighestR07MM[i,] > cutPal[3] ~ cutPalLab[3],
                                DayHighestR07MM[i,] > cutPal[4] ~ cutPalLab[4],
                                DayHighestR07MM[i,] > cutPal[5] ~ cutPalLab[5],
                                .default = NA)
  
  length(table(DayHighestR07Cut))
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = DayHighestR07Cut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("Adult density, scenario: ", name, "; period: ", min(years), "-", max(years)))+
    theme(plot.background  = element_blank(),
          aspect.ratio = 1)
  
  # if(!(name %in% c("Cn70", "Hg70"))){
  plotCut <- plotCut +
    theme(legend.position = "none",
          panel.grid = element_blank(), 
          line = element_blank(), 
          rect = element_blank(), 
          text = element_blank(), 
          plot.background = element_rect(fill = "transparent", color = "transparent"))
  # }
  
  ggsave(file = 
           paste0(folderPlot, "/DayHighestR07_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", mean(date): ")
  print(as.Date(round(mean(DayHighestR07MM[i,], na.rm = T)), origin = "2024-12-31"))
  cat("\n")
  
}
