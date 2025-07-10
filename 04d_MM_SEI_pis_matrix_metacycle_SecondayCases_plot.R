# code to prot abundances of secondary cases, and why not adults, R0 

# inspired by both MM_pis_matrix_EOBS_cycle_consec_compute_plot_R0.R
# and MM_pis_matrix_EOBS_cycle_consec_01_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

folderSim = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_04"
folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
folderPlotSim4 = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/Sim_SEI_4"


## initial settings----

# load 1 for dimension
name = "Hist"
files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_SEIS_", name))
Sim <- readRDS(paste0(folderSim, "/", files[1]))
nC = 8 # number of classes: 5 + exposed + infexted + susceptible hosts
nIDs = (ncol(Sim)-1)/nC # number of regions
IDs = 1:nIDs
IDsSubSet = IDs

if(!exists("AreaKm2")){
  AreaKm2 = 63.735 # approximate surface of each cell (max rel error: 0.44%)
}

# scenarios

scenariosDF= data.frame(name = c("Hist", "ssp245", "ssp245", "ssp585", "ssp585"),
                        yearStart = c(1996, 2050, 2080, 2050, 2080),
                        yearEnd = c(1996, 2050, 2080, 2050, 2080)+9)

# create meta matrices for each scenario (hist, ssp2, ssp5) for both adults and MTS for dengue

AmjjasoMM <- matrix(NA, ncol = nIDs, nrow = 5)
LTSdengueMM <- matrix(NA, ncol = nIDs, nrow = 5)
SecCaseMM <- matrix(NA, ncol = nIDs, nrow = 5) # Secondary Cases metamatrix
PrevMM <- matrix(NA, ncol = nIDs, nrow = 5) # prevalence metamatrix

#host preference
phiAU = 0.9 #human biting preference (urban)
phiAR = 0.5 #human biting preference (rural) #Caminade 2016
RTh = 50 #defines density over km2 below which an area is "rural", with little phi

IIPdengue = 5 #Benkimoun 2021

bV2H = 0.5 # b Blagrove 2020
bH2Vdengue = 0.31 # beta Metelmann 2021

## cycle to calculate indicators ----

for(k in 1:nrow(scenariosDF)){
  name = scenariosDF$name[k]
  years = scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  
  files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_SEIS_", name))
  
  # matrices of indicators: average adults and R0
  
  SecCaseM = matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  PrevM = matrix(NA, nrow = length(years), ncol = nIDs)  # prevalence matrix
  
  for(i in  1:length(years)){
    
    file = files[i]
    
    Sim <- readRDS(paste0(folderSim, "/", file))
    year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
    
    #determine mjjaso
    nD <- nrow(Sim)
    FMay <- yday(as.Date(paste0(year, "-05-01"))) 
    LOct <- yday(as.Date(paste0(year, "-10-31"))) 
    
    ### Adults ----
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
    
    ## LTS----
    R0dengueM = (A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue
    LTSdengueM[i,] = colSums(R0dengueM>1, na.rm =T)
    
    # Secondary Cases and Prevalence: one introduction per month---- 
    # let's take "the worst"
    
    SH <- Sim[,7*nIDs + 1:nIDs]
    
    MaxSH <-sapply(1:ncol(SH), function(x){max(SH[,x])})
    MinSH <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    
    MaxSecondayCases = 100*(MaxSH - MinSH)*AreaKm2 # 100 per ha
    SecCaseM[i, ] <- MaxPrevHost 
    
    MaxPrevHost = 100*(1 - MinSH/MaxSH)
    PrevM[i,] = colSums(R0dengueM>1, na.rm =T)
  }
  
  rm(IDsDT)
  
  AmjjasoMM[k,] <- colMeans(AmjjasoM, na.rm =T)
  LTSdengueMM[k,] <- colMeans(LTSdengueM, na.rm =T)
  SecCaseMM[k,] <- colMeans(SecCaseM, na.rm =T)
  PrevMM[k,] <- colMeans(PrevM, na.rm =T)
}

# Save and load----
saveRDS(AmjjasoMM, file = paste0(folderSim, "/AmjjasoMM.rds"))
saveRDS(LTSdengueMM, file = paste0(folderSim, "/LTSdengueMM.rds"))
saveRDS(SecCaseMM, file = paste0(folderSim, "/SecCaseMM.rds"))
saveRDS(PrevMM, file = paste0(folderSim, "/PrevMM.rds"))

AmjjasoMM <- readRDS(file = paste0(folderSim, "/AmjjasoMM.rds"))
LTSdengueMM <- readRDS(file = paste0(folderSim, "/LTSdengueMM.rds"))
SecCaseMM <- readRDS(file = paste0(folderSim, "/SecCaseMM.rds"))
PrevMM <- readRDS(file = paste0(folderSim, "/PrevMM.rds"))

# Plot----

# Load map
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

### Adults ----

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
           paste0(folderPlotSim4, "/Amjjaso_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut , units="in", width=5.5, height=7, dpi=300)
  
}

### R0 ----

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
           paste0(folderPlotSim40, "/LTS_dengue_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut , units="in", width=5.5, height=7, dpi=300)
  
  
}

### Secondary cases ----

cutPal = c(10^3, 10^2, 10^1, 1, 1)
cutPalLab = c("e > 1000", "d > 100", "c > 10", "b > 1", "a < 1")
colPal<- c( "#a63603", "#a63603", "#e6550d", "#fd8d3c", "#fdbe85", "#feedde")

# Cycle

for(i in 1:nrow(scenariosDF)){
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  SecCaseCut <- case_when(SecCaseMM[i,] >= cutPal[1] ~ cutPalLab[1],
                          SecCaseMM[i,] >= cutPal[2] ~ cutPalLab[2],
                          SecCaseMM[i,] >= cutPal[3] ~ cutPalLab[3],
                          SecCaseMM[i,] >= cutPal[4] ~ cutPalLab[4],
                          SecCaseMM[i,] <= cutPal[4] ~ cutPalLab[5])
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = SecCaseCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("LTS (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))
  
  ggsave(file = 
           paste0(folderPlotSim4, "/SecCase_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut , units="in", width=5.5, height=7, dpi=300)
  
}

### Prevalence ----

cutPal = c(105, 56, 21, 1, 0)
cutPalLab = c("e 15 or more", "d 8 to 15", "c 3 to 8", "b 0 to 3", "a 0")
colPal<- c("#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724")

# Cycle

for(i in 1:nrow(scenariosDF)){
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  PrevSelCut <- case_when(PrevdengueMM[i,] >= cutPal[1] ~ cutPalLab[1],
                         PrevdengueMM[i,] >= cutPal[2] ~ cutPalLab[2],
                         PrevdengueMM[i,] >= cutPal[3] ~ cutPalLab[3],
                         PrevdengueMM[i,] >= cutPal[4] ~ cutPalLab[4],
                         PrevdengueMM[i,] <= cutPal[4] ~ cutPalLab[5])
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = PrevSelCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("Prev (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))
  
  ggsave(file = 
           paste0(folderPlotSim40, "/Prev_dengue_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut , units="in", width=5.5, height=7, dpi=300)
  
  
}

