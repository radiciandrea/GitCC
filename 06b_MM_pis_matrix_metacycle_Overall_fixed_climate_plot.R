#Plot E0 + Adults + ----

# Inspired by 
# ModelMetelmann_pis_matrix_EOBS_cycle_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

mod = "" # "" = CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63, cold = MPI-M-MPI-ESM-LR_MPI-CSC-REMO2009, hot = MOHC-HadGEM2-ES_CLMcom-CCLM4-8-17

folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_06a")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod, "_sim_06a")
folderDrias = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
folderShape = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"

dir.create(folderPlot)

files = list.files(paste0(folderSim,"/"), pattern = "02_")

namesAll = substring(files, nchar(files)-12, nchar(files)-9)
# yearsAll = substring(files, nchar(files)-7, nchar(files)-4) 

# load for 1 dimension
E01 <- readRDS(paste0(folderSim, "/", files[1]))

E0m = matrix(NA, ncol = length(E01), nrow = length(files))

for (i in 1:length(files)){
  file = files[i]
  E0v <- readRDS(paste0(folderSim, "/", file))
  E0m[i,]= E0v
}

# Load map
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

# Metelmann palette & cuts
colPal= c("#384AB4", "#5570DF", "#8EB0FE", "#C5D7F3", "#F2CDBB", "#F29878", "#D04B45", "#B00026")
cutPal = c(0, 10^(-3:3), 10^10)
dev.new(height=3)
# scenarios

scenariosDF= data.frame(name = c("Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(2026, 2046, 2066, 2026, 2046, 2066)+19)


for(k in 1:nrow(scenariosDF)){
  
  name = scenariosDF$name[k]
  years = scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  nR <- which(namesAll == name)
  
  E0Sel <-  apply(E0m[nR,], 2,
                  function(x){exp(mean(log(x)))})
  
  E0SelCut <- cut(E0Sel, breaks=cutPal,
                  labels=sapply(cutPal [-length(cutPal )], function(x){paste0(">", as.character(x))}))
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = E0SelCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("E0, ", name, ", ", min(years), "-", max(years)))+
    theme(plot.background  = element_blank(),
          aspect.ratio = 1)
  
  if(!(name %in% c("Cn70", "Hg70"))){
    plotCut <- plotCut +
      theme(legend.position = "none")
  }
  
  ggsave(file = 
           paste0(folderPlot, "/E0_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", E0>1: ", round(100*sum(E0Sel>1, na.rm = T)/8981, 0), "\n")
  
}

# Same but of 04

## initial settings----

# load 1 for dimension
name = "Hs"
filesAdults = list.files(paste0(folderSim,"/"), pattern = "Adults")
filesSH = list.files(paste0(folderSim,"/"), pattern = "SH")
Adults <- readRDS(paste0(folderSim, "/", filesAdults[1]))
nC = 8 # number of classes: 5 + exposed + infexted + susceptible hosts
nIDs = ncol(Adults) # number of regions
IDs = 1:nIDs
IDsSubSet = IDs

# create meta matrices for each scenario (hist, ssp2, ssp5) for both adults and MTS for dengue

AmjjasoMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
LTSdengueMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
SecCaseMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
PrevMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # prevalence metamatrix

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
  
  # matrices of indicators
  
  AmjjasoM = matrix(NA, nrow = length(years), ncol = nIDs)
  LTSdengueM = matrix(NA, nrow = length(years), ncol = nIDs)
  SecCaseM = matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  PrevM = matrix(NA, nrow = length(years), ncol = nIDs)  # prevalence matrix
  
  for(i in  1:length(years)){
    
    fileAdults = filesAdults[i]
    fileSH = filesSH[i]
    
    ### Adults ----
    Adults <- readRDS(paste0(folderSim, "/", fileAdults))
    
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
    
    # load H (just once) for R0
    if(!exists("IDsDT")){
      IDsDT <- WTotDT %>%
        distinct(ID, .keep_all = TRUE) %>%
        dplyr::select(c("ID", "pop", "surfHa")) %>%
        filter(ID %in% IDsSubSet)
    }
    
    #reshape human matrix
    H = matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T )
    
    #area as (km²)
    AreaKm2 = IDsDT$surfHa*10^-2
    
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
    
    SH <- readRDS(paste0(folderSim, "/", fileSH))

    MaxSH <-sapply(1:ncol(SH), function(x){max(SH[,x])})
    MinSH <-sapply(1:ncol(SH), function(x){min(SH[,x])})

    MaxSecondayCases = 100*(MaxSH - MinSH)*AreaKm2 # 100 per ha
    SecCaseM[i, ] <- MaxSecondayCases

    MaxPrevHost = 100*(1 - MinSH/MaxSH)
    PrevM[i,] = MaxPrevHost
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
colPal<- c( "#a63603", "#e6550d", "#fd8d3c", "#fdbe85", "#feedde")

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

### Secondary cases ----

cutPal = c(20, 5, 1, 0.5, 0.5)
cutPalLab = c("e > 20", "d > 5", "c > 1", "b > 0.5", "a < 0.5")
colPal<- c("#fcfdbf", "#fc8961", "#b73779", "#51127c", "#000004")

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
    ggtitle(paste0("Secondary cases (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))+
    theme(plot.background  = element_blank(),
          aspect.ratio = 1)
  
  if(!(name %in% c("Cn70", "Hg70"))){
    plotCut <- plotCut +
      theme(legend.position = "none")
  }
  
  ggsave(file = 
           paste0(folderPlot, "/SecCase_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
}

### Prevalence ----

cutPal = c(20, 5, 1, 0.1, 0.1)
cutPalLab = c("e > 20 %", "d > 5 %", "c > 1 %", "b > 0.1 %", "a  < 0.1 %")
colPal<- c("#0d0887", "#7e03a8", "#cc4778", "#f89540", "#f0f921")

# Cycle

for(i in 1:nrow(scenariosDF)){
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  PrevSelCut <- case_when(PrevMM[i,] >= cutPal[1] ~ cutPalLab[1],
                          PrevMM[i,] >= cutPal[2] ~ cutPalLab[2],
                          PrevMM[i,] >= cutPal[3] ~ cutPalLab[3],
                          PrevMM[i,] >= cutPal[4] ~ cutPalLab[4],
                          PrevMM[i,] <= cutPal[4] ~ cutPalLab[5])
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = PrevSelCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("Prev (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))+
    theme(plot.background  = element_blank(),
          aspect.ratio = 1)
  
  if(!(name %in% c("Cn70", "Hg70"))){
    plotCut <- plotCut +
      theme(legend.position = "none")
  }
  
  ggsave(file = 
           paste0(folderPlot, "/Prev_dengue_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  
}

