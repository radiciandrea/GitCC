# code to prot abundances of Adults, R0, LTSR0

# inspired by both MM_pis_matrix_EOBS_cycle_consec_compute_plot_R0.R
# and MM_pis_matrix_EOBS_cycle_consec_01_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

folderSim = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_04e"
folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
folderShape = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"
folderPlotSim = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/Drias_sim_04e"

## initial settings----

# load 1 for dimension
name = "Hs"
nC = 8 # number of classes: 5 + exposed + infexted + susceptible hosts
nIDs = 8981 # number of regions
IDs = 1:nIDs
IDsSubSet = IDs

# scenarios

scenariosDF= data.frame(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(1986, 2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(1986, 2026, 2046, 2066, 2026, 2046, 2066)+19)

# create meta matrices for each scenario (hist, ssp2, ssp5) for both Adults and MTS for dengue

AmjjasoMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
LTSR0dengueMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
LTSSecCaseMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix

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
  LTSR0dengueM = matrix(NA, nrow = length(years), ncol = nIDs)
  LTSSecCaseM = matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  
  filesSH <- list.files(paste0(folderSim,"/"), paste0("04e_SH_Drias_SEIS_", name))
  filesAdults <- list.files(paste0(folderSim,"/"), paste0("04e_Adults_Drias_SEIS_", name))
  
  for(i in  1:length(years)){
    
    fileAdults = filesAdults[i]
    fileSH = filesSH[i]
    
    #load Adults
    Adults <- readRDS(paste0(folderSim, "/", fileAdults))
    
    year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
    
    #determine mjjaso
    nD <- nrow(Adults)
    FMay <- yday(as.Date(paste0(year, "-05-01"))) 
    LOct <- yday(as.Date(paste0(year, "-10-31"))) 
    
    ### Elaborate adults ----
 
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
    SurfHaM = matrix(rep(IDsDT$surfHa, nD), nrow = nD, byrow = T )
    
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
    LTSR0dengueM[i,] = colSums(R0dengueM>1, na.rm =T)
    
    # Secondary Cases---- 
    
    SH <- readRDS(paste0(folderSim, "/", fileSH))
    
    SHabs <- SH*SurfHaM
    
    # start and end: when the first and the last infections are produced. we round at 0.5
    EpiStart <- sapply(IDs, function(x){which(SHabs[,x]<(max(SHabs[,x])-0.5))[1]-1})
    EpiEnd <- sapply(IDs, function(x){which(SHabs[,x]<(min(SHabs[,x])+0.5))[1]+1})
  
    LTSSecCaseM[i, ] <- pmax(EpiEnd - EpiStart, 0)
  
  }
  
  rm(IDsDT)
  
  AmjjasoMM[k,] <- colMeans(AmjjasoM, na.rm =T)
  LTSR0dengueMM[k,] <- colMeans(LTSR0dengueM, na.rm =T)
  LTSSecCaseMM[k,] <- colMeans(LTSSecCaseM, na.rm =T)
}

# Save and load----
saveRDS(AmjjasoMM, file = paste0(folderSim, "/AmjjasoMM.rds"))
saveRDS(LTSR0dengueMM, file = paste0(folderSim, "/LTSR0dengueMM.rds"))
saveRDS(LTSSecCaseMM, file = paste0(folderSim, "/LTSSecCaseMM.rds"))

AmjjasoMM <- readRDS(file = paste0(folderSim, "/AmjjasoMM.rds"))
LTSR0dengueMM <- readRDS(file = paste0(folderSim, "/LTSR0dengueMM.rds"))
LTSSecCaseMM <- readRDS(file = paste0(folderSim, "/LTSSecCaseMM.rds"))

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
           paste0(folderPlotSim, "/Amjjaso_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
}

### LTS R0 ----

cutPal = c(105, 56, 21, 1, 0)
cutPalLab = c("e 15 or more", "d 8 to 15", "c 3 to 8", "b 0 to 3", "a 0")
colPal<- c("#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724")

# Cycle

for(i in 1:nrow(scenariosDF)){
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  LTSR0SelCut <- case_when(LTSR0dengueMM[i,] >= cutPal[1] ~ cutPalLab[1],
                         LTSR0dengueMM[i,] >= cutPal[2] ~ cutPalLab[2],
                         LTSR0dengueMM[i,] >= cutPal[3] ~ cutPalLab[3],
                         LTSR0dengueMM[i,] >= cutPal[4] ~ cutPalLab[4],
                         LTSR0dengueMM[i,] <= cutPal[4] ~ cutPalLab[5])
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = LTSR0SelCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("LTSR0 (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))+
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
           paste0(folderPlotSim, "/LTSR0_dengue_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", LTSR0>1: ", round(100*sum(LTSR0dengueMM[i,]>1, na.rm = T)/8981, 0), "\n")
  
}

### LTS Secondary cases ----

cutPal = c(105, 56, 21, 1, 0)
cutPalLab = c("e 15 or more", "d 8 to 15", "c 3 to 8", "b 0 to 3", "a 0")
colPal<- c("#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724")

# Cycle

for(i in 1:nrow(scenariosDF)){
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  LTSSecCaseSelCut <- case_when(LTSSecCasedengueMM[i,] >= cutPal[1] ~ cutPalLab[1],
                           LTSSecCasedengueMM[i,] >= cutPal[2] ~ cutPalLab[2],
                           LTSSecCasedengueMM[i,] >= cutPal[3] ~ cutPalLab[3],
                           LTSSecCasedengueMM[i,] >= cutPal[4] ~ cutPalLab[4],
                           LTSSecCasedengueMM[i,] <= cutPal[4] ~ cutPalLab[5])
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = LTSSecCaseSelCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("LTSSecCase (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))+
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
           paste0(folderPlotSim, "/LTSSecCase_dengue_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", LTSSecCase>1: ", round(100*sum(LTSSecCasedengueMM[i,]>1, na.rm = T)/8981, 0), "\n")
  
}

# ### Secondary cases ----
# 
# cutPal = c(20, 5, 1, 0.5, 0.5)
# cutPalLab = c("e > 20", "d > 5", "c > 1", "b > 0.5", "a < 0.5")
# colPal<- c("#fcfdbf", "#fc8961", "#b73779", "#51127c", "#000004")
# 
# # Cycle
# 
# for(i in 1:nrow(scenariosDF)){
#   name = scenariosDF$name[i]
#   years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
#   
#   LTSSecCaseCut <- case_when(LTSSecCaseMM[i,] >= cutPal[1] ~ cutPalLab[1],
#                           LTSSecCaseMM[i,] >= cutPal[2] ~ cutPalLab[2],
#                           LTSSecCaseMM[i,] >= cutPal[3] ~ cutPalLab[3],
#                           LTSSecCaseMM[i,] >= cutPal[4] ~ cutPalLab[4],
#                           LTSSecCaseMM[i,] <= cutPal[4] ~ cutPalLab[5])
#   
#   plotCut <- ggplot()+
#     geom_sf(data = domain, aes(fill = LTSSecCaseCut), colour = NA)+ #
#     scale_fill_manual(values = colPal)+
#     ggtitle(paste0("Secondary cases (dengue), scenario: ", name, "; period: ", min(years), "-", max(years)))+
#     theme(plot.background  = element_blank(),
#           aspect.ratio = 1)
#   
#   if(!(name %in% c("Cn70", "Hg70"))){
#     plotCut <- plotCut +
#       theme(legend.position = "none")
#   }
#   
#   ggsave(file = 
#            paste0(folderPlotSim, "/LTSSecCase_", name, "_", min(years), "-", max(years), ".png"),
#          plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
#   
# }
