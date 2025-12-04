# code to prot abundances of secondary cases, and why not adults, R0 

# inspired by both MM_pis_matrix_EOBS_cycle_consec_compute_plot_R0.R
# and MM_pis_matrix_EOBS_cycle_consec_01_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

mod = "" # "" = CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63, cold = MPI-M-MPI-ESM-LR_MPI-CSC-REMO2009, hot = MOHC-HadGEM2-ES_CLMcom-CCLM4-8-17

folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_040a1")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod, "_sim_040a1")
folderDrias = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
folderShape = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"

# scenarios
scenariosDF= data.frame(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(1986, 2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(1986, 2026, 2046, 2066, 2026, 2046, 2066)+19)

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

# if(!exists("AreaKm2")){
#   DomainSf <- st_read(paste0(folderShape, "/SafranDensOmphale.shp"))
#   AreaKm2 = DomainSf$surf_ha/100 # approximate surface of each cell (max rel error: 0.44%)
# }

# create meta matrices for each scenario (hist, ssp2, ssp5) for both adults and MTS for dengue

SecCaseMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix

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
  
  filesSH <- list.files(paste0(folderSim,"/"), paste0("040a_SH_Drias_SEIS_", name))
  filesAdults <- list.files(paste0(folderSim,"/"), paste0("040a_Adults_Drias_SEIS_", name))
  
  # matrices of indicators
  SecCaseM = matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  
  for(i in  1:length(years)){
    
    fileAdults = filesAdults[i]
    fileSH = filesSH[i]
    
    Adults <- readRDS(paste0(folderSim, "/", fileAdults))
    year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
    
    #determine mjjaso
    nD <- nrow(Adults)
    FMay <- yday(as.Date(paste0(year, "-05-01"))) 
    LOct <- yday(as.Date(paste0(year, "-10-31"))) 
    
    ### Adults ----
    Amjjaso <- colMeans(Adults[FMay:LOct,], na.rm =T)
    AmjjasoM[i, ] <- Amjjaso
    
    # load weather for R0
    
    WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
      filter(ID %in% IDsSubSet) 
    
    tas = matrix(WTotDT$tas, nrow = nD)
    
    IDsDT <- WTotDT %>%
      distinct(ID, .keep_all = TRUE) %>%
      dplyr::select(c("ID", "pop", "surfHa")) %>%
      filter(ID %in% IDsSubSet)
    
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
  
  SecCaseMM[k,] <- colMeans(SecCaseM, na.rm =T)

}

# Save and load----
saveRDS(SecCaseMM, file = paste0(folderSim, "/SecCaseMM.rds"))

SecCaseMM <- readRDS(file = paste0(folderSim, "/SecCaseMM.rds"))


# Plot----

# Load map
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

### Secondary cases ----

cutPal = c(50, 20, 5, 1, 1)
cutPalLab = c("e > 50", "d > 20", "c > 5", "b > 1", "a < 1")
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
    ggtitle(paste0("Secondary cases, scenario: ", name, "; period: ", min(years), "-", max(years)))+
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
           paste0(folderPlot, "/SecCase_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", SC>1: ", round(100*sum(SecCaseMM[i,]>1, na.rm = T)/8981, 0), "\n")
  
}
