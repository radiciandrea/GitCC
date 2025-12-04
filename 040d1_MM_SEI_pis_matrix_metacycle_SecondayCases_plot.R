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

SecCaseJanMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseFebMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseMarMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseAprMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseMayMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseJunMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseJulMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseAugMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseSepMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseOctMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseNovMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix
SecCaseDecMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF)) # Secondary Cases metamatrix

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
  
  # matrices of indicators
  SecCaseM = matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  
  SecCaseJanM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseFebM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseMarM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseAprM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseMayM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseJunM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseJulM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseAugM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseSepM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseOctM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseNovM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  SecCaseDecM <- matrix(NA, nrow = length(years), ncol = nIDs) # Secondary Cases matrix
  
  for(i in  1:length(years)){
    
    fileSH = filesSH[i]
    
    year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
    
    WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
      filter(ID %in% IDsSubSet) 
    
    IDsDT <- WTotDT %>%
      distinct(ID, .keep_all = TRUE) %>%
      dplyr::select(c("ID", "pop", "surfHa")) %>%
      filter(ID %in% IDsSubSet)
    
    #reshape human matrix
    H = matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T )
    
    #area as (km²)
    AreaKm2 = IDsDT$surfHa*10^-2
    
    # Secondary Cases one introduction per month---- 
    # let's take "the worst"
    
    SH <- readRDS(paste0(folderSim, "/", fileSH))
    
    MaxSH <-sapply(1:ncol(SH), function(x){max(SH[,x])})
    
    MinSHJan <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHFeb <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHMar <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHApr <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHMay <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHJun <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHJul <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHAug <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHSep <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHOct <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHNov <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    MinSHDec <-sapply(1:ncol(SH), function(x){min(SH[,x])})
    
    MaxSecondayCasesJan = 100*(MaxSH - MinSHJan)*AreaKm2 # 100 per ha
    MaxSecondayCasesFeb = 100*(MaxSH - MinSHFeb)*AreaKm2 # 100 per ha
    MaxSecondayCasesMar = 100*(MaxSH - MinSHMar)*AreaKm2 # 100 per ha
    MaxSecondayCasesApr = 100*(MaxSH - MinSHApr)*AreaKm2 # 100 per ha
    MaxSecondayCasesMay = 100*(MaxSH - MinSHMay)*AreaKm2 # 100 per ha
    MaxSecondayCasesJun = 100*(MaxSH - MinSHJun)*AreaKm2 # 100 per ha
    MaxSecondayCasesJul = 100*(MaxSH - MinSHJul)*AreaKm2 # 100 per ha
    MaxSecondayCasesAug = 100*(MaxSH - MinSHAug)*AreaKm2 # 100 per ha
    MaxSecondayCasesSep = 100*(MaxSH - MinSHSep)*AreaKm2 # 100 per ha
    MaxSecondayCasesOct = 100*(MaxSH - MinSHOct)*AreaKm2 # 100 per ha
    MaxSecondayCasesNov = 100*(MaxSH - MinSHNov)*AreaKm2 # 100 per ha
    MaxSecondayCasesDec = 100*(MaxSH - MinSHDec)*AreaKm2 # 100 per ha
    
    
    SecCaseJanM[i, ] <- MaxSecondayCasesJan
    SecCaseFebM[i, ] <- MaxSecondayCasesFeb
    SecCaseMarM[i, ] <- MaxSecondayCasesMar
    SecCaseAprM[i, ] <- MaxSecondayCasesApr
    SecCaseMayM[i, ] <- MaxSecondayCasesMay
    SecCaseJunM[i, ] <- MaxSecondayCasesJun
    SecCaseJulM[i, ] <- MaxSecondayCasesJul
    SecCaseAugM[i, ] <- MaxSecondayCasesAug
    SecCaseSepM[i, ] <- MaxSecondayCasesSep
    SecCaseOctM[i, ] <- MaxSecondayCasesOct
    SecCaseNovM[i, ] <- MaxSecondayCasesNov
    SecCaseDecM[i, ] <- MaxSecondayCasesDec
    
  }
  
  rm(IDsDT)
  
  SecCaseJanMM[k,] <- colMeans(SecCaseJanM, na.rm =T)
  SecCaseFebMM[k,] <- colMeans(SecCaseFebM, na.rm =T)
  SecCaseMarMM[k,] <- colMeans(SecCaseMarM, na.rm =T)
  SecCaseAprMM[k,] <- colMeans(SecCaseAprM, na.rm =T)
  SecCaseMayMM[k,] <- colMeans(SecCaseMayM, na.rm =T)
  SecCaseJunMM[k,] <- colMeans(SecCaseJunM, na.rm =T)
  SecCaseJulMM[k,] <- colMeans(SecCaseJulM, na.rm =T)
  SecCaseAugMM[k,] <- colMeans(SecCaseAugM, na.rm =T)
  SecCaseSepMM[k,] <- colMeans(SecCaseSepM, na.rm =T)
  SecCaseOctMM[k,] <- colMeans(SecCaseOctM, na.rm =T)
  SecCaseNovMM[k,] <- colMeans(SecCaseNovM, na.rm =T)
  SecCaseDecMM[k,] <- colMeans(SecCaseDecM, na.rm =T)
  
}

SecCaseList = list(SecCaseJanMM,
                   SecCaseFebMM,
                   SecCaseMarMM,
                   SecCaseAprMM,
                   SecCaseMayMM,
                   SecCaseJunMM,
                   SecCaseJulMM,
                   SecCaseAugMM,
                   SecCaseSepMM,
                   SecCaseOctMM,
                   SecCaseNovMM,
                   SecCaseDecMM)
# Save and load----
saveRDS(SecCaseList, file = paste0(folderSim, "/SecCaseList.rds"))

SecCaseList <- readRDS(file = paste0(folderSim, "/SecCaseList.rds"))

# Plot----

# Load map
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

### Secondary cases ----

cutPal = c(50, 20, 5, 1, 1)
cutPalLab = c("e > 50", "d > 20", "c > 5", "b > 1", "a < 1")
colPal<- c("#fcfdbf", "#fc8961", "#b73779", "#51127c", "#000004")

mont_v = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Cycle
for(j in 1:length(SecCaseList)){
  SecCaseMM <- SecCaseList[[j]]
  
  mont_j = mont_v[j]
  
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
             paste0(folderPlot, "/SecCase_", name, "_", min(years), "-", max(years), "_", mont_j, ".png"),
           plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
    
    cat("name:", name, ", month:", mont_j, "SC>1: ", round(100*sum(SecCaseMM[i,]>1, na.rm = T)/8981, 0), "\n")
    
  }
}

