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
folderPlotA = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/Adults"
folderPlotR0 = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/R0"

## initial settings----

# load 1 for dimension
name = "Hist"
files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_SEIS_", name))
Sim <- readRDS(paste0(folderSim, "/", files[1]))
nC = 8 # number of classes: 5 + exposed + infexted + susceptible hosts
nIDs = (ncol(Sim)-1)/nC # number of regions
IDs = 1:nIDs
IDsSubSet = IDs

# scenarios

scenariosDF= data.frame(name = c("Hist", "ssp245", "ssp245", "ssp585", "ssp585"),
                        yearStart = c(1996, 2050, 2080, 2050, 2080),
                        yearEnd = c(1996, 2050, 2080, 2050, 2080)+10)

# create meta matrices for each scenario (hist, ssp2, ssp5) for both adults and MTS for dengue

SecCaseMM <- matrix(NA, ncol = nIDs, nrow = 5) # Secondary Cases metamatrix
PrevMM <- matrix(NA, ncol = nIDs, nrow = 5) # prevalence metamatrix

for(k in 1:nrow(scenariosDF)){
  name = scenariosDF$name[k]
  years = scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  
  files = list.files(paste0(folderSim,"/"), paste0("Sim_Drias_", name))
  
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
    
    Adults <- Sim[,3*nIDs + 1:nIDs]
    
    SecCase <- colMeans(Adults[FMay:LOct,], na.rm =T)
    SecCaseM[i, ] <- SecCase
    
    
    PrevM[i,] = colSums(R0dengueM>1, na.rm =T)
  }
  
  rm(IDsDT)
  
  SecCaseMM[k,] <- colMeans(SecCaseM, na.rm =T)
  PrevMM[k,] <- colMeans(PrevM, na.rm =T)
}