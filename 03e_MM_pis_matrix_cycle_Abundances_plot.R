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

# Common initial settings

# load 1 for dimension
files = list.files(paste0(folderData,"/"), pattern = "Sim_Drias_Hist")
Sim <- readRDS(paste0(folderData, "/", files[1]))
nC = 5 # number of classes
nIDs = (ncol(Sim)-1)/nC # number of regions
IDs = 1:nIDs
IDsSubSet = IDs

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

files = list.files(paste0(folderData,"/"), pattern = "Sim_Drias_Hist")

# matrices of indicators: average adults and R0

AmjjasoM = matrix(NA, nrow = length(years), ncol = nIDs)
LTSdengueM = matrix(NA, nrow = length(years), ncol = nIDs)

for(i in  1:length(years)){
  
  file = files[i]
  
  Sim <- readRDS(paste0(folderData, "/", file))
  year <- years[i] # substr(file, nchar(file)-7, nchar(file)-4)
  
  #determine mjjaso
  nD <- nrow(Sim)
  FMay <- yday(as.Date(paste0(year, "-05-01"))) 
  LOct <- yday(as.Date(paste0(year, "-10-31"))) 
  
  Adults <- Sim[,3*nIDs + 1:nIDs]
  
  Amjjaso <- colMeans(Adults[FMay:LOct,])
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
  LTSdengueM[i,] = colSums(R0dengueM>1)
}

rm(IDsDT)