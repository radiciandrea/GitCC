# repeat codes 02 and 04, but with fixed climate to historic

library(dplyr)

rm(list = ls())

IDsSubSet = 1:8981

## climatic model

mod = "" # "" = CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63, cold = MPI-M-MPI-ESM-LR_MPI-CSC-REMO2009, hot = MOHC-HadGEM2-ES_CLMcom-CCLM4-8-17

if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folderOut = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS",mod,"_sim_060a")
  folderDrias = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS",mod,"_elab")
  folderX0 = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS",mod,"_sim")
} else {
  folderOut = paste0("DRIAS",mod,"_sim_060a")
  folderDrias = paste0("DRIAS",mod,"_elab")
  folderX0 = paste0("DRIAS",mod,"_sim")
}
# pop scenarios

scenariosDF= data.frame(popScenario = c("Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(2026, 2046, 2066, 2026, 2046, 2066)+19)


for(k in 1:nrow(scenariosDF)){
  
  popScenario = scenariosDF$popScenario[k]
  
  IDsDT <- readRDS(paste0(folderDrias, "/Drias_", popScenario,"_20", substr(popScenario, 3,4),".rds")) %>%
    distinct(ID, .keep_all = TRUE) %>%
    dplyr::select(c("ID", "lat", "lon", "pop", "surfHa")) %>%
    filter(ID %in% IDsSubSet)
  
  name = paste0("Hs99_pop", popScenario)
  years = 1986:2005 #scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  source("0620_MM_pis_matrix_cycle_E0.R")
  source("0630a_MM_pis_matrix_cycle_Abundances.R")
  rm(IDsDT)
  
  
}





