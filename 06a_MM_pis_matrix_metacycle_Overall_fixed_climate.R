# repeat codes 02 and 04, but with fixed climate to historic

library(dplyr)

rm(list = ls())

IDsSubSet = 1

if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_06a"
  folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
  folderX0 = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim"
} else {
  folderOut = "DRIAS_sim_06a"
  folderDrias = "DRIAS_elab"
  folderX0 = "DRIAS_sim"
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
  
  source("02_MM_pis_matrix_cycle_E0.R")
  source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")
  rm(IDsDT)
  
  
}





