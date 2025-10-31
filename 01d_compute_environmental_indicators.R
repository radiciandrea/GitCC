# code to compute temperature increases

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
folderShp <- "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"

## initial settings----

IDsSubSet = 1:8981
nIDs = length(IDsSubSet)

scenariosDF= data.frame(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(1986, 2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(1986, 2026, 2046, 2066, 2026, 2046, 2066)+19,
                        tas = NA,
                        pr = NA,
                        pop = NA)

## cycle----

for(k in 1:nrow(scenariosDF)){
  
  name = scenariosDF$name[k]
  years = scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  tas = 0
  pr = 0
  pop = 0
  
  for(i in  1:length(years)){
    
    year <- years[i]
    
    WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
      filter(ID %in% IDsSubSet) 
    
    nD = nrow(WTotDT)/nIDs
    
    tas = ((i-1)*tas + mean(WTotDT$tas))/i
    pr = ((i-1)*pr + sum(WTotDT$pr)/nIDs)/i
    pop = ((i-1)*pop + 10^2 * sum(WTotDT$pop*WTotDT$surfHa, na.rm = T)/(nD))/i
    
  }
  
  scenariosDF$tas[k] = tas
  scenariosDF$pr[k] = pr
  scenariosDF$pop[k] = pop
}


SafranDensSF <- st_read(paste0(folderShp, "/SafranDensOmphale.shp"))
sum(SafranDensSF$PopKmHs99*SafranDensSF$surf_ha, na.rm = T)*100
sum(SafranDensSF$PopKmCn35*SafranDensSF$surf_ha, na.rm = T)*100
sum(SafranDensSF$PopKmCn55*SafranDensSF$surf_ha, na.rm = T)*100
sum(SafranDensSF$PopKmCn70*SafranDensSF$surf_ha, na.rm = T)*100
sum(SafranDensSF$PopKmHg35*SafranDensSF$surf_ha, na.rm = T)*100
sum(SafranDensSF$PopKmHg55*SafranDensSF$surf_ha, na.rm = T)*100
sum(SafranDensSF$PopKmHg70*SafranDensSF$surf_ha, na.rm = T)*100
