# code to compute temperature increases

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

mod = "" # "" = CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63, cold = MPI-M-MPI-ESM-LR_MPI-CSC-REMO2009, hot = MOHC-HadGEM2-ES_CLMcom-CCLM4-8-17

folderDrias = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
folderShp <- "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod)

## initial settings----

IDsSubSet = 1:8981
nIDs = length(IDsSubSet)

scenariosDF= data.frame(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(1986, 2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(1986, 2026, 2046, 2066, 2026, 2046, 2066)+19,
                        tas = NA,
                        pr = NA,
                        pop = NA)

tasMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
prMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
popMM <- matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))


## cycle----

for(k in 1:nrow(scenariosDF)){
  
  name = scenariosDF$name[k]
  years = scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  tas = 0
  pr = 0
  pop = 0
  
  tasM = matrix(NA, nrow = length(years), ncol = nIDs)
  prM = matrix(NA, nrow = length(years), ncol = nIDs)
  
  for(i in  1:length(years)){
    
    year <- years[i]
    
    WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
      filter(ID %in% IDsSubSet) 
    
    tasM[i,] = WTotDT %>% group_by(ID) %>% summarise(tasY = mean(tas, na.rm = T)) %>% ungroup() %>% pull(tasY)
    prM[i,] = WTotDT %>% group_by(ID) %>% summarise(prY = sum(pr)) %>% ungroup() %>% pull(prY)
    
    nD = nrow(WTotDT)/nIDs
    
    tas = ((i-1)*tas + mean(WTotDT$tas))/i
    pr = ((i-1)*pr + sum(WTotDT$pr)/nIDs)/i
  }

  
  tasMM[k,] <- colMeans(tasM, na.rm =T)
  prMM[k, ] <- colMeans(prM, na.rm =T)
  popMM[k, ] <- WTotDT %>% filter(DOS == 1) %>% pull(pop)
  
  scenariosDF$tas[k] = tas
  scenariosDF$pr[k] = pr
}

SafranDensSF <- st_read(paste0(folderShp, "/SafranDensOmphale.shp"))
scenariosDF$pop[1] <- sum(SafranDensSF$PopKmHs99*SafranDensSF$surf_ha, na.rm = T)*10^-8
scenariosDF$pop[2] <- sum(SafranDensSF$PopKmCn35*SafranDensSF$surf_ha, na.rm = T)*10^-8
scenariosDF$pop[3] <- sum(SafranDensSF$PopKmCn55*SafranDensSF$surf_ha, na.rm = T)*10^-8
scenariosDF$pop[4] <- sum(SafranDensSF$PopKmCn70*SafranDensSF$surf_ha, na.rm = T)*10^-8
scenariosDF$pop[5] <- sum(SafranDensSF$PopKmHg35*SafranDensSF$surf_ha, na.rm = T)*10^-8
scenariosDF$pop[6] <- sum(SafranDensSF$PopKmHg55*SafranDensSF$surf_ha, na.rm = T)*10^-8
scenariosDF$pop[7] <- sum(SafranDensSF$PopKmHg70*SafranDensSF$surf_ha, na.rm = T)*10^-8

### save and load

saveRDS(tasMM, file = paste0(folderDrias, "/tasMM.rds"))
saveRDS(prMM, file = paste0(folderDrias, "/prMM.rds"))
saveRDS(popMM, file = paste0(folderDrias, "/popMM.rds"))
saveRDS(scenariosDF, file = paste0(folderDrias, "scenariosDF.rds"))

tasMM<- readRDS(file = paste0(folderDrias, "/tasMM.rds"))
prMM <- readRDS(file = paste0(folderDrias, "/prMM.rds"))
popMM <- readRDS(file = paste0(folderDrias, "/popMM.rds"))
scenariosDF <- readRDS(file = paste0(folderDrias, "/scenariosDF.rds"))

### # Load map
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

# tas

# Metelmann palette & cuts
colPal= c("#384AB4", "#5570DF", "#8EB0FE", "#C5D7F3", "#F2CDBB", "#F29878", "#D04B45", "#B00026")
cutPal = c(-3,2,6,9,11,13,16,20,25)

for(i in 1:nrow(scenariosDF)){
  
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  tasSelCut <- cut(tasMM[i,], breaks=cutPal,
                   labels=sapply(cutPal [-length(cutPal )], function(x){paste0(">", as.character(x))}))
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = tasSelCut), colour = NA)+ #
    scale_fill_manual(values = colPal, drop = FALSE)+
    ggtitle(paste0("tas, scenario: ", name, "; period: ", min(years), "-", max(years)))+
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
           paste0(folderPlot, "/tas_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", tas: ", round(mean(tasMM[i,]),1), "\n")
  
}

# rain

# Metelmann palette & cuts
colPal = rev(c("#01392d", "#339990", "#c9eae6", "#e0c37e", "#c08328", "#532d00"))
cutPal = round(860^c(0.88, 0.92, 0.96, 1, 1.06, 1.12, 1.18), -1)

for(i in 1:nrow(scenariosDF)){
  
  name = scenariosDF$name[i]
  years = scenariosDF$yearStart[i]:scenariosDF$yearEnd[i]
  
  prSelCut <- cut(prMM[i,], breaks=cutPal,
                   labels=sapply(cutPal [-length(cutPal )], function(x){paste0(">", as.character(x))}))
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = prSelCut), colour = NA)+ #
    scale_fill_manual(values = colPal, drop = FALSE)+
    ggtitle(paste0("pr, scenario: ", name, "; period: ", min(years), "-", max(years)))+
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
           paste0(folderPlot, "/pr_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", pr: ", round(mean(prMM[i,]),0), "\n")
  
}

