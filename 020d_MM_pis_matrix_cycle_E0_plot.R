#Plot E0 ----

# Inspired by 
# ModelMetelmann_pis_matrix_EOBS_cycle_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

mod = "" # "" = CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63, cold = MPI-M-MPI-ESM-LR_MPI-CSC-REMO2009, hot = MOHC-HadGEM2-ES_CLMcom-CCLM4-8-17

folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_020")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod, "_sim_020")

nIDs = 8981 # number of regions
IDs = 1:nIDs
IDsSubSet = IDs

files = list.files(paste0(folderSim,"/"), pattern = "E0_")

namesAll = substring(files, nchar(files)-12, nchar(files)-9)
yearsAll = substring(files, nchar(files)-7, nchar(files)-4) 

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

#simplified palette
colPal= c("#384AB4", "#8EB0FE", "#F29878",  "#B00026")
cutPal = c(0, 10^(-1:1), 10^10)

# scenarios

scenariosDF= data.frame(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(1986, 2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(1986, 2026, 2046, 2066, 2026, 2046, 2066)+19)


E0MM <-  matrix(NA, ncol = nIDs, nrow = nrow(scenariosDF))
  
for(k in 1:nrow(scenariosDF)){
  
  name = scenariosDF$name[k]
  years = scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  nR <- which((namesAll == name) & (yearsAll %in% years))
  
  E0mSel = E0m[nR,]
  
  #replace 0 and nas with the average
  id0nas = rbind(which(E0mSel==0, arr.ind = TRUE),which(is.na(E0mSel), arr.ind = TRUE))
  
  E0mSelcorr = E0mSel
  
  for(i in 1:nrow(id0nas)){
    coli <- E0mSel[,id0nas[i,2]]
    colicr <- coli[which(coli>0 & !is.na(coli))]
    E0mSelcorr[id0nas[i,1], id0nas[i,2]]= exp(mean(log(colicr)))
  }
  
  E0Sel <-  apply(E0mSelcorr, 2,
                  function(x){exp(mean(log(x), na.rm = T))})

  
  E0MM[k,] =  E0Sel
  
  E0SelCut <- cut(E0Sel, breaks=cutPal,
                  labels=sapply(cutPal [-length(cutPal )], function(x){paste0(">", as.character(x))}))
  
  plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = E0SelCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("E0, ", name, ", ", min(years), "-", max(years)))+
    theme(plot.background  = element_blank(),
          aspect.ratio = 1)
  
  # if(!(name %in% c("Cn70", "Hg70"))){
  #   plotCut <- plotCut +
  #     theme(legend.position = "none")
  # }
  
  plotCut <- plotCut +
    theme(legend.position = "none",
          panel.grid = element_blank(), 
          line = element_blank(), 
          rect = element_blank(), 
          text = element_blank(), 
          plot.background = element_rect(fill = "transparent", color = "transparent"))
  
  ggsave(file = 
           paste0(folderPlot, "/E0_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", E0>1: ", round(100*sum(E0Sel>1, na.rm = T)/8981, 0), ", nas:", sum(is.nan(E0Sel)),"\n")
  
}

saveRDS(E0MM, file = paste0(folderSim, "/E0MM.rds"))
