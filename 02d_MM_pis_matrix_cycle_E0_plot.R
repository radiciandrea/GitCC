#Plot E0 ----

# Inspired by 
# ModelMetelmann_pis_matrix_EOBS_cycle_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

folderData = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_02"
folderPlot = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS_sim_02"

files = list.files(paste0(folderData,"/"), pattern = "E0")

namesAll = substring(files, nchar(files)-12, nchar(files)-9)
yearsAll = substring(files, nchar(files)-7, nchar(files)-4) 

# load for 1 dimension
E01 <- readRDS(paste0(folderData, "/", files[1]))

E0m = matrix(NA, ncol = length(E01), nrow = length(files))

for (i in 1:length(files)){
  file = files[i]
  E0v <- readRDS(paste0(folderData, "/", file))
  E0m[i,]= E0v
}

# Load map
domain <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

# Metelmann palette & cuts
colPal= c("#384AB4", "#5570DF", "#8EB0FE", "#C5D7F3", "#F2CDBB", "#F29878", "#D04B45", "#B00026")
cutPal = c(0, 10^(-3:3), 10^10)
dev.new(height=3)
# scenarios

scenariosDF= data.frame(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                        yearStart = c(1986, 2026, 2046, 2066, 2026, 2046, 2066),
                        yearEnd = c(1986, 2026, 2046, 2066, 2026, 2046, 2066)+19)


for(k in 1:nrow(scenariosDF)){
  
  name = scenariosDF$name[k]
  years = scenariosDF$yearStart[k]:scenariosDF$yearEnd[k]
  
  nR <- which((namesAll == name) & (yearsAll %in% years))
  
  E0Sel <-  apply(E0m[nR,], 2,
                  function(x){exp(mean(log(x)))})
  
  E0SelCut <- cut(E0Sel, breaks=cutPal,
                  labels=sapply(cutPal [-length(cutPal )], function(x){paste0(">", as.character(x))}))
 
   plotCut <- ggplot()+
    geom_sf(data = domain, aes(fill = E0SelCut), colour = NA)+ #
    scale_fill_manual(values = colPal)+
    ggtitle(paste0("E0, ", name, ", ", min(years), "-", max(years)))+
    theme(plot.background  = element_blank(),
          aspect.ratio = 1)
  
  if(!(name %in% c("Cn70", "Hg70"))){
    plotCut <- plotCut +
      theme(legend.position = "none")
  }
  
  ggsave(file = 
           paste0(folderPlot, "/E0_", name, "_", min(years), "-", max(years), ".png"),
         plot= plotCut, units="in", height=3.2, width = 4.2, dpi=300) #units="in", height=4,
  
  cat("name:", name, ", E0>1: ", round(100*sum(E0Sel>1, na.rm = T)/8981, 0), "\n")

  }
