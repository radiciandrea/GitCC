# code to plot map (T winter vs T avg) under different climate change conditiosn

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)
library(metR)
library(ggrepel)
library(ggpubr)

folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"

scenarios <- c("Cn70", "Cn55", "Cn35", "Hs99", "Hg35", "Hg55", "Hg70")
cites <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice")
IDsSubSet <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249)

MapDT <- data.table(city = rep(cites, length(scenarios)),
                    ID = rep(IDsSubSet, length(scenarios)),
                    scenario = rep(scenarios,length(cites)),
                    tasAvgYea = 0, #average annual T
                    tasMinWin = 0) #average minimal in J

for(k in 1:length(scenarios)){
  
  scenariox = scenarios[k]
  files = list.files(paste0(folderDrias,"/"), paste0("Drias_", scenariox))
  
  for(i in  1:length(files)){
    
    file = files[i]
    year <- substr(file, nchar(file)-7, nchar(file)-4)
    
    WTotDT <- readRDS(paste0(folderDrias, "/Drias_", scenariox, "_", year, ".rds")) %>%
      filter(ID %in% IDsSubSet)
    
    for(j in 1:length(IDsSubSet)){
      
      IDx = IDsSubSet[j]
      WTotDTtemp = WTotDT %>% filter(ID == IDx)
      
      MapDT[scenario == scenariox & ID == IDx, tasAvgYea := (tasAvgYea*(j-1)+mean(WTotDTtemp[,tas]))/j]
      MapDT[scenario == scenariox & ID == IDx, tasMinWin := (tasMinWin*(j-1)+mean(WTotDTtemp[DOS <= 31, tasMin]))/j]
      cat(mean(WTotDTtemp$tas), "\n")
    }
    
  }
}

# fake background df

bakcDF = data.table(tasAvgYea = rep(seq(11, 20, by = 0.5), each = 21),
                    tasMinWin = rep(seq(1, 10, by = 0.5), times = 21),
                    suitability = NA)

bakcDF$suitability =  (bakcDF$tasMinWin - 3)^2 + (bakcDF$tasAvgYea)^1.7

ggplot() +
  geom_contour_fill(
    data = bakcDF,
    aes(x = tasAvgYea, y = tasMinWin, z = log(suitability))
  ) +
  ggtitle("Suitability")+
  geom_contour(
    data = bakcDF,
    aes(x = tasAvgYea, y = tasMinWin, z = suitability),
    color = "black", breaks = c(1))+
  theme_test()+
  geom_path(data = MapDT, aes(x = tasAvgYea, y = tasMinWin), color= "white") +
  geom_point(data = MapDT, aes(x = tasAvgYea, y = tasMinWin, shape = "city"), color = "white", size = 2) +
  guides(size = "legend", colour = "none")+
  scale_color_grey()+
  geom_label_repel(data = MapDT ,
                   aes(x = tasAvgYea, y = tasMinWin, label = scenario),
                   label.padding = 0.15, segment.color = NA, size = 3) #size = 4

# plot(MapDT$tasAvgYea, MapDT$tasMinWin)
# 
# 
# +
#   scale_fill_gradientn(
#     colors = col_br_post, # Use the defined colors
#     values = scales::rescale(log10(br_post)), # Rescale breaks for the log10 scale
#     na.value = "#32003C" # Define the color for NA values
#   )+