# plot adult and R0 trajectories

# per scenarios:

# Hist 1986-2005
# central + rcp 4.5 2026-2065
# central + rcp 4.5 2046-2065
# central + rcp  4.5 2066-2085
# high + rcp  8.5 2026-2065
# high + rcp  8.5 2046-2065
# high + rcp 8.5 2066-2085

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)
library(stats)

mod = ""

# folders

folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_030")
folderSimNoDiap = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_030f")
folderData = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod, "_sim_0340")

# scenarios and cities

scenarios <- c("Cn70", "Hs99","Hg70")
cities <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice", "Strasbourg", "Clermont-Ferrand")
IDsSubSet <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249, 7379, 3564)


## 1: diapausing and homodynamic Adults----
# plot diapausing and homodynamic trajectories for Montpellier


# high + rcp 8.5 2066-2085
nameSc = "Hg70"
IDx = 3500 #Lyon
IDx = 1040 #Montpellier

colPalette <- c("#D04B45", "#5200A3")

for(IDx in IDsSubSet){
  
  cityx = cities[which(IDsSubSet == IDx)]
  
  trajAdDF = data.frame(day = rep(1:365, 2*length(scenarios)),
                        type =rep(c("D", "H"), each = 365*length(scenarios)),
                        nameSc = rep(rep(scenarios, each = 365), times = 2),
                        meanDensity = NA,
                        medianDensity = NA,
                        IQ = NA,
                        IIIQ = NA)
  
  for(nameSc in scenarios){
    
    
    fileDAdults= list.files(paste0(folderSim,"/"), pattern = paste0("030a_Adults_Drias_", nameSc))
    fileHAdults= list.files(paste0(folderSimNoDiap,"/"), pattern = paste0("030f_Adults_Drias_", nameSc))
    
    DAdultsM = matrix(NA, nrow = 365, ncol = length(fileDAdults))
    HAdultsM = matrix(NA, nrow = 365, ncol = length(fileDAdults))
    
    for(i in 1:length(fileDAdults)){
      DAdultsM[,i] = readRDS(paste0(folderSim, "/", fileDAdults[i]))[1:365, IDx]
    }
    
    for(i in 1:length(fileHAdults)){
      HAdultsM[,i] = readRDS(paste0(folderSimNoDiap, "/", fileHAdults[i]))[1:365, IDx]
    }
    
    #smoothing D
    DAdultsMAM <- stats::filter(DAdultsM, rep(1 / 3, 3), sides = 2)
    DAdultsMAM[is.na(DAdultsMAM)]=0
    
    trajIQ = sapply(1:365, function(t){quantile(DAdultsMAM[t,], 0.25, na.rm = T)})
    trajMean = rowMeans(DAdultsMAM, na.rm = T)
    trajMedian = sapply(1:365, function(t){quantile(DAdultsMAM[t,], 0.5, na.rm = T)})
    trajIIIQ = sapply(1:365, function(t){quantile(DAdultsMAM[t,], 0.75, na.rm = T)})
    
    trajAdDF$meanDensity[which(trajAdDF$type == "D" & trajAdDF$nameSc == nameSc)]= trajMean
    trajAdDF$medianDensity[which(trajAdDF$type == "D" & trajAdDF$nameSc == nameSc)]= trajMedian
    trajAdDF$IQ[which(trajAdDF$type == "D" & trajAdDF$nameSc == nameSc)]= trajIQ
    trajAdDF$IIIQ[which(trajAdDF$type == "D" & trajAdDF$nameSc == nameSc)]= trajIIIQ
    
    #smoothing H
    HAdultsMAM <- stats::filter(HAdultsM, rep(1 / 3, 3), sides = 2)
    HAdultsMAM[is.na(HAdultsMAM)]=0
    
    trajIQ = sapply(1:365, function(t){quantile(HAdultsMAM[t,], 0.25, na.rm = T)})
    trajMean = rowMeans(HAdultsMAM, na.rm = T)
    trajMedian = sapply(1:365, function(t){quantile(HAdultsMAM[t,], 0.5, na.rm = T)})
    trajIIIQ = sapply(1:365, function(t){quantile(HAdultsMAM[t,], 0.75, na.rm = T)})
    
    # if(nameSc == "Hg70"){
      trajAdDF$meanDensity[which(trajAdDF$type == "H" & trajAdDF$nameSc == nameSc)]= trajMean
      trajAdDF$medianDensity[which(trajAdDF$type == "H" & trajAdDF$nameSc == nameSc)]= trajMedian
      trajAdDF$IQ[which(trajAdDF$type == "H" & trajAdDF$nameSc == nameSc)]= trajIQ
      trajAdDF$IIIQ[which(trajAdDF$type == "H" & trajAdDF$nameSc == nameSc)]= trajIIIQ
    # }
  }
  
  trajAdDF <- trajAdDF %>%
    filter(!is.na(medianDensity))
  
  # plot
  
  plotCut <-ggplot(data = trajAdDF)+
    geom_line(aes(x = day, y = log(medianDensity+1), color = type, linetype = nameSc), linewidth = 0.8)+
    geom_ribbon(aes(x = day, ymin=log(IQ+1), ymax=log(IIIQ+1), fill = type, linetype = nameSc), alpha = 0.2)+
    scale_fill_discrete(palette = colPalette)+
    scale_color_discrete(palette = colPalette)+
    geom_hline(aes(yintercept = log(2)),linetype = 2)+
    ylim(c(0,9.5))+
    theme_test()
  
  # plotCut <- plotCut +
  #   theme(legend.position = "none",
  #         panel.grid = element_blank(), 
  #         line = element_blank(), 
  #         rect = element_blank(), 
  #         text = element_blank(), 
  #         plot.background = element_rect(fill = "transparent", color = "transparent"))
  
  plotCut <- plotCut +
    theme(legend.position = "none",
          panel.grid = element_blank(), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          plot.background = element_rect(fill = "transparent", color = "transparent"))
  
  ggsave(file = 
           paste0(folderPlot, "/PoPtraj_", cityx, ".png"),
         plot= plotCut, units="cm", height=5.5, width = 8.5, dpi=300) #units="in", height=4,
  
}

## 2: R0 at different scenarios----
##  load for another city (Lyon?) also data to plot R0

# epidemic parameters
#host preference
phiAU = 0.9 #human biting preference (urban)
phiAR = 0.5 #human biting preference (rural) #Caminade 2016
RTh = 50 #defines density over km2 below which an area is "rural", with little phi

IIPdengue = 5 #Benkimoun 2021

bV2H = 0.5 # b Blagrove 2020
bH2Vdengue = 0.31 # beta Metelmann 2021

IDx = 3500 #Lyon
IDx = 1040 #Montpellier

scenarios <- c("Cn70", "Hs99","Hg70")
cities <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice", "Strasbourg", "Clermont-Ferrand")
IDsSubSet <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249, 7379, 3564)

for(IDx in IDsSubSet){
  
  cityx = cities[which(IDsSubSet == IDx)]
  
  trajR0DF = data.frame(day = rep(1:365, times = length(scenarios)),
                        nameSc = rep(scenarios, each = 365),
                        meanR0 = NA,
                        medianR0 = NA,
                        IQ = NA,
                        IIIQ = NA)
  
  tic()
  for(j in 1:length(scenarios)){
    
    nameSc = scenarios[j]
    ##
    filesW= list.files(paste0(folderData,"/"), pattern = paste0("Drias_", nameSc))
    fileAdults= list.files(paste0(folderSim,"/"), pattern = paste0("030a_Adults_Drias_", nameSc))
    
    R0dengueM = matrix(NA, nrow = 365, ncol = length(fileAdults))
    
    for(i in 1:length(fileAdults)){
      Adults = readRDS(paste0(folderSim, "/", fileAdults[i]))[,IDx]
      tas = readRDS(paste0(folderData, "/", filesW[i])) %>% filter(ID == IDx) %>% pull(tas)
      H = (readRDS(paste0(folderData, "/", filesW[i])) %>% filter(ID == IDx) %>% pull(pop))[1]
      
      #vector to host ratio
      m = Adults*100/H
      
      #demographic parameters
      muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
      muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
      
      #epidemiological parameters
      A = (0.0043*tas + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
      phiA = phiAU*(H>RTh)+phiAR*(H<=RTh)
      
      EIPdengue = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021
      
      # LTS
      R0dengueM[,i] = ((A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue)[1:365]
      
    }
    
    #smoothing
    R0dengueMAM <- stats::filter(R0dengueM, rep(1 / 1, 1), sides = 2)
    R0dengueMAM[is.na(R0dengueMAM)]=0
    
    trajIQ = sapply(1:365, function(t){quantile(R0dengueMAM[t,], 0.25, na.rm = T)})
    trajMean = rowMeans(R0dengueMAM, na.rm = T)
    trajMedian = sapply(1:365, function(t){quantile(R0dengueMAM[t,], 0.5, na.rm = T)})
    trajIIIQ = sapply(1:365, function(t){quantile(R0dengueMAM[t,], 0.75, na.rm = T)})
    
    cat(cityx, " - ", nameSc, ": ", sum(trajMean>1), "\n")
    
    trajR0DF$meanR0[which(trajR0DF$nameSc == nameSc)]= trajMean
    trajR0DF$medianR0[which(trajR0DF$nameSc == nameSc)]= trajMedian
    trajR0DF$IQ[which(trajR0DF$nameSc == nameSc)]= trajIQ
    trajR0DF$IIIQ[which(trajR0DF$nameSc == nameSc)]= trajIIIQ
    
    toc()
  }
  
  colPalette <- c( "#21908C", "#FCE724", "#3A528A")
  
  plotCut <- ggplot(data = trajR0DF)+
    geom_line(aes(x = day, y = medianR0, color = nameSc), linewidth = 0.8)+
    geom_ribbon(aes(x = day, ymin=IQ, ymax=IIIQ, fill = nameSc), alpha = 0.2)+
    scale_fill_discrete(palette = colPalette)+
    scale_color_discrete(palette = colPalette)+
    geom_hline(aes(yintercept = 1),linetype = 2)+
    ylim(c(0,7))+
    theme_test()
  
  # plotCut <- plotCut +
  #   theme(legend.position = "none",
  #         panel.grid = element_blank(), 
  #         line = element_blank(), 
  #         rect = element_blank(), 
  #         text = element_blank(), 
  #         plot.background = element_rect(fill = "transparent", color = "transparent"))
  
  plotCut <- plotCut +
    theme(legend.position = "none",
          panel.grid = element_blank(), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          plot.background = element_rect(fill = "transparent", color = "transparent"))
  
  ggsave(file = 
           paste0(folderPlot, "/R0traj_", cityx, ".png"),
         plot= plotCut, units="cm", height=5.5, width = 8.5, dpi=300) #units="in", height=4,
  
}

