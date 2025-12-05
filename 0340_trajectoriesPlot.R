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

# plot diapausing and homodynamic trajectories for Montpellier

# high + rcp 8.5 2066-2085

nameSc = "Hg70"
mod = ""

folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_030")
folderData = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod, "_sim_0340")

fileAdults= list.files(paste0(folderSim,"/"), pattern = paste0("030a_Adults_Drias_", nameSc))

ID = 1040 #Montpellier

diapausingAdultsM = matrix(NA, nrow = 365, ncol = length(fileAdults))

for(i in 1:length(fileAdults)){
  diapausingAdultsM[,i] = readRDS(paste0(folderSim, "/", fileAdults[i]))[1:365, ID]
}

#smoothing
diapausingAdultsMAM <- stats::filter(diapausingAdultsM, rep(1 / 3, 3), sides = 2)
diapausingAdultsMAM[is.na(diapausingAdultsMAM)]=0

trajIQ = sapply(1:365, function(t){quantile(diapausingAdultsMAM[t,], 0.25)})
trajMean = rowMeans(diapausingAdultsMAM)
trajIIIQ = sapply(1:365, function(t){quantile(diapausingAdultsMAM[t,], 0.75)})

trajAdDF = data.frame(day = 1:365,
                    type ="diapausing",
                    medianDensity = trajMean,
                    IQ = trajIQ,
                    IIIQ = trajIIIQ)

# plot

ggplot(data = trajAdDF)+
  geom_line(aes(x = day, y = medianDensity, color = type))+
  geom_ribbon(aes(x = day, ymin=IQ, ymax=IIIQ, fill = type), alpha = 0.2)


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
  
  
  trajR0DF = data.frame(day = rep(1:365, times = length(Scenarios)),
                        nameSc = rep(Scenarios, each = 365),
                        medianDensity = NA,
                        IQ = NA,
                        IIIQ = NA)
  
  tic()
  for(j in 1:length(Scenarios)){
    
    nameSc = Scenarios[j]
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
      
      ## LTS----
      R0dengueM[,i] = ((A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue)[1:365]
      
    }
    
    cat(nameSc, ": ", sum(R0dengueM>1)/length(fileAdults))
    
    #smoothing
    R0dengueMAM <- R0dengueM # stats::filter(R0dengueM, rep(1 / 1, 1), sides = 2)
    R0dengueMAM[is.na(R0dengueMAM)]=0
    
    trajIQ = sapply(1:365, function(t){quantile(R0dengueMAM[t,], 0.25)})
    trajMean = rowMeans(R0dengueMAM)
    trajIIIQ = sapply(1:365, function(t){quantile(R0dengueMAM[t,], 0.75)})
    
    trajR0DF$meanR0[which(trajR0DF$nameSc == nameSc)]= trajMean
    trajR0DF$IQ[which(trajR0DF$nameSc == nameSc)]= trajIQ
    trajR0DF$IIIQ[which(trajR0DF$nameSc == nameSc)]= trajIIIQ
    
    toc()
  }
  
  colPalette <- c( "#21908C", "#FCE724", "#3A528A")
  
  plotCut <- ggplot(data = trajR0DF)+
    geom_line(aes(x = day, y = meanR0, color = nameSc), linewidth = 0.8)+
    geom_ribbon(aes(x = day, ymin=IQ, ymax=IIIQ, fill = nameSc), alpha = 0.2)+
    scale_fill_discrete(palette = colPalette)+
    scale_color_discrete(palette = colPalette)+
    geom_hline(aes(yintercept = 1),linetype = 2)+
    ylim(c(0,4))
  
  plotCut <- plotCut +
    theme(legend.position = "none",
          panel.grid = element_blank(), 
          line = element_blank(), 
          rect = element_blank(), 
          text = element_blank(), 
          plot.background = element_rect(fill = "transparent", color = "transparent"))
  
  plotCut
  
  ggsave(file = 
           paste0(folderPlot, "/ROtraj", IDx, ".png"),
         plot= plotCut, units="cm", height=5.2, width = 8.2, dpi=300) #units="in", height=4,
  
}

