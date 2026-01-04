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

mod = "hot"

# folders

folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_030")
folderSimNoDiap = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_030f")
folderOut = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_050")
folderData = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod, "_sim_0530")

# scenarios and cities

scenarios <- c("Cn70", "Hs99","Hg70")
cities <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice", "Strasbourg", "Clermont-Ferrand")
IDs <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249, 7379, 3564)

cities <- c(cities, "46190 Sousceyrac-en-Quercy (Comiac)")
IDs <- c(IDs,2580)

cityDF = data.frame(cities = cities,
                    IDs = IDs)

cityDF <- cityDF %>%
  arrange(IDs)

IDsSubSet = cityDF$IDs
cities = cityDF$cities

nIDs = nrow(cityDF)
nSs = length(scenarios)
nD = 365

# epidemic parameters
#host preference
phiAU = 0.9 #human biting preference (urban)
phiAR = 0.5 #human biting preference (rural) #Caminade 2016
RTh = 50 #defines density over km2 below which an area is "rural", with little phi

IIPdengue = 5 #Benkimoun 2021

bV2H = 0.5 # b Blagrove 2020
bH2Vdengue = 0.31 # beta Metelmann 2021

colPalette1 <- c("#D04B45", "#5200A3")
colPalette2 <- c( "#21908C", "#FCE724", "#3A528A")

## 3: cycle ----

R0List <- vector(mode = "list", nSs*nIDs)
HAdList <- vector(mode = "list", nSs*nIDs)
DAdList <- vector(mode = "list", nSs*nIDs)

tic()
for(nameSc in scenarios){
  
  fileDAdults= list.files(paste0(folderSim,"/"), pattern = paste0("030a_Adults_Drias_", nameSc))
  fileHAdults= list.files(paste0(folderSimNoDiap,"/"), pattern = paste0("030f_Adults_Drias_", nameSc))
  filesW= list.files(paste0(folderData,"/"), pattern = paste0("Drias_", nameSc))
  
  nRep = length(fileDAdults)
  
  DAdultsM = array(NA, c(nD, nIDs, nRep))
  HAdultsM = array(NA, c(nD, nIDs, nRep))
  tasM = array(NA, c(nD, nIDs, nRep))
  HM = array(NA, c(nD, nIDs, nRep))
  R0dengueAdultsM = array(NA, c(nD, nIDs, nRep))
  
  for(i in 1:nRep){
    DAdultsM[, , i] = readRDS(paste0(folderSim, "/", fileDAdults[i]))[1:nD, IDsSubSet]
    HAdultsM[, , i] = readRDS(paste0(folderSimNoDiap, "/", fileHAdults[i]))[1:nD, IDsSubSet]
    
    tasM[, , i] = matrix(readRDS(paste0(folderData, "/", filesW[i]))
                         %>% filter(ID %in% IDsSubSet)
                         %>% filter(DOS < (nD+1))
                         %>% pull(tas)
                         , nrow = nD)
    HM[, , i] = matrix(rep(readRDS(paste0(folderData, "/", filesW[i])) 
                           %>% filter(ID %in% IDsSubSet) 
                           %>% filter(DOS ==1)
                           %>% pull(pop)), ncol = nIDs, nrow = nD, byrow = T )
  }
  
  #vector to host ratio
  m = DAdultsM*100/HM
  
  #demographic parameters
  muA = -log(0.677 * exp(-0.5*((tasM-20.9)/13.2)^6)*tasM^0.1) # adult mortality rate
  muA[which(tasM<=0)] = -log(0.677 * exp(-0.5*((tasM[which(tasM<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  #epidemiological parameters
  A = (0.0043*tasM + 0.0943)/2  #biting rate (Zanardini et al., Caminade 2016, Blagrove 2020)
  phiA = phiAU*(HM>RTh)+phiAR*(HM<=RTh)
  
  EIPdengue = 1.03*(4+exp(5.15 - 0.123*tasM)) #Metelmann 2021
  
  # LTS
  R0dengueM = ((A*phiA)^2*m/(muA+muA^2*EIPdengue)*bV2H*bH2Vdengue*IIPdengue)
  
  #### TRAJ ----
  
  #smoothing NO
  
  R0dengueMAM <- R0dengueM #stats::filter(R0dengueM, rep(1 / 1, 1), sides = 2)
  R0dengueMAM[is.na(R0dengueMAM)]=0
  
  DAdultsMAM <- DAdultsM #stats::filter(R0dengueM, rep(1 / 1, 1), sides = 2)
  DAdultsMAM[is.na(DAdultsMAM)]=0
  
  HAdultsMAM <- HAdultsM #stats::filter(R0dengueM, rep(1 / 1, 1), sides = 2)
  HAdultsMAM[is.na(HAdultsMAM)]=0
  
  for(j in 1:nIDs){
    
    trajR0DF = data.table(city = cities[j],
                          day = 1:nD,
                          nameSc = nameSc,
                          Mean = NA,
                          Median = NA,
                          P10=NA,
                          P90=NA,
                          IQ = NA,
                          IIIQ = NA)
    
    trajDAdDF = data.table(city = cities[j],
                           type = "D",
                           day = 1:nD,
                           nameSc = nameSc,
                           Mean = NA,
                           Median = NA,
                           P10=NA,
                           P90=NA,
                           IQ = NA,
                           IIIQ = NA)
    
    trajHAdDF = data.table(city = cities[j],
                           type = "H",
                           day = 1:nD,
                           nameSc = nameSc,
                           Mean = NA,
                           Median = NA,
                           P10=NA,
                           P90=NA,
                           IQ = NA,
                           IIIQ = NA)
    
    trajR0DF$IQ = sapply(1:nD, function(t){quantile(R0dengueMAM[t,j,], 0.25, na.rm = T)})
    trajR0DF$Mean = sapply(1:nD, function(t){mean(R0dengueMAM[t,j,], na.rm = T)})
    trajR0DF$Median = sapply(1:nD, function(t){quantile(R0dengueMAM[t,j,], 0.5, na.rm = T)})
    trajR0DF$IIIQ = sapply(1:nD, function(t){quantile(R0dengueMAM[t,j,], 0.75, na.rm = T)})
    trajR0DF$P10 = sapply(1:nD, function(t){quantile(R0dengueMAM[t,j,], 0.1, na.rm = T)})
    trajR0DF$P90 = sapply(1:nD, function(t){quantile(R0dengueMAM[t,j,], 0.9, na.rm = T)})
    
    trajDAdDF$IQ = sapply(1:nD, function(t){quantile(DAdultsMAM[t,j,], 0.25, na.rm = T)})
    trajDAdDF$Mean = sapply(1:nD, function(t){mean(DAdultsMAM[t,j,], na.rm = T)})
    trajDAdDF$Median = sapply(1:nD, function(t){quantile(DAdultsMAM[t,j,], 0.5, na.rm = T)})
    trajDAdDF$IIIQ = sapply(1:nD, function(t){quantile(DAdultsMAM[t,j,], 0.75, na.rm = T)})
    trajDAdDF$P10 = sapply(1:nD, function(t){quantile(DAdultsMAM[t,j,], 0.1, na.rm = T)})
    trajDAdDF$P90 = sapply(1:nD, function(t){quantile(DAdultsMAM[t,j,], 0.9, na.rm = T)})
    
    trajHAdDF$IQ = sapply(1:nD, function(t){quantile(HAdultsMAM[t,j,], 0.25, na.rm = T)})
    trajHAdDF$Mean = sapply(1:nD, function(t){mean(HAdultsMAM[t,j,], na.rm = T)})
    trajHAdDF$Median = sapply(1:nD, function(t){quantile(HAdultsMAM[t,j,], 0.5, na.rm = T)})
    trajHAdDF$IIIQ = sapply(1:nD, function(t){quantile(HAdultsMAM[t,j,], 0.75, na.rm = T)})
    trajHAdDF$P10 = sapply(1:nD, function(t){quantile(HAdultsMAM[t,j,], 0.1, na.rm = T)})
    trajHAdDF$P90 = sapply(1:nD, function(t){quantile(HAdultsMAM[t,j,], 0.9, na.rm = T)})
    
    
    cat(cities[j], " - ", nameSc, ": ", sum(trajR0DF$Mean>1), "\n")
    toc()
    
    R0List[[(which(scenarios == nameSc)-1)*nIDs+j]] = trajR0DF
    DAdList[[(which(scenarios == nameSc)-1)*nIDs+j]] = trajDAdDF
    HAdList[[(which(scenarios == nameSc)-1)*nIDs+j]] = trajHAdDF
    
  }
}

trajR0DT <- data.table::rbindlist(R0List)
trajDAdDT <- data.table::rbindlist(DAdList)
trajHAdDT <- data.table::rbindlist(HAdList)

### save and load
saveRDS(trajR0DT, file = paste0(folderOut, "/trajR0DT.rds"))
saveRDS(trajDAdDT, paste0(folderOut, "/trajDAdDT.rds"))
saveRDS(trajHAdDT, paste0(folderOut, "/trajHAdDT.rds"))

trajR0DT <- readRDS(paste0(folderOut, "/trajR0DT.rds"))
trajDAdDT <- readRDS(paste0(folderOut, "/trajDAdDT.rds"))
trajHAdDT <- readRDS(paste0(folderOut, "/trajHAdDT.rds"))

trajAdDT = rbind(trajDAdDT, trajHAdDT)
                     
###################################################################################


# plot

for(cityx in cities){
  
  trajAdDF <- trajAdDT %>%
    filter(city == cityx)
  
  # plotCut <-ggplot(data = trajAdDF)+
  #   geom_line(aes(x = day, y = log(Median+1), color = type, linetype = nameSc), linewidth = 0.8)+
  #   geom_ribbon(aes(x = day, ymin=log(IQ+1), ymax=log(IIIQ+1), fill = type, linetype = nameSc), alpha = 0.2)+
  #   scale_fill_discrete(palette = colPalette1)+
  #   scale_color_discrete(palette = colPalette1)+
  #   geom_hline(aes(yintercept = log(2)),linetype = 2)+
  #   ylim(c(0,9.5))+
  #   theme_test()
  
  maxY = ceil(max(trajAdDF$IIIQ)/10^(floor(log(max(trajAdDF$IIIQ), 10))-1))*10^(floor(log(max(trajAdDF$IIIQ), 10))-1)
  
  # plotCut <-ggplot(data = trajAdDF)+
  #   geom_line(aes(x = day, y = Median, color = type, linetype = nameSc), linewidth = 0.8)+
  #   geom_ribbon(aes(x = day, ymin=IQ, ymax=IIIQ, fill = type, linetype = nameSc), alpha = 0.2)+
  #   scale_fill_discrete(palette = colPalette1)+
  #   scale_color_discrete(palette = colPalette1)+
  #   scale_y_continuous(breaks = c(0, maxY/3, 2*maxY/3, maxY), labels = c("", round(maxY/3,0), round(2*maxY/3), maxY))+
  #   geom_hline(aes(yintercept = 2),linetype = 2)+
  #   theme_test()
  
  # plotCut <- plotCut +
  #   theme(legend.position = "none",
  #         panel.grid = element_blank(), 
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         axis.text.x=element_blank(),
  #         axis.text.y=element_blank(),
  #         plot.background = element_rect(fill = "transparent", color = "transparent"))

plotCut <-ggplot(data = trajAdDF)+
  geom_line(aes(x = day, y = Mean, color = type, linetype = nameSc), linewidth = 0.8)+
  geom_ribbon(aes(x = day, ymin=P10, ymax=P90, fill = type, linetype = nameSc), alpha = 0.2)+
  scale_fill_discrete(palette = colPalette1)+
  scale_color_discrete(palette = colPalette1)+
  scale_y_continuous(breaks = c(0, maxY/3, 2*maxY/3, maxY), labels = c("", round(maxY/3,0), round(2*maxY/3), maxY))+
  geom_hline(aes(yintercept = 2),linetype = 2)+
  theme_test()
  
  plotCut <- plotCut +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.text.y = element_text(margin = margin(r = -25, unit = "pt")),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = "transparent"))
    
  ggsave(file = 
           paste0(folderPlot, "/PoPtraj_", cityx, ".png"),
         plot= plotCut, units="cm", height=5.5, width = 8.5, dpi=300) #units="in", height=4,
  
  
  trajR0DF <- trajR0DT %>%
    filter(city == cityx)
  
  # plotCut <- ggplot(data = trajR0DF)+
  #   geom_line(aes(x = day, y = Median, color = nameSc), linewidth = 0.8)+
  #   geom_ribbon(aes(x = day, ymin=IQ, ymax=IIIQ, fill = nameSc), alpha = 0.2)+
  #   scale_fill_discrete(palette = colPalette2)+
  #   scale_color_discrete(palette = colPalette2)+
  #   geom_hline(aes(yintercept = 1),linetype = 2)+
  #   ylim(c(0,2))+
  #   theme_test()
  
  plotCut <- ggplot(data = trajR0DF)+
    geom_line(aes(x = day, y = Mean, color = nameSc), linewidth = 0.8)+
    geom_ribbon(aes(x = day, ymin=P10, ymax=P90, fill = nameSc), alpha = 0.2)+
    scale_fill_discrete(palette = colPalette2)+
    scale_color_discrete(palette = colPalette2)+
    geom_hline(aes(yintercept = 1),linetype = 2)+
    ylim(c(0,max(2,max(trajR0DF$P90, na.rm = T))))+
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
