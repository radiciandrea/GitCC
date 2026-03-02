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
folderOut = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_050")
folderData = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod, "_sim_0530")

# scenarios and cities

scenarios <- c("Cn70", "Hs99","Hg70")

domainPop <- st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDensOmphale.shp") %>%
  st_drop_geometry() %>%
  group_by(ID) %>%
  summarise(popKmAvg = mean(c(PopKmHs99, PopKmCn35, PopKmCn55, PopKmCn70, PopKmHg35, PopKmHg55, PopKmHg70))) %>%
  ungroup()

#between 2500 and 5000
cities <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice", "Strasbourg", "Clermont-Ferrand")
IDs <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249, 7379, 3564)

# between 50 and 100 hab
citiesCountryside <- c("Montpellier (Montarnaud)",
                       "Nantes (Saint-Philbert-de-Grand-Lieu)",
                       "Rennes (Janzé)",
                       "Lille (Esquelbecq)", 
                       "Paris-est (Crisenoy)", 
                       "Lyon (Savigneux)", 
                       "Grenoble (La Motte d'Aveillans)",
                       "Bordeaux (Tizac de Courton)",
                       "Toulouse (Lavalette)",
                       "Marseille (Le Tholonet)",
                       "Nice (Sospel)",
                       "Strasbourg (Epfig)",
                       "Clermont-Ferrand (Bassinet)")
IDsCountryside <- c(1038, 4984, 6256, 8966, 7285, 3823, 2699, 2475, 1017, 971, 1521, 7024, 3646)
# 
# cities <- c(cities, "46190 Sousceyrac-en-Quercy (Comiac)")
# IDs <- c(IDs,2580)

cities <- c(cities, citiesCountryside)
IDs <- c(IDs,IDsCountryside)

cityDF = data.frame(city = cities,
                    ID = IDs,
                    stat = c(rep("city", length(citiesCountryside)), rep("countryside", length(citiesCountryside))))

cityDF <- left_join(cityDF, domainPop) %>%
  arrange(ID)




##
cityDF <- cityDF %>%
  filter(city %in% c("Montpellier (Montarnaud)",
                     "Montpellier",
                     "Bordeaux (Tizac de Courton)",
                     "Bordeaux",
                     "Strasbourg (Epfig)",
                     "Strasbourg"))

####

IDsSubSet = cityDF$ID
cities = cityDF$city

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

##### cut

colPalette1 <- c("#D04B45", "#5200A3")
colPalette2 <- c( "#21908C", "#FCE724", "#3A528A")
trajR0DT <- readRDS(paste0(folderOut, "/trajR0DT.rds"))
trajDAdDT <- readRDS(paste0(folderOut, "/trajDAdDT.rds"))
trajHAdDT <- readRDS(paste0(folderOut, "/trajHAdDT.rds"))

trajAdDT = rbind(trajDAdDT, trajHAdDT)

### plot ----

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
  
  maxY = round(ceil(max(trajAdDF$IIIQ)/30^(floor(log(max(trajAdDF$IIIQ), 30))-1))*30^(floor(log(max(trajAdDF$IIIQ), 30))-1), 0)
  maxY = 1000 + 500*(max(trajAdDF$IIIQ)>1000)
  
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
    geom_line(aes(x = day, y = Mean, color = type, linetype = nameSc), linewidth = 0.5)+
    scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
    geom_ribbon(aes(x = day, ymin=P10, ymax=P90, fill = type, linetype = nameSc), alpha = 0.2)+
    scale_fill_discrete(palette = colPalette1)+
    scale_color_discrete(palette = colPalette1)+
    scale_y_continuous(breaks = c(0, 500, 1000, 1500))+
    coord_cartesian(ylim = c(0, maxY))+
    geom_hline(aes(yintercept = 1),linetype = 2)+
    theme_test()
  
  plotCut <- plotCut +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.text.y = element_blank(), #element_text(size = 4, margin = margin(r = -12, unit = "pt"))
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          plot.background = element_rect(fill = "transparent", color = "transparent"))
  
  ggsave(file = 
           paste0(folderPlot, "/ArticlePoPtraj_", cityx, ".png"),
         plot= plotCut, units="cm", height=8/3+0.2+4/3*(max(trajAdDF$IIIQ)>1000), width = 8.5, dpi=300) #units="in", height=4,
  
  
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
  
  maxY = 8 + 4*(max(trajR0DF$IIIQ)>8)
  
  plotCut <- ggplot(data = trajR0DF)+
    geom_line(aes(x = day, y = Mean, color = nameSc), linewidth = 0.5)+
    geom_ribbon(aes(x = day, ymin=P10, ymax=P90, fill = nameSc), alpha = 0.2)+
    scale_fill_discrete(palette = colPalette2)+
    scale_color_discrete(palette = colPalette2)+
    geom_hline(aes(yintercept = 1),linetype = 2)+
    scale_y_continuous(breaks = c(0, 4, 8, 12))+
    coord_cartesian(ylim = c(0, maxY))+ #max(2,max(trajR0DF$P90, na.rm = T))
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
           paste0(folderPlot, "/ArticleR0traj_", cityx, ".png"),
         plot= plotCut, units="cm", height=8/3+0.2+4/3*(max(trajR0DF$IIIQ)>8), width = 8.5, dpi=300) #units="in", height=4,
  
}
