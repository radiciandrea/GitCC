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
library(Hmisc)

mod = ""
# sim = "AvgMin"
# sim = "MaxMin"

folderDrias = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod,"_elab")
folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod,"_sim_050")
folderSimSin = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_050")
folderPlot = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Esperimenti/Outputs/Scenari climatici/DRIAS", mod,"_sim_050")

# we choose places with 3000-5500 inhabitants
# scenarios <- c("Cn70", "Cn55", "Cn35", "Hs99", "Hg35", "Hg55", "Hg70")
scenarios <- c("Cn70", "Hs99","Hg70")
cities <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice", "Strasbourg", "Clermont-Ferrand")
IDsSubSet <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249, 7379, 3564)
# 
# #### Elab ----
# 
# MapDT <- data.table(city = rep(cities, length(scenarios)),
#                     ID = rep(IDsSubSet, length(scenarios)),
#                     scenario = rep(scenarios,length(cities)),
#                     tasAvgYea = 0, #average annual T
#                     tasMinWin = 0, #average in January
#                     tasMaxSum = 0, #average in July
#                     tasAvgMJJASO = 0, #average in MJJASO
#                     tasAvgNDJFMA = 0, #average in NDJFMA
#                     GDD15 = 0, #Growing degree days over 15
#                     E0 = NA,
#                     A0 = NA,
#                     LTS = NA,
#                     SC = NA) #
#                     
# 
# for(k in 1:length(scenarios)){
# 
#   scenariox = scenarios[k]
#   files = list.files(paste0(folderDrias,"/"), paste0("Drias_", scenariox))
# 
#   for(i in  1:length(files)){
# 
#     file = files[i]
#     year <- substr(file, nchar(file)-7, nchar(file)-4)
# 
#     WTotDT <- readRDS(paste0(folderDrias, "/Drias_", scenariox, "_", year, ".rds")) %>%
#       filter(ID %in% IDsSubSet)
# 
#     for(j in 1:length(IDsSubSet)){
# 
#       IDx = IDsSubSet[j]
#       WTotDTtemp = WTotDT %>% filter(ID == IDx)
# 
#       MapDT[scenario == scenariox & ID == IDx, tasAvgYea := (tasAvgYea*(j-1)+mean(WTotDTtemp[,tas]))/j]
#       MapDT[scenario == scenariox & ID == IDx, tasMinWin := (tasMinWin*(j-1)+mean(WTotDTtemp[DOS <= 31, tas]))/j]
#       MapDT[scenario == scenariox & ID == IDx, tasMaxSum := (tasMaxSum*(j-1)+mean(WTotDTtemp[DOS %in% 182:212, tas]))/j]
#       MapDT[scenario == scenariox & ID == IDx, tasAvgMJJASO := (tasAvgMJJASO*(j-1)+mean(WTotDTtemp[DOS %in% 121:304, tas]))/j]
#       MapDT[scenario == scenariox & ID == IDx, tasAvgNDJFMA := (tasAvgMJJASO*(j-1)+mean(WTotDTtemp[DOS %nin% 121:304, tas]))/j]
#       MapDT[scenario == scenariox & ID == IDx, GDD15 := (GDD15*(j-1)+sum(WTotDTtemp[which(tas>15), tas]))/j]
#       cat(mean(WTotDTtemp$tas), "\n")
#     }
# 
#   }
# }
# 
# # Sort DF
# 
# MapDT <- MapDT %>%
#   mutate(scenario = factor(scenario, levels = c("Cn70", "Hs99", "Hg70"))) %>%
#   arrange(scenario, ID)
# 
# MapDT$cityLabel[MapDT$scenario == "Hs99"] = unique(MapDT$city)
# 
# # load E0: 020d_MM_pis_matrix_cycle_E0_plot.R
# folderSimE0 = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_020")
# E0MM <- readRDS(paste0(folderSimE0, "/E0MM.rds"))
# E0DT = data.table(ID = 1:8981,
#                    Hs99 = E0MM[1,],
#                    Cn35 = E0MM[2,],
#                    Cn55 = E0MM[3,],
#                    Cn70 = E0MM[4,],
#                    Hg35 = E0MM[5,],
#                    Hg55 = E0MM[6,],
#                    Hg70 = E0MM[7,])
# 
# E0DTsel <- E0DT %>%
#   filter(ID %in% IDsSubSet) 
# 
# MapDT$E0 <- c(E0DTsel$Cn70, E0DTsel$Hs99, E0DTsel$Hg70)
# 
# #load A0: 020g_MM_pis_matrix_cycle_A0_plot.R
# folderSimA0 = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_020e")
# A0MM <- readRDS(paste0(folderSimA0, "/A0MM.rds"))
# A0Msel = A0MM[c(4,1,7), IDsSubSet]
# A0DT = data.table(ID = 1:8981,
#                    Hs99 = A0MM[1,],
#                    Cn35 = A0MM[2,],
#                    Cn55 = A0MM[3,],
#                    Cn70 = A0MM[4,],
#                    Hg35 = A0MM[5,],
#                    Hg55 = A0MM[6,],
#                    Hg70 = A0MM[7,])
# 
# A0DTsel <- A0DT %>%
#   filter(ID %in% IDsSubSet) 
# 
# MapDT$A0 <- c(A0DTsel$Cn70, A0DTsel$Hs99, A0DTsel$Hg70)
# 
# # load LTS: 040d_MM_SEI_pis_matrix_metacycle_SecondayCases_plot.R
# folderSimDengue = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_sim_040")
# LTSdengueMM <- readRDS(file = paste0(folderSimDengue, "/LTSdengueMM.rds"))
# LTSdengueMsel = LTSdengueMM[c(4,1,7), IDsSubSet]
# LTSDT = data.table(ID = 1:8981,
#                    Hs99 = LTSdengueMM[1,],
#                    Cn35 = LTSdengueMM[2,],
#                    Cn55 = LTSdengueMM[3,],
#                    Cn70 = LTSdengueMM[4,],
#                    Hg35 = LTSdengueMM[5,],
#                    Hg55 = LTSdengueMM[6,],
#                    Hg70 = LTSdengueMM[7,])
# 
# LTSDTsel <- LTSDT %>%
#   filter(ID %in% IDsSubSet) 
# 
# MapDT$LTS <- c(LTSDTsel$Cn70, LTSDTsel$Hs99, LTSDTsel$Hg70)
# 
# # load Secondary cases: 040d_MM_SEI_pis_matrix_metacycle_SecondayCases_plot.R
# SecCasedengueMM <- readRDS(file = paste0(folderSimDengue, "/SecCaseMM.rds"))
# SecCasedengueMsel = SecCasedengueMM[c(4,1,7), IDsSubSet]
# SCDT = data.table(ID = 1:8981,
#                    Hs99 = SecCasedengueMM[1,],
#                    Cn35 = SecCasedengueMM[2,],
#                    Cn55 = SecCasedengueMM[3,],
#                    Cn70 = SecCasedengueMM[4,],
#                    Hg35 = SecCasedengueMM[5,],
#                    Hg55 = SecCasedengueMM[6,],
#                    Hg70 = SecCasedengueMM[7,])
# 
# SCDTsel <- SCDT %>%
#   filter(ID %in% IDsSubSet) 
# 
# MapDT$SC <- c(SCDTsel$Cn70, SCDTsel$Hs99, SCDTsel$Hg70)
# 
# #### Save and Load ----
# 
# saveRDS(MapDT, file = paste0(folderSim, "/MapDTap.rds"))

MapDT <- readRDS(paste0(folderSim, "/MapDTap.rds"))

### Suitability ----

IndDT <- MapDT %>%
  filter(city %in% c("Montpellier", "Rennes", "Lille", "Paris-est", "Lyon", "Bordeaux", "Nice", "Strasbourg", "Clermont-Ferrand"))%>%
  mutate(statusSuitability = case_when(
    (E0 > 10)&(A0 > 10) ~ "7", # Strong suitability of homodynamic population
    (E0 > 10)&(A0 > 1) ~ "6", # Suitability of homodynamic population
    (E0 > 10)&(A0 > 0.1) ~ "5", # Weak suitability of homodynamic population
    (E0 > 10)&(A0 < 0.1) ~ "4", # strong suitability of diapausing population
    (E0 > 1)&(A0 > 0.1) ~ "5", # Weak suitability of homodynamic population
    (E0 > 1)&(A0 < 0.1) ~ "3", # Suitability of diapausing population
    (E0 > 0.1)&(A0 < 0.1) ~ "2", # Weak suitability of diapausing population
    (E0 < 0.1)&(A0 < 0.1) ~ "1")) # Strong unsuitability for Aedes albopictus

IndDT$statusSuitability <- as.factor(IndDT$statusSuitability)

colPal= c("#384AB4", "#8EB0FE", "#F29878",  "#B00026", "#AD5CFF", "#5200A3", "#0D001A")

plotCut <- ggplot() +
 
  geom_path(data = IndDT, aes(x = tasAvgMJJASO,
                              y = tasMinWin,
                              group = city), #AvgYea
            arrow = arrow(ends = "both",
                          type = "closed",
                          length = unit(0.05, "inches")), color = "grey70") + # , color= "white"
  geom_point(data = IndDT, aes(x =tasAvgMJJASO,
                               y = tasMinWin,
                               shape = scenario,
                               color = statusSuitability), size = 2) + #, color= "white", #shape = MapDT$pointShape, 
  scale_color_manual(values = colPal, breaks = as.factor(1:7))+
  ylim(c(1,13))+
  xlim(c(12,26))+
  theme_test()

plotCut+
  ggtitle("Suitability")+
  geom_label_repel(data = IndDT,
                   aes(x = tasAvgMJJASO, y = tasMinWin, label = cityLabel),
                   label.padding = 0.15, segment.color = NA, size = 4) #size = 4

plotCut <- plotCut +
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.background = element_rect(fill = "transparent", color = "transparent"))


ggsave(file = 
         paste0(folderPlot, "/SuitabilityStatusAP.png"),
       plot= plotCut, units="cm", height=5.5, width = 8.5, dpi=300) #units="in", height=4,


### R0 - LTS ----

IndDT <- IndDT %>%
  mutate(statusLTSDengue = case_when(
    (E0 > 1)&(LTS > 161) ~ "6",
    (E0 > 1)&(LTS > 105) ~ "5",
    (E0 > 1)&(LTS > 56) ~ "4",
    (E0 > 1)&(LTS > 21) ~ "3",
    (E0 > 1)&(LTS > 1) ~ "2",
    (E0 > 1)&(LTS < 1) ~ "1",
    (E0 < 1) ~ "0"))


colPal<- c("#0D001A", "#450054", "#3A528A", "#21908C", "#5CC963", "#FCE724", "#F7f5bc")

plotCut <- ggplot() +
  geom_path(data = IndDT, aes(x = tasAvgMJJASO,
                              y = tasMinWin,
                              group = city), #AvgYea
            arrow = arrow(ends = "both",
                          type = "closed",
                          length = unit(0.05, "inches")), color = "grey70") + # , color= "white"
  geom_point(data = IndDT, aes(x =tasAvgMJJASO,
                               y = tasMinWin,
                               shape = scenario,
                               color = statusLTSDengue), size = 2) + #, color= "white", #shape = MapDT$pointShape, 
  scale_color_manual(values = colPal, breaks = as.factor(-1+1:7))+
  ylim(c(1,13))+
  xlim(c(12,26))+
  theme_test()

plotCut+
  ggtitle("LTS")+
  geom_label_repel(data = IndDT,
                   aes(x = tasAvgMJJASO, y = tasMinWin, label = cityLabel),
                   label.padding = 0.15, segment.color = NA, size = 4) #size = 4

plotCut <- plotCut +
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.background = element_rect(fill = "transparent", color = "transparent"))

ggsave(file = 
         paste0(folderPlot, "/LTSDengueStatusAP.png"),
       plot= plotCut, units="cm", height=5.5, width = 8.5, dpi=300) #units="in", height=4,

# ### Secondary cases ----
# 
# IndDT <- IndDT %>%
#   mutate(statusSCDengue = case_when(
#     (E0 > 1)&(SC > 100) ~ "6",
#     (E0 > 1)&(SC > 50) ~ "5",
#     (E0 > 1)&(SC > 20) ~ "4",
#     (E0 > 1)&(SC > 5) ~ "3",
#     (E0 > 1)&(SC > 1) ~ "2",
#     (E0 > 1)&(SC < 1) ~ "1",
#     (E0 < 1) ~ "0"))
# 
# 
# colPal<- c("#fcfdbf", "#fc8961", "#b73779", "#51127c", "#2b0c57", "#000004")
# 
# plotCut <- ggplot() +
#   ggtitle("Secondary cases")+
#   
#   geom_path(data = IndDT, aes(x = tasAvgMJJASO,
#                               y = tasMinWin,
#                               group = city), #AvgYea
#             arrow = arrow(ends = "both",
#                           type = "closed",
#                           length = unit(0.05, "inches")), color = "grey70") + # , color= "white"
#   
#   geom_point(data = IndDT, aes(x =tasAvgMJJASO,
#                                y = tasMinWin,
#                                shape = scenario,
#                                color = statusSCDengue), size = 4) + #, color= "white", #shape = MapDT$pointShape, 
#   scale_color_manual(values = colPal, breaks = as.factor(-1+1:7))+
#   theme_test()
# 
# plotCut+
#   geom_label_repel(data = IndDT,
#                    aes(x = tasAvgMJJASO, y = tasMinWin, label = cityLabel),
#                    label.padding = 0.15, segment.color = NA, size = 4) #size = 4
# 
# plotCut <- plotCut +
#   theme(legend.position = "none",
#         plot.background = element_rect(fill = "transparent", color = "transparent"))
# 
# ggsave(file = 
#          paste0(folderPlot, "/SCDengueStatusAP.png"),
#        plot= plotCut, units="cm", height=5.2, width = 8.2, dpi=300) #units="in", height=4,
# 
# 
# 
