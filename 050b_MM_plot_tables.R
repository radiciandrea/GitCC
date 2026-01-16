# code to print the table for each variable of interest

library(dplyr)
library(pracma)
library(data.table)
library(scales)
library(flextable)

### COMMON LOADINGS ----

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

cities <- c(cities, citiesCountryside)
IDs <- c(IDs,IDsCountryside)

cityDF = data.frame(city = cities,
                    ID = IDs,
                    stat = c(rep("city", length(citiesCountryside)), rep("countryside", length(citiesCountryside))))

#### LOAD DIFFERENT MODELS ----

mod = "cold"
folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod,"_sim_050")

MapDTcold <- readRDS(paste0(folderSim, "/MapDTap.rds")) %>%
  select(-c("tasAvgYea", "tasMaxSum", "tasAvgNDJFMA", "GDD15", "SC", "cityLabel", "ID")) %>%
  mutate(Model = "cold")

####

mod = ""
folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod,"_sim_050")

MapDTintermediate <- readRDS(paste0(folderSim, "/MapDTap.rds")) %>%
  select(-c("tasAvgYea", "tasMaxSum", "tasAvgNDJFMA", "GDD15", "SC", "cityLabel", "ID")) %>%
  mutate(Mod = "inter.")

####

mod = "hot"
folderSim = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod,"_sim_050")

MapDTwarm <- readRDS(paste0(folderSim, "/MapDTap.rds")) %>%
  select(-c("tasAvgYea", "tasMaxSum", "tasAvgNDJFMA", "GDD15", "SC", "cityLabel", "ID")) %>%
  mutate(Model = "warm")

#### COMMON ELABORATIONS

MapDT <- rbind(MapDTcold, MapDTintermediate, MapDTwarm) %>%
  mutate(scenario = case_when(scenario == "Cn70" ~ "MP, 2066-85",
                              scenario == "Hg70" ~ "HP, 2066-85",
                              scenario == "Hs99" ~ "1986-2005")) %>%
  arrange(city, scenario) %>%
  mutate(tasMinWin = scientific(tasMinWin, digits = 3)) %>%
  mutate(tasAvgMJJASO = scientific(tasAvgMJJASO, digits = 3)) %>%
  mutate(E0 = scientific(E0, digits = 3)) %>%
  mutate(A0 = scientific(A0, digits = 3)) %>%
  mutate(LTS = scientific(LTS, digits = 3)) 
  
names(MapDT) <- c("Site", "Scenario", "T January (°C)", "T May to October (°C)", "E0", "A0", "LTS", "Modl")

#plot in table

MapDT_doc <- flextable::flextable(data = MapDT)
print(MapDT_doc , preview ="docx")
