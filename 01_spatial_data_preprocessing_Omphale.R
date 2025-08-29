#### Spatial data pre processing from Insee adn Omphale  ----
# using OMPHALE 2022

# data from https://www.insee.fr/fr/statistiques/6652134?sommaire=6652140
 
# explication from http://www.progedo-adisp.fr/apf_projpop.php#:~:text=Qu'est%2Dce%20que%20l,de%20sc%C3%A9narios%20pour%20le%20futur


# reminder of ssp scenarios:
# ssp 5 - "fossil fuelled development". Maybe associated with "hypothèse haute", even if population in ssp5 is supposed to decline... 
# ssp 2 - "median hypothesis" -> associated with central scenario

# new horizons: 
# 1996-2005 (« hist, 2000 »)
# 2035-2044 « 2040 »
# 2065-2074 « 2070 »

## Load libraries ----

library(sf)
library(ncdf4)
library(lubridate)
library(xlsx)
library(dplyr)
library(data.table)
library(ggplot2)
library(vroom)
library(spatialEco)

## Demography: create dep shp ----

#folders

folderInsee <- "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/INSEE_Omphale_2022"
folderAdminShp <- "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_adm"
foldeShp <- "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"

depShp <- st_read(paste0(folderAdminShp, "/departements-20180101-shp/departements-20180101.shp"))

# consider France metropolitain only (there are = 69)

depsMetrV <- c(c("01","02","03","04","05","06","07","08","09"), 10:19, c("2A", "2B"), 21:95)

#Handle Corse, Rhone, overseas territories, and order

dep69Shp <- depShp %>%
  filter(code_insee %in% c("69D", "69M")) %>%
  st_union() %>%
  st_sf() %>%
  mutate(code_insee = "69",
         nom = "Rhône",
         nuts3 = "FR716",
         wikipedia = "fr:Rhône (département)",
         surf_km2 = 2720+538)

depMetrShp <- depShp %>%
  filter(!(code_insee %in% c("971", "972", "973", "974", "976","69D", "69M")))

depMetrShp <- rbind(depMetrShp, dep69Shp) 

depMetrShp <- depMetrShp[match(depsMetrV, depMetrShp$code_insee),]

# create Vanilla df

years <- 2018:2070

popDepTot <- data.frame(matrix(NA, nrow = length(years), ncol = nrow(depMetrShp)+1))
names(popDepTot) = c("years", depMetrShp$nom)
popDepTot$years = years

##### Central Scenario ----

popDepExtendedCentral <- xlsx::read.xlsx(paste0(folderInsee, "/donnees_det_Central.xlsx"),
                                         sheetName = "Population",
                                         startRow = 6)

popDepCentral <- popDepTot

for(d in depsMetrV){
  
  nameD = depMetrShp$nom[depMetrShp$code_insee == d]
  
  popDepDF <- popDepExtendedCentral %>% filter(ZONE == nameD)
  
  popDepV <- colSums(popDepDF[, 4:ncol(popDepDF)])
  
  popDepCentral[,which(names(popDepCentral)==nameD)] = popDepV
  
}

# Pop of France in 2025, 2040, 2070. it does not change a lot. 

sum(popDepCentral[which(popDepCentral$years == 2025),])
sum(popDepCentral[which(popDepCentral$years == 2040),])
sum(popDepCentral[which(popDepCentral$years == 2070),])

##### High Scenario ----

popDepExtendedHigh <- xlsx::read.xlsx(paste0(folderInsee, "/donnees_det_Pop_haute.xlsx"),
                                      sheetName = "Population",
                                      startRow = 6)

popDepHigh <- popDepTot

for(d in depsMetrV){
  
  nameD = depMetrShp$nom[depMetrShp$code_insee == d]
  
  popDepDF <- popDepExtendedHigh %>% filter(ZONE == nameD)
  
  popDepV <- colSums(popDepDF[, 4:ncol(popDepDF)])
  
  popDepHigh[,which(names(popDepHigh)==nameD)] = popDepV
  
}

# Pop of France in 2025, 2040, 2070. It changes quite a lot

sum(popDepHigh[which(popDepHigh$years == 2025),])
sum(popDepHigh[which(popDepHigh$years == 2040),])
sum(popDepHigh[which(popDepHigh$years == 2070),])


##### Historic Scenario (1999)----

popHist <- xlsx::read.xlsx(paste0(folderInsee, "/base-pop-historiques-1876-2022_simpl.xlsx"),
                           sheetName = "pop_1876_2022")

popDepHistMetr <- popHist %>%
  rename(code_dep = DEP) %>%
  rename(nom = LIBGEO) %>%
  rename(pop_1999 = PSDC1999) %>%
  filter(!(code_dep %in% c("971", "972", "973", "974"))) %>%
  group_by(code_dep)%>%
  summarise(popDep_1999 = sum(pop_1999)) %>%
  ungroup()

popDepHistMetr <- popDepHistMetr[match(depsMetrV, popDepHistMetr$code_dep),]

##### Create Shp of Dep with Pop ----

depDemMetrShp <- depMetrShp %>%
  rename(code_dep = code_insee ) %>%
  mutate(pop_1999 = popDepHistMetr$popDep_1999) %>%
  mutate(pop_2018 = as.numeric(as.vector(popDepCentral[which(popDepCentral$years == 2018),2:ncol(popDepCentral)]))) %>%
  mutate(pop_Cn2025 = as.numeric(as.vector(popDepCentral[which(popDepCentral$years == 2025),2:ncol(popDepCentral)]))) %>%
  mutate(pop_Cn2040 = as.numeric(as.vector(popDepCentral[which(popDepCentral$years == 2040),2:ncol(popDepCentral)]))) %>%
  mutate(pop_Cn2070 = as.numeric(as.vector(popDepCentral[which(popDepCentral$years == 2070),2:ncol(popDepCentral)]))) %>%
  mutate(pop_Hg2025 = as.numeric(as.vector(popDepHigh[which(popDepHigh$years == 2025),2:ncol(popDepCentral)]))) %>%
  mutate(pop_Hg2040 = as.numeric(as.vector(popDepHigh[which(popDepHigh$years == 2040),2:ncol(popDepCentral)]))) %>%
  mutate(pop_Hg2070 = as.numeric(as.vector(popDepHigh[which(popDepHigh$years == 2070),2:ncol(popDepCentral)])))

st_write(depDemMetrShp, paste0(foldeShp, "/DemHist_ScenariosOmphaleCentrHighDep.shp"))

## Demography: create comm shp ----

communesShp <- st_read(paste0(folderAdminShp, "/communes-20180101-shp/communes-20181110-metr-simp001.shp"))

communesShp <- communesShp %>%
  rename(code_insee = insee)  %>%
  select(-c("nom"))

##### Correct INSEE code associated to multiple municipalities ----
# found with which(table(communesShp$code_insee) == 2)

multiCodeInsee = c("05001", "21352", "38284", "38560", "44003", "69019", "69135", "73151", "73236", "74212", "74289")

# multiCodeInsee = c("05001")

communesMultiCodeInseeShp <- communesShp  %>%
  filter(code_insee %in% multiCodeInsee)

# df to keep area (dissolve does not work very good)
communesMultiCodeInseeDissDF <- communesMultiCodeInseeShp  %>%
  st_drop_geometry() %>%
  group_by(code_insee) %>%
  summarise(surf_ha = sum(surf_ha)) %>%
  ungroup()

communesMonoCodeInseeShp <- communesShp  %>%
  filter(!(code_insee %in% multiCodeInsee))

# I dissolve the polyongs of the municipalities with the same insee code
communesMultiCodeInseeDissShp <- sf_dissolve(communesMultiCodeInseeShp, "code_insee")

communesMultiCodeInseeDissShp <- communesMultiCodeInseeDissShp %>%
  rename(geometry = x)

communesMultiCodeInseeDissShp <- left_join(communesMultiCodeInseeDissShp, communesMultiCodeInseeDissDF )

# rbind the old and the new dataframe

communesCodeInseeDissShp <- rbind(communesMonoCodeInseeShp, communesMultiCodeInseeDissShp) 

##### Load population ----

communesPop2018DF <- vroom(paste0(folderInsee, "/base-ic-evol-struct-pop-2018_simpl.csv")) %>%
  select(c("DEP", "COM", "LIBCOM", "P18_POP")) %>%
  rename(code_insee = COM)%>%
  rename(nom = LIBCOM) %>%
  mutate(code_dep = substr(code_insee, 1, 2)) %>%
  filter(code_dep != "97")%>%
  group_by(code_dep, code_insee, nom) %>%
  summarise(Pop_18 = sum(P18_POP)) %>%
  ungroup()

# compute Frac pop in each Dep

communesPop2018DF <- communesPop2018DF %>%
  group_by(code_dep)%>%
  mutate(PopDep_18 = sum(Pop_18)) %>%
  ungroup() %>%
  mutate(fracPopNommDep_18 = Pop_18/PopDep_18)

depDemMetrShp <- st_read(paste0(foldeShp, "/DemHist_ScenariosOmphaleCentrHighDep.shp")) %>%
  rename(popDep_1999 = pop_1999) %>%
  rename(popDep_Cn2070 = pop_Cn2070) %>%
  rename(popDep_Hg2040 = pop_Hg2040) %>%
  rename(popDep_Cn2040 = pop_Cn2040) %>%
  rename(popDep_Hg2070 = pop_Hg2070) 

# keep only pop for dep

depDemMetrDF <- depDemMetrShp %>%
  st_drop_geometry() %>%
  select(c("code_dep", "popDep_1999", "popDep_Cn2040", "popDep_Cn2070", "popDep_Hg2040", "popDep_Hg2070"))

# join with communes

communesPopDF <- left_join(communesPop2018DF, depDemMetrDF) %>%
  mutate(popCom_1999 = fracPopNommDep_18*popDep_1999) %>%
  mutate(popCom_Cn2040 = fracPopNommDep_18*popDep_Cn2040) %>%
  mutate(popCom_Hg2040 = fracPopNommDep_18*popDep_Hg2040) %>%
  mutate(popCom_Cn2070 = fracPopNommDep_18*popDep_Cn2070) %>%
  mutate(popCom_Hg2070 = fracPopNommDep_18*popDep_Hg2070)

##### Correct municipalities with multiple INSEE----

# merge problematic commons: Marseille, Lyon, Paris
MarseillePopDF <- communesPopDF %>%
  filter(code_insee %in% c(13201:13216)) 

MarseillePopDF <- data.frame(code_dep = 13,
                             code_insee = 13055,
                             nom = "Marseille",
                             Pop_18 = sum(MarseillePopDF$Pop_18),
                             PopDep_18 = sum(MarseillePopDF$PopDep_18),
                             fracPopNommDep_18 = sum(MarseillePopDF$fracPopNommDep_18),
                             popDep_1999 = sum(MarseillePopDF$popDep_1999),
                             popDep_Cn2040 = mean(MarseillePopDF$popDep_Cn2040),
                             popDep_Cn2070 = mean(MarseillePopDF$popDep_Cn2070),
                             popDep_Hg2040 = mean(MarseillePopDF$popDep_Hg2040),
                             popDep_Hg2070 = mean(MarseillePopDF$popDep_Hg2070),
                             popCom_1999 = sum(MarseillePopDF$popCom_1999),
                             popCom_Cn2040 = sum(MarseillePopDF$popCom_Cn2040),
                             popCom_Hg2040 = sum(MarseillePopDF$popCom_Hg2040),
                             popCom_Cn2070 = sum(MarseillePopDF$popCom_Cn2070),
                             popCom_Hg2070 = sum(MarseillePopDF$popCom_Hg2070))

LyonPopDF <- communesPopDF %>%
  filter(code_insee %in% c(69381:69389)) 

LyonPopDF <- data.frame(code_dep = 69,
                        code_insee = 69123,
                        nom = "Lyon",
                        Pop_18 = sum(LyonPopDF$Pop_18),
                        PopDep_18 = sum(LyonPopDF$PopDep_18),
                        fracPopNommDep_18 = sum(LyonPopDF$fracPopNommDep_18),
                        popDep_1999 = sum(LyonPopDF$popDep_1999),
                        popDep_Cn2040 = mean(LyonPopDF$popDep_Cn2040),
                        popDep_Cn2070 = mean(LyonPopDF$popDep_Cn2070),
                        popDep_Hg2040 = mean(LyonPopDF$popDep_Hg2040),
                        popDep_Hg2070 = mean(LyonPopDF$popDep_Hg2070),
                        popCom_1999 = sum(LyonPopDF$popCom_1999),
                        popCom_Cn2040 = sum(LyonPopDF$popCom_Cn2040),
                        popCom_Hg2040 = sum(LyonPopDF$popCom_Hg2040),
                        popCom_Cn2070 = sum(LyonPopDF$popCom_Cn2070),
                        popCom_Hg2070 = sum(LyonPopDF$popCom_Hg2070))

ParisPopDF <- communesPopDF %>%
  filter(code_insee %in% c(75101:75120)) 

ParisPopDF <- data.frame(code_dep = 75,
                         code_insee = 75056,
                         nom = "Paris",
                         Pop_18 = sum(ParisPopDF$Pop_18),
                         PopDep_18 = sum(ParisPopDF$PopDep_18),
                         fracPopNommDep_18 = sum(ParisPopDF$fracPopNommDep_18),
                         popDep_1999 = sum(ParisPopDF$popDep_1999),
                         popDep_Cn2040 = mean(ParisPopDF$popDep_Cn2040),
                         popDep_Cn2070 = mean(ParisPopDF$popDep_Cn2070),
                         popDep_Hg2040 = mean(ParisPopDF$popDep_Hg2040),
                         popDep_Hg2070 = mean(ParisPopDF$popDep_Hg2070),
                         popCom_1999 = sum(ParisPopDF$popCom_1999),
                         popCom_Cn2040 = sum(ParisPopDF$popCom_Cn2040),
                         popCom_Hg2040 = sum(ParisPopDF$popCom_Hg2040),
                         popCom_Cn2070 = sum(ParisPopDF$popCom_Cn2070),
                         popCom_Hg2070 = sum(ParisPopDF$popCom_Hg2070))

# This municiaplity, "Saline" (14712) does not exist anymore, but here is the merge of "Sannerville" (14666) and "Troarn" (14712)

SalinePopDF <- communesPopDF %>%
  filter(code_insee %in% c(14712, 14666)) 

SalinePopDF <- data.frame(code_dep =14,
                          code_insee = 14712,
                          nom = "Saline",
                          Pop_18 = sum(SalinePopDF$Pop_18),
                          PopDep_18 = sum(SalinePopDF$PopDep_18),
                          fracPopNommDep_18 = sum(SalinePopDF$fracPopNommDep_18),
                          popDep_1999 = sum(SalinePopDF$popDep_1999),
                          popDep_Cn2040 = mean(SalinePopDF$popDep_Cn2040),
                          popDep_Cn2070 = mean(SalinePopDF$popDep_Cn2070),
                          popDep_Hg2040 = mean(SalinePopDF$popDep_Hg2040),
                          popDep_Hg2070 = mean(SalinePopDF$popDep_Hg2070),
                          popCom_1999 = sum(SalinePopDF$popCom_1999),
                          popCom_Cn2040 = sum(SalinePopDF$popCom_Cn2040),
                          popCom_Hg2040 = sum(SalinePopDF$popCom_Hg2040),
                          popCom_Cn2070 = sum(SalinePopDF$popCom_Cn2070),
                          popCom_Hg2070 = sum(SalinePopDF$popCom_Hg2070))


# rbind corrected municipalities
communesPopDF <- communesPopDF %>%
  filter(!(code_insee %in% c(c(14712, 14666), c(75101:75120), c(69381:69389), c(13201:13216)))) 

communesPopDF <- rbind(communesPopDF, MarseillePopDF, LyonPopDF, ParisPopDF, SalinePopDF)

#join with shp
communesPopShp <- left_join(communesPopDF, communesCodeInseeDissShp, by = "code_insee")

#calculate density: per m2

