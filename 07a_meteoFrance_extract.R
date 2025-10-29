# Downlead meteo to simulate calibrate local cases of dengue

# La Crau 2024 (25)
 
# S. Ceclie le Vignes 2024 (18)

# and others


# Function buld by Paul to download data

# paul git
# https://github.com/ptaconet/study_albo_mtp_2023_2024

# modified to include path

# run for MM

library(httr)
library(dplyr)
library(sf)
library(furrr)
library(readr)
library(lubridate)
library(data.table)

path = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance"
folder_out = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance_elab" 

years <- 2019:2025

df_cities_2024 = data.frame(name = c("LA CRAU", "SAINTE CECILE LES VIGNES", "FREJUS", "VALLAURIS"),
                       dep = c("83", "84", "83", "06"),
                       weather_station = c("HYERES","ORANGE", "FREJUS", "ANTIBES_SAPC"), #Antibes_golf misses data
                       cell = c("582", "1858", "897", "1072"),
                       popKm2 = c(513, 133, 572, 2163),
                       surfHa = c(3787, 1982, 10227, 1304))

df_cities_2025 = data.frame(name = c("ROGNAC", "AUBAGNE"),
                            dep = c("13", "13"),
                            weather_station = c("MARIGNANE", "AUBAGNE"),
                            cell = c("882", "721"),
                            popKm2 = c(706, 869),
                            surfHa = c(1746, 5490))

df_cities = rbind(df_cities_2024, df_cities_2025)
  
# download data MétéoFrance


departements <- unique(df_cities$dep)

for(i in 1:length(departements)){
  # données 2024-2025
  httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",departements[i],"_latest-2024-2025_RR-T-Vent.csv.gz"),httr::write_disk(file.path(path,paste0("dpt_",departements[i],"_2024_2025_RR-T-Vent.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))
  
  # donnees historiques
  httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",departements[i],"_previous-1950-2023_RR-T-Vent.csv.gz"),httr::write_disk(file.path(path,paste0("dpt_",departements[i],"_historique_RR-T-Vent.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))
}

# #extract weather stattion
# dep_x = departements[3]
# df_meteofrance_2024 <- read_delim(paste0(path, "/dpt_", dep_x,"_2024_2025_RR-T-Vent.csv.gz"), delim = ";", na = "", show_col_types = FALSE)
# unique(df_meteofrance_2024 %>% pull(NOM_USUEL))

# prepare data

for(name_x in df_cities$name) {
  
  cat(name_x, "\n")
  
  weather_station_x = df_cities %>% filter(name == name_x) %>% pull(weather_station)
  dep_x = df_cities %>% filter(name == name_x) %>% pull(dep)
  
  df_meteofrance_2024 <- read_delim(paste0(path, "/dpt_", dep_x,"_2024_2025_RR-T-Vent.csv.gz"), delim = ";", na = "", show_col_types = FALSE) %>%
    filter(NOM_USUEL %in% c(weather_station_x)) %>%
    mutate(date = parse_date_time(AAAAMMJJ,"ymd"), year = year(date), month = month(date), week = week(date)) %>%
    filter(year <= max(years)) %>%
    group_by(NOM_USUEL,date,year,month,week) %>%
    summarise(prec = sum(RR, na.rm = T),
              tas = mean(TM, na.rm = T),
              tasMin = mean(TN, na.rm = T),
              tasMax = mean(TX, na.rm = T)) %>%
    rename(nom_commune = NOM_USUEL)
  
  
  df_meteofrance_pre_2024 <- read_delim(paste0(path, "/dpt_", dep_x,"_historique_RR-T-Vent.csv.gz"), delim = ";", na = "", show_col_types = FALSE) %>%
    filter(NOM_USUEL %in% c(weather_station_x)) %>%
    mutate(date = parse_date_time(AAAAMMJJ,"ymd"), year = year(date), month = month(date), week = week(date)) %>%
    filter(year >= min(years)) %>%
    group_by(NOM_USUEL,date,year,month,week) %>%
    summarise(prec = sum(RR, na.rm = T),
              tas = mean(TM, na.rm = T),
              tasMin = mean(TN, na.rm = T),
              tasMax = mean(TX, na.rm = T)) %>%
    rename(nom_commune = NOM_USUEL)
  
  df_meteofrance <- rbind(df_meteofrance_pre_2024, df_meteofrance_2024)
  
  cat("NAs in tas:", sum(is.na(df_meteofrance$tas)), "\n")
  cat("NAs in tasMax:", sum(is.na(df_meteofrance$tasMax)), "\n")
  cat("NAs in tasMin:", sum(is.na(df_meteofrance$tasMin)), "\n")
  cat("NAs in prec:", sum(is.na(df_meteofrance$prec)), "\n")
  
  # # na in TM
  # 
  # df_meteofrance$TMN[which(is.na(df_meteofrance$TMN))] = 0.5*(df_meteofrance$TMIN[which(is.na(df_meteofrance$TMN))]+df_meteofrance$TMAX[which(is.na(df_meteofrance$TMN))])
  
  
  #extract lat, lon, pop
  domain = st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDensOmphale.shp")
  
  cell_x = df_cities %>% filter(name == name_x) %>% pull(cell)
  
  cell_geo = domain %>% filter(ID == cell_x)
  lat_x =  st_coordinates(st_centroid(cell_geo))[2]
  lon_x = st_coordinates(st_centroid(cell_geo))[1]
  pop_x = df_cities %>% filter(name == name_x) %>% pull(popKm2)
  surfHa_x = df_cities %>% filter(name == name_x) %>% pull(surfHa)
    
  for(y in years){
    
    df_meteofrance_y <- df_meteofrance %>%
      filter(year == y)
    
    WTotDT <- data.table(ID = cell_x,
                         lat = lat_x,
                         lon = lon_x,
                         pop = pop_x,
                         surfHa = surfHa_x,
                         DOS = as.numeric(strftime(df_meteofrance_y %>% pull(date), format = "%j")),
                         date = df_meteofrance_y %>% pull(date),
                         prec = df_meteofrance_y %>% pull(prec),
                         tas = df_meteofrance_y %>% pull(tas),
                         tasMax = df_meteofrance_y %>% pull(tasMax),
                         tasMin = df_meteofrance_y %>% pull(tasMin))
    
    WTotDT$tas[which(is.na(WTotDT$tas))] = 0.5*(WTotDT$tasMax[which(is.na(WTotDT$tas))]+WTotDT$tasMin[which(is.na(WTotDT$tas))])
    
    saveRDS(WTotDT,
            file = paste0(folder_out, "/", name_x, "_", y, "_Safran.rds"))
  }
} 
