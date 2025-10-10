# Downlead meteo to simulate calibrate local cases of dengue

# La Crau 2024 (25)
 
# S. Ceclie le Vignes 2024 (18)


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

years <- 2019:2024

df_cities = data.frame(name = c("LA CRAU", "SAINTE CECILE LES VIGNES"),
                       dep = c("83", "84"),
                       weather_station = c("HYERES","ORANGE"),
                       cell = c("582", "1858"),
                       popKm2 = c(513, 133),
                       surfHa = c(3787, 1982))

# download data MétéoFrance


list_departements <- unique(df_cities$dep)

for(i in 1:length(list_departements)){
  # données 2024-2025
  httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",list_departements[i],"_latest-2024-2025_RR-T-Vent.csv.gz"),httr::write_disk(file.path(path,paste0("dpt_",list_departements[i],"_2024_2025_RR-T-Vent.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))
  
  # donnees historiques
  httr::GET(paste0("https://object.files.data.gouv.fr/meteofrance/data/synchro_ftp/BASE/QUOT/Q_",list_departements[i],"_previous-1950-2023_RR-T-Vent.csv.gz"),httr::write_disk(file.path(path,paste0("dpt_",list_departements[i],"_historique_RR-T-Vent.csv.gz")), overwrite = T),httr::progress(),config = list(maxredirs=-1))
}

# prepare data

for(name_x in df_cities$name) {
  
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
    
    saveRDS(WTotDT,
            file = paste0(folder_out, "/", name_x, "_", y, "_Safran.rds"))
  }
} 



