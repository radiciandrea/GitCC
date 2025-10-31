# after running code 01a and 01b

# Pre-processing of spatial data: weather and population ----

# Notes from "Esperimenti/Scenari climatici"

# inspired by Read_EOBS_cycle.R

# climate projections:
# https://www.drias-climat.fr/drias_prod/accueil/okapiWebDrias/index.jsp?iddrias=climat

# After meeting Cyril Pachka Paul and Benjamin Le Roy

# 3 horizons + histoic
# 2 scenarios: "hard", "median"
# 3 couples regional/global model

## Load libraries ----

library(sf)
library(terra)
library(ncdf4)
library(lubridate)
library(xlsx)
library(dplyr)
library(data.table)
library(ggplot2)
library(leaflet)

#folders

sim = "" # "", cold, hot

ShpFolder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"

if(sim ==""){
  dataFolder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS/"
  folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab/"
  codeMod = "CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63"
} else if((sim =="cold")) {
  dataFolder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS^cold/"
  folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS^cold_elab/"
  codeMod = "MPI-M-MPI-ESM-LR_MPI-CSC-REMO2009"
} else if((sim =="hot")) {
  dataFolder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS^hot/"
  folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS^hot_elab/"
  codeMod = "MOHC-HadGEM2-ES_CLMcom-CCLM4-8-17"
}

dir.create(folderOut)

safranGridGeom <- st_read(paste0(ShpFolder,"/SafranDomain.shp"))
safranGridDT = as.data.table(safranGridGeom)

## Cycle ----

# name : H
scenariosDT <- data.table(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                          yearS = c(1986, 2026, 2046, 2066, 2026, 2046, 2066),
                          yearE = c(1986, 2026, 2046, 2066, 2026, 2046, 2066)+19,
                          yearPop = c(1999, 2035, 2055, 2070, 2035, 2055, 2070),
                          omphale = c("historical", "central", "central", "central", "high", "high", "high"),
                          rcp = c("Historical", "rcp4.5", "rcp4.5", "rcp4.5", "rcp8.5",  "rcp8.5", "rcp8.5"))


for(s in scenariosDT[,name]){
  
  ### Data processing ----
  
  # (8981 points) details in grilleSafran_complete_drias2021
  # variables: temperature (max, min, mean), in K, precipitations, in kg/m^2/s
  
  name = s
  yearS = scenariosDT[name == s, yearS]
  yearE = scenariosDT[name == s, yearE]
  rcp = scenariosDT[name == s, rcp]
  
  codeOmphale = paste0("PopKm", s)
  years = yearS:yearE
  
  
  # read domainPop (beware: each cell has people/m² and a given area)
  
  domainPopDF =  st_read(paste0(folderShp, "/SafranDensOmphale.shp"))
  domainPopDT = as.data.table(domainPopDF)
  nReg = nrow(domainPopDT)
  
  # load tas (temperature)
  
  tasHistNCDF <- nc_open(paste0(dataFolder, "tasAdjust_France_", codeMod,"_",
                                rcp ,"_METEO-FRANCE_ADAMONT-France_SAFRAN_day_", yearS,"0101-", yearE,"1231.nc"))
  
  print(tasHistNCDF)
  attributes(tasHistNCDF$var)
  
  lat = ncvar_get(tasHistNCDF, "lat")
  lon = ncvar_get(tasHistNCDF, "lon")
  
  time = ncvar_get(tasHistNCDF, "time") # days since 1950-01-01 00:00:00
  date = as.Date(time, origin=as.Date("1950-01-01"))
  yearRep = sapply(date, function(x){substr(x, 1, 4)})
  
  tasMaxHistNCDF <- nc_open(paste0(dataFolder, "tasmaxAdjust_France_", codeMod,"_",
                                   rcp ,"_METEO-FRANCE_ADAMONT-France_SAFRAN_day_", yearS,"0101-", yearE,"1231.nc"))
  tasMinHistNCDF <- nc_open(paste0(dataFolder, "tasminAdjust_France_", codeMod,"_",
                                   rcp ,"_METEO-FRANCE_ADAMONT-France_SAFRAN_day_", yearS,"0101-", yearE,"1231.nc"))
  prTotHistNCDF <- nc_open(paste0(dataFolder, "prtotAdjust_France_", codeMod,"_",
                                  rcp ,"_METEO-FRANCE_ADAMONT-France_SAFRAN_day_", yearS,"0101-", yearE,"1231.nc"))
  
  #extract 3Dmatrices with ncvar_get
  
  tas <- ncvar_get(tasHistNCDF, attributes(tasHistNCDF$var)$names[5])
  tasMax <- ncvar_get(tasMaxHistNCDF, attributes(tasMaxHistNCDF$var)$names[5])
  tasMin <- ncvar_get(tasMinHistNCDF, attributes(tasMinHistNCDF$var)$names[5])
  prTot <- ncvar_get(prTotHistNCDF, attributes(prTotHistNCDF$var)$names[5])
  
  rm(tasHistNCDF, tasMaxHistNCDF, tasMinHistNCDF, prTotHistNCDF)
  
  # Cosnistency test
  # m <- abs(lat - IdomainPop$lat[1])
  # which(m == min(m, na.rm = T), arr.ind =  TRUE) # ok
  
  for(year in years){
    
    indexYear = which(yearRep == year)
    
    WList <- vector(mode = "list", nReg*length(indexYear))
    
    for(id in 1:nReg){
      WDT<- data.table(
        ID = id,
        lat = safranGridDT[id, lat],
        lon = safranGridDT[id, lon],
        pop = domainPopDT[id, get(codeOmphale)], #km2
        surfHa = domainPopDT[id, surf_ha], #ha
        DOS = as.numeric(strftime(date[indexYear], format = "%j")),
        date = date[indexYear],
        pr = prTot[safranGridDT[id, positionX]+1, safranGridDT[id, positionY]+1, indexYear], # correct UM later
        tas = tas[safranGridDT[id, positionX]+1, safranGridDT[id, positionY]+1, indexYear], # correct later
        tasMax = tasMax[safranGridDT[id, positionX]+1, safranGridDT[id, positionY]+1, indexYear], # correct later
        tasMin = tasMin[safranGridDT[id, positionX]+1, safranGridDT[id, positionY]+1, indexYear] # correct later
      )
      
      WList[[id]]<-WDT
      
      cat(id, "\n")
    }
    
    cat("YEAR:", year, "\n")
    
    WTotDT <- data.table::rbindlist(WList)
    
    #correct UM
    WTotDT[, tas:=tas-273.15] # from K to °C
    WTotDT[, tasMax:=tasMax-273.15] # from K to °C
    WTotDT[, tasMin:=tasMin-273.15] # from K to °C
    WTotDT[, pr:=pr*24*3600] # from kg/m2/s to mm/d
    
    #save
    saveRDS(WTotDT,
            file = paste0(folderOut, "Drias_", name, "_", year, ".rds"))
    
  }
  
  # check 
  
  # WTotDT_sum <- WTotDT[,.("tasAv"=mean(tas), "prCum"=sum(pr)), by = .(ID)]
  # 
  # ggplot(domainPop, aes(fill = WTotDT_sum$pr))+
  #   geom_sf()
  
  
}

## Old code ----
# 
# # website 0 https://www.earthdata.nasa.gov/data/projects/gpw
# # website 1 https://sedac.ciesin.columbia.edudatacollectiongpw-v4
# # website 2 http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/sets/browse
# 
# # documentation  http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/documentation
# 
# # inactive: doing it here
# 
# # read domain
# 
# domain = st_read(paste0(ShpFolder,"/SafranDomain.shp"))
# 
# # read 2000 GWv4 (number of persons per square kilometer)
# 
# GPW <- rast("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GPW_4/Global_2000_PopulationDensity30sec_GPWv4.tiff")
# 
# GPWCrop <- crop(GPW, ext(domain)) # France only
# 
# # writeRaster(GPW_crop, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GPW_4/Global_2000_PopulationDensity30sec_GPWv4_France.tiff")
# 
# GPWCropExtract <- raster::extract(GPWCrop, domain) %>%
#   group_by(ID) %>%
#   summarise(pop = sum(Global_2000_PopulationDensity30sec_GPWv4, na.rm = T)/
#               n()) %>%
#   ungroup()
# 
# domainPop <- left_join(domain, GPWCropExtract)
# 
# st_write(domainPop, paste0(ShpFolder,"/SafranDomainPopHist_2000.shp"))


