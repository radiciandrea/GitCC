# Pre-processing of spatial data: weather and population ----

# Notes from "Esperimenti/Scenari climatici"

# inspired by Read_EOBS_cycle.R

# climate projections:
# https://www.drias-climat.fr/drias_prod/accueil/okapiWebDrias/index.jsp?iddrias=climat

# After meeting Cyril Pachka Paul

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

ShpFolder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab"
dataFolder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS/"
folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab/"

## Settings ----

## climate model 

# "CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63"

## details in

# https://www.drias-climat.fr/accompagnement/sections/40
# https://www.drias-climat.fr/accompagnement/sections/240


## climate scenarios: 
# reference
# RCP 4.5
# RCP 8.5

## years

# 1986-2005
# 2046-2065
# 2066-2086

## spatial grid:

# safran: https://www.drias-climat.fr/drias_prod/_composantsHTML/simulations/refGeoSimulations/aide_safran_drias2021.html

## Extract safran grid ----

SafranFolder <- "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/SAFRAN/"

safranGrid <- read.xlsx2(paste0(SafranFolder, "grilleSafran_utile_drias2021.xls"),
                         sheetName = "grilleSafran",
                         startRow = 8,
                         endRow = 8988,
                         colIndex = 1:10,
                         header = FALSE)

safranGrid <- lapply(safranGrid, as.numeric)

names(safranGrid) <- c("point", 
                       "positionX", "positionY",
                       "LambertIIE", "LambertIIN",
                       "Lambert93E", "Lambert93N",
                       "lon", "lat", "alt")

safranGrid <- as.data.table(safranGrid)

#define a grid over Lambert II. Get consecutive points

#table(safranGrid$positionY)
deltaLambertIIE = safranGrid$LambertIIE[safranGrid$positionY == 99][2] - safranGrid$LambertIIE[safranGrid$positionY == 99][1]

#table(safranGrid$positionX)
deltaLambertIIN = safranGrid$LambertIIN[safranGrid$positionX == 70][2] - safranGrid$LambertIIN[safranGrid$positionX == 70][1]

safranGrid[, c("LambertIIEmin") := LambertIIE - deltaLambertIIE/2]
safranGrid[, c("LambertIIEmax") := LambertIIE + deltaLambertIIE/2]
safranGrid[, c("LambertIINmin") := LambertIIN - deltaLambertIIN/2]
safranGrid[, c("LambertIINmax") := LambertIIN + deltaLambertIIN/2]

safranGridList <- list()

# define polygons
for(i in 1:nrow(safranGrid)) {
  Emin <- safranGrid[i,LambertIIEmin]
  Emax <- safranGrid[i,LambertIIEmax]
  Nmin <- safranGrid[i,LambertIINmin]
  Nmax <- safranGrid[i,LambertIINmax]
  
  x <- c(Emax, Emax, Emin, Emin, Emax)
  y <- c(Nmin, Nmax, Nmax, Nmin, Nmin)
  
  xy <- list(matrix(c(x,y), ncol = 2, byrow = F))
  
  polyTemp = st_polygon(xy)
  polyTemp = st_sf(geometry = st_sfc(polyTemp), crs = 27572) # L II étendue; L 93 2154
  
  safranGridList[[i]] <- polyTemp
  
  cat(i, "\n")
}

safranGridGeom <- do.call("rbind", safranGridList)

# combine datatable

safranGridGeom <- cbind(safranGridGeom, safranGrid)

#compute surface km^2
#A <- st_area(safranGridGeom)[1] is 6.4e+07 [m^2]

# change CRS to WGS 84 and keep only interesting columns

safranGridGeom <- st_transform(safranGridGeom, crs = 4326)

safranGridGeom <- safranGridGeom[, c("point", "positionX", "positionY", "lat", "lon", "geometry")]

# check
# 
# leaflet() %>% 
#   addTiles() %>%
#   addPolygons(data= safranGridGeom)

safranGridGeom <- safranGridGeom %>%
  mutate(ID = rank(point))

st_write(safranGridGeom, paste0(ShpFolder,"/SafranDomain.shp"))

## Running code 00 ----

safranGridGeom <- st_read(paste0(ShpFolder,"/SafranDomain.shp"))
safranGridDT = as.data.table(safranGridGeom)

## Cycle ----

# name : H
scenariosDT <- data.table(name = c("Hs99", "Cn35", "Cn55", "Cn70", "Hg35", "Hg55", "Hg70"),
                          yearS = c(1986, 2026, 2046, 2066, 2046, 2066),
                          yearE = c(1986, 2026, 2046, 2066, 2046, 2066)+19,
                          yearPop = c(1999, 2035, 2055, 2070, 2055, 2070),
                          omphale = c("historical", "central", "central", "central", "high", "high"),
                          rcp = c("Historical", "rcp4.5", "rcp4.5", "rcp4.5", "rcp8.5", "rcp8.5"))


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
  
  tasHistNCDF <- nc_open(paste0(dataFolder, "tasAdjust_France_CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63_",
                                rcp ,"_METEO-FRANCE_ADAMONT-France_SAFRAN_day_", yearS,"0101-", yearE,"1231.nc"))
  
  print(tasHistNCDF)
  attributes(tasHistNCDF$var)
  
  lat = ncvar_get(tasHistNCDF, "lat")
  lon = ncvar_get(tasHistNCDF, "lon")
  
  time = ncvar_get(tasHistNCDF, "time") # days since 1950-01-01 00:00:00
  date = as.Date(time, origin=as.Date("1950-01-01"))
  yearRep = sapply(date, function(x){substr(x, 1, 4)})
  
  tasMaxHistNCDF <- nc_open(paste0(dataFolder, "tasmaxAdjust_France_CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63_",
                                   rcp ,"_METEO-FRANCE_ADAMONT-France_SAFRAN_day_", yearS,"0101-", yearE,"1231.nc"))
  tasMinHistNCDF <- nc_open(paste0(dataFolder, "tasminAdjust_France_CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63_",
                                   rcp ,"_METEO-FRANCE_ADAMONT-France_SAFRAN_day_", yearS,"0101-", yearE,"1231.nc"))
  prTotHistNCDF <- nc_open(paste0(dataFolder, "prtotAdjust_France_CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63_",
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


