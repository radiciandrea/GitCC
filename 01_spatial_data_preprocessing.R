# Pre-processing of spatial data: weather and population ----

# Notes from "Esperimenti/Scenari climatici"

# inspired by Read_EOBS_cycle.R

# climate projections:
# https://www.drias-climat.fr/drias_prod/accueil/okapiWebDrias/index.jsp?iddrias=climat

# After meeting Cyril Pachka Paul


## Settings ----

## climate model 

# "WRF381P_IPSL-CM5A"

## details in

# https://www.drias-climat.fr/accompagnement/sections/40
# https://www.drias-climat.fr/accompagnement/sections/240


## climate scenarios: 
# reference
# RCP 4.5
# RCP 8.5

## years

# 1996-2005
# 2050-2059
# 2080-2089

## spatial grid:

# safran: https://www.drias-climat.fr/drias_prod/_composantsHTML/simulations/refGeoSimulations/aide_safran_drias2021.html

### Extract safran grid ----

library(xlsx)
library(dplyr)
library(data.table)
library(ggplot2)
library(leaflet)

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

st_write(safranGridGeom, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

### Download GPW Historic 2000 ----

# website 0 https://www.earthdata.nasa.gov/data/projects/gpw
# website 1 https://sedac.ciesin.columbia.edudatacollectiongpw-v4
# website 2 http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/sets/browse

# documentation  http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/documentation

# inactive: doin it here

# read domain

domain = st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomain.shp")

# read 2000 GWv4 (number of persons per square kilometer)

GPW <- rast("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GPW_4/Global_2000_PopulationDensity30sec_GPWv4.tiff")

GPWCrop <- crop(GPW, ext(domain)) # France only

# writeRaster(GPW_crop, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GPW_4/Global_2000_PopulationDensity30sec_GPWv4_France.tiff")

GPWCropExtract <- raster::extract(GPWCrop, domain) %>%
  group_by(ID) %>%
  summarise(pop = mean(Global_2000_PopulationDensity30sec_GPWv4, na.rm = T)) %>%
  ungroup()

domainPop <- left_join(domain, GPWCropExtract)

st_write(domainPop, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomainPop2000.shp")

### Download Safran Historic 1996-2005 ----
# format: ncdf
# years: 1996-2005
# (8981 points) details in grilleSafran_complete_drias2021
# variables: temperature (max, min, mean), in K, precipitations, in kg/m^2/s

library(sf)
library(terra)
library(ncdf4)
library(lubridate)

dataFolder = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS/"
folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_ELAB/"

years = 1996:2005
name = "Hist"

# read domainPop

domainPop = as.data.table(st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomainPop2000.shp"))

nReg = nrow(domainPop)

# load tas (temperature)

# tasHistRaster <- rast(paste0(dataFolder, "tasAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_Historical_METEO-FRANCE_ADAMONT-France_SAFRAN_day_19960101-20051231.nc"))
# observation: it has no latitude longitude and so on.
# plot(tasHistRaster)

tasHistNCDF <- nc_open(paste0(dataFolder, "tasAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_Historical_METEO-FRANCE_ADAMONT-France_SAFRAN_day_19960101-20051231.nc"))

print(tasHistNCDF)
attributes(tasHistNCDF$var)

lat = ncvar_get(tasHistNCDF, "lat")
lon = ncvar_get(tasHistNCDF, "lon")

time = ncvar_get(tasHistNCDF, "time") # days since 1950-01-01 00:00:00
date = as.Date(time, origin=as.Date("1950-01-01"))
yearRep = sapply(date, function(x){substr(x, 1, 4)})

tasMaxHistNCDF <- nc_open(paste0(dataFolder, "tasmaxAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_Historical_METEO-FRANCE_ADAMONT-France_SAFRAN_day_19960101-20051231.nc"))
tasMinHistNCDF <- nc_open(paste0(dataFolder, "tasminAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_Historical_METEO-FRANCE_ADAMONT-France_SAFRAN_day_19960101-20051231.nc"))
prTotHistNCDF <- nc_open(paste0(dataFolder, "prtotAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_Historical_METEO-FRANCE_ADAMONT-France_SAFRAN_day_19960101-20051231.nc"))

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
      lon = domainPop[id, lon],
      lat = domainPop[id, lat],
      pop = domainPop[id, pop],
      year = year,
      DOS = as.numeric(strftime(date[indexYear], format = "%j")),
      date = date[indexYear],
      pr = prTot[domainPop[id, positionX]+1, domainPop[id, positionY]+1, indexYear], # correct UM later
      tas = tas[domainPop[id, positionX]+1, domainPop[id, positionY]+1, indexYear], # correct later
      tasMax = tasMax[domainPop[id, positionX]+1, domainPop[id, positionY]+1, indexYear], # correct later
      tasMin = tasMin[domainPop[id, positionX]+1, domainPop[id, positionY]+1, indexYear] # correct later
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
       file = paste0(folderOut, "_", year, "_", name, ".RDS")) 
  
}