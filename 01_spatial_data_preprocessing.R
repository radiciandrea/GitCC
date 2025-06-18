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
folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_ELAB/"

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

## Historic ----

### GPW Historic 2000 ----

# website 0 https://www.earthdata.nasa.gov/data/projects/gpw
# website 1 https://sedac.ciesin.columbia.edudatacollectiongpw-v4
# website 2 http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/sets/browse

# documentation  http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/documentation

# inactive: doing it here

# read domain

domain = st_read(paste0(ShpFolder,"/SafranDomain.shp"))

# read 2000 GWv4 (number of persons per square kilometer)

GPW <- rast("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GPW_4/Global_2000_PopulationDensity30sec_GPWv4.tiff")

GPWCrop <- crop(GPW, ext(domain)) # France only

# writeRaster(GPW_crop, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/GPW_4/Global_2000_PopulationDensity30sec_GPWv4_France.tiff")

GPWCropExtract <- raster::extract(GPWCrop, domain) %>%
  group_by(ID) %>%
  summarise(pop = sum(Global_2000_PopulationDensity30sec_GPWv4, na.rm = T)/
              n()) %>%
  ungroup()

domainPop <- left_join(domain, GPWCropExtract)

st_write(domainPop, paste0(ShpFolder,"/SafranDomainPopHist_2000.shp"))

### Safran Historic 1996-2005 ----
# format: ncdf
# years: 1996-2005
# (8981 points) details in grilleSafran_complete_drias2021
# variables: temperature (max, min, mean), in K, precipitations, in kg/m^2/s

years = 1996:2005
name = "Hist"

# read domainPop

domainPopDF = st_read(paste0(ShpFolder,"/SafranDomainPopHist_2000.shp"))
domainPopDT = as.data.table(domainPopDF)
nReg = nrow(domainPopDT)

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
      lat = domainPopDT[id, lat],
      pop = domainPopDT[id, pop],
      DOS = as.numeric(strftime(date[indexYear], format = "%j")),
      date = date[indexYear],
      pr = prTot[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct UM later
      tas = tas[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMax = tasMax[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMin = tasMin[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear] # correct later
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

## SSP2 2050-2059 ----

### GPW SSP2 2055 ----

# website https://figshare.com/articles/dataset/Projecting_1_km-grid_population_distributions_from_2020_to_2100_globally_under_shared_socioeconomic_pathways/19608594/3

# article by Wang et al https://www.nature.com/articles/s41597-022-01675-x

# BEWARE it is a pop count

# BEWARE it uses old pop projections (IIASA 2018)

# read domain

domain = st_read(paste0(ShpFolder,"/SafranDomain.shp"))

# read 2055 

GPWSSP2_2055 <- rast("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/WANG2024/SSP2_2055.tif")

GPWCropSSP2_2055 <- crop(GPWSSP2_2055, ext(domain)) # France only

# writeRaster(GPWCropSSP2_2055, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/WANG2024/CropSSP2_2055.tiff")

GPWCropExtract <- raster::extract(GPWCropSSP2_2055, domain) %>%
  group_by(ID) %>%
  summarise(pop = sum(SSP2_2055, na.rm = T)) %>%
  ungroup()

domainPopSSP2_2055 <- left_join(domain, GPWCropExtract)

domainPopSSP2_2055$pop = 10^6*domainPopSSP2_2055$pop/as.numeric(st_area(domainPopSSP2_2055))

st_write(domainPopSSP2_2055, paste0(ShpFolder,"/SafranDomainPopSSP2_2055.shp"))

### Safran RCP 4.5 2050-2059 ----
# format: ncdf

years = 2050:2059
name = "ssp245"

# read domainPop

domainPopDF = st_read(paste0(ShpFolder,"/SafranDomainPopSSP2_2055.shp"))
domainPopDT = as.data.table(domainPopDF)
nReg = nrow(domainPopDT)

# load tas (temperature)

tasNCDF <- nc_open(paste0(dataFolder, "tasAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"))

time = ncvar_get(tasNCDF, "time") # days since 1950-01-01 00:00:00
date = as.Date(time, origin=as.Date("1950-01-01"))
yearRep = sapply(date, function(x){substr(x, 1, 4)})

tasMaxNCDF <- nc_open(paste0(dataFolder, "tasmaxAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"))
tasMinNCDF <- nc_open(paste0(dataFolder, "tasminAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"))
prTotNCDF <- nc_open(paste0(dataFolder, "prtotAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"))

#extract 3Dmatrices with ncvar_get

tas <- ncvar_get(tasNCDF, attributes(tasNCDF$var)$names[5])
tasMax <- ncvar_get(tasMaxNCDF, attributes(tasMaxNCDF$var)$names[5])
tasMin <- ncvar_get(tasMinNCDF, attributes(tasMinNCDF$var)$names[5])
prTot <- ncvar_get(prTotNCDF, attributes(prTotNCDF$var)$names[5])

rm(tasNCDF, tasMaxNCDF, tasMinNCDF, prTotNCDF)

for(year in years){
  
  indexYear = which(yearRep == year)
  
  WList <- vector(mode = "list", nReg*length(indexYear))
  
  for(id in 1:nReg){
    WDT<- data.table(
      ID = id,
      lat = domainPopDT[id, lat],
      pop = domainPopDT[id, pop],
      DOS = as.numeric(strftime(date[indexYear], format = "%j")),
      date = date[indexYear],
      pr = prTot[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct UM later
      tas = tas[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMax = tasMax[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMin = tasMin[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear] # correct later
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

## SSP2 2080-2089 ----

### GPW SSP2 2085 ----

# read domain

domain = st_read(paste0(ShpFolder,"/SafranDomain.shp"))

# read 2085 

GPWSSP2_2085 <- rast("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/WANG2024/SSP2_2085.tif")

GPWCropSSP2_2085 <- crop(GPWSSP2_2085, ext(domain)) # France only

# writeRaster(GPWCropSSP2_2085, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/WANG2024/CropSSP2_2085.tiff")

GPWCropExtract <- raster::extract(GPWCropSSP2_2085, domain) %>%
  group_by(ID) %>%
  summarise(pop = sum(SSP2_2085, na.rm = T)) %>%
  ungroup()

domainPopSSP2_2085 <- left_join(domain, GPWCropExtract)

domainPopSSP2_2085$pop = 10^6*domainPopSSP2_2085$pop/as.numeric(st_area(domainPopSSP2_2085))

st_write(domainPopSSP2_2085, paste0(ShpFolder,"/SafranDomainPopSSP2_2085.shp"))

### Safran RCP 4.5 2080-2089 ----
# format: ncdf

years = 2080:2089
name = "ssp245"

# read domainPop

domainPopDF = st_read(paste0(ShpFolder,"/SafranDomainPopSSP2_2085.shp"))
domainPopDT = as.data.table(domainPopDF)
nReg = nrow(domainPopDT)

# load tas (temperature)

tasNCDF <- nc_open(paste0(dataFolder, "tasAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20800101-20891231.nc"))

time = ncvar_get(tasNCDF, "time") # days since 1950-01-01 00:00:00
date = as.Date(time, origin=as.Date("1950-01-01"))
yearRep = sapply(date, function(x){substr(x, 1, 4)})

tasMaxNCDF <- nc_open(paste0(dataFolder, "tasmaxAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20800101-20891231.nc"))
tasMinNCDF <- nc_open(paste0(dataFolder, "tasminAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20800101-20891231.nc"))
prTotNCDF <- nc_open(paste0(dataFolder, "prtotAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20800101-20891231.nc"))

#extract 3Dmatrices with ncvar_get

tas <- ncvar_get(tasNCDF, attributes(tasNCDF$var)$names[5])
tasMax <- ncvar_get(tasMaxNCDF, attributes(tasMaxNCDF$var)$names[5])
tasMin <- ncvar_get(tasMinNCDF, attributes(tasMinNCDF$var)$names[5])
prTot <- ncvar_get(prTotNCDF, attributes(prTotNCDF$var)$names[5])

rm(tasNCDF, tasMaxNCDF, tasMinNCDF, prTotNCDF)

for(year in years){
  
  indexYear = which(yearRep == year)
  
  WList <- vector(mode = "list", nReg*length(indexYear))
  
  for(id in 1:nReg){
    WDT<- data.table(
      ID = id,
      lat = domainPopDT[id, lat],
      pop = domainPopDT[id, pop],
      DOS = as.numeric(strftime(date[indexYear], format = "%j")),
      date = date[indexYear],
      pr = prTot[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct UM later
      tas = tas[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMax = tasMax[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMin = tasMin[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear] # correct later
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
          file = paste0(folderOut, "Drias", year, name, ".rds")) 
  
}

## SSP5 2050-2059 ----

### GPW SSP5 2055 ----

# read domain

domain = st_read(paste0(ShpFolder,"/SafranDomain.shp"))

# read 2055 

GPWSSP5_2055 <- rast("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/WANG2024/SSP5_2055.tif")

GPWCropSSP5_2055 <- crop(GPWSSP5_2055, ext(domain)) # France only

# writeRaster(GPWCropSSP5_2055, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/WANG2024/CropSSP5_2055.tiff")

GPWCropExtract <- raster::extract(GPWCropSSP5_2055, domain) %>%
  group_by(ID) %>%
  summarise(pop = sum(SSP5_2055, na.rm = T)) %>%
  ungroup()

domainPopSSP5_2055 <- left_join(domain, GPWCropExtract)

domainPopSSP5_2055$pop = 10^6*domainPopSSP5_2055$pop/as.numeric(st_area(domainPopSSP5_2055))

st_write(domainPopSSP5_2055, paste0(ShpFolder,"/SafranDomainPopSSP5_2055.shp"))

### Safran RCP 8.5 2050-2059 ----
# format: ncdf

years = 2050:2059
name = "ssp585"

# read domainPop

domainPopDF = st_read(paste0(ShpFolder,"/SafranDomainPopSSP5_2055.shp"))
domainPopDT = as.data.table(domainPopDF)
nReg = nrow(domainPopDT)

# load tas (temperature)

tasNCDF <- nc_open(paste0(dataFolder, "tasAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp8.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"))

time = ncvar_get(tasNCDF, "time") # days since 1950-01-01 00:00:00
date = as.Date(time, origin=as.Date("1950-01-01"))
yearRep = sapply(date, function(x){substr(x, 1, 4)})

tasMaxNCDF <- nc_open(paste0(dataFolder, "tasmaxAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp8.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"))
tasMinNCDF <- nc_open(paste0(dataFolder, "tasminAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp8.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"))
prTotNCDF <- nc_open(paste0(dataFolder, "prtotAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp8.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"))

#extract 3Dmatrices with ncvar_get

tas <- ncvar_get(tasNCDF, attributes(tasNCDF$var)$names[5])
tasMax <- ncvar_get(tasMaxNCDF, attributes(tasMaxNCDF$var)$names[5])
tasMin <- ncvar_get(tasMinNCDF, attributes(tasMinNCDF$var)$names[5])
prTot <- ncvar_get(prTotNCDF, attributes(prTotNCDF$var)$names[5])

rm(tasNCDF, tasMaxNCDF, tasMinNCDF, prTotNCDF)

for(year in years){
  
  indexYear = which(yearRep == year)
  
  WList <- vector(mode = "list", nReg*length(indexYear))
  
  for(id in 1:nReg){
    WDT<- data.table(
      ID = id,
      lat = domainPopDT[id, lat],
      pop = domainPopDT[id, pop],
      DOS = as.numeric(strftime(date[indexYear], format = "%j")),
      date = date[indexYear],
      pr = prTot[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct UM later
      tas = tas[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMax = tasMax[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMin = tasMin[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear] # correct later
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

## SSP2 2080-2089 ----

### GPW SSP5 2085 ----

# read domain

domain = st_read(paste0(ShpFolder,"/Shp_elab/SafranDomain.shp"))

# read 2085

GPWSSP5_2085 <- rast("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/WANG2024/SSP5_2085.tif")

GPWCropSSP5_2085 <- crop(GPWSSP5_2085, ext(domain)) # France only

# writeRaster(GPWCropSSP5_2085, "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/WANG2024/CropSSP5_2085.tiff")

GPWCropExtract <- raster::extract(GPWCropSSP5_2085, domain) %>%
  group_by(ID) %>%
  summarise(pop = sum(SSP5_2085, na.rm = T)) %>%
  ungroup()

domainPopSSP5_2085 <- left_join(domain, GPWCropExtract)

domainPopSSP5_2085$pop = 10^6*domainPopSSP5_2085$pop/as.numeric(st_area(domainPopSSP5_2085))

st_write(domainPopSSP5_2085, paste0(ShpFolder,"/SafranDomainPopSSP5_2085.shp"))

### Safran RCP 8.5 2080-2089 ----
# format: ncdf

years = 2080:2089
name = "ssp585"

# read domainPop

domainPopDF = st_read("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/Shp_elab/SafranDomainPopSSP5_2085.shp")
domainPopDT = as.data.table(domainPopDF)
nReg = nrow(domainPopDT)

# load tas (temperature)

tasNCDF <- nc_open(paste0(dataFolder, "tasAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp8.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20800101-20891231.nc"))

time = ncvar_get(tasNCDF, "time") # days since 1950-01-01 00:00:00
date = as.Date(time, origin=as.Date("1950-01-01"))
yearRep = sapply(date, function(x){substr(x, 1, 4)})

tasMaxNCDF <- nc_open(paste0(dataFolder, "tasmaxAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp8.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20800101-20891231.nc"))
tasMinNCDF <- nc_open(paste0(dataFolder, "tasminAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp8.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20800101-20891231.nc"))
prTotNCDF <- nc_open(paste0(dataFolder, "prtotAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp8.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20800101-20891231.nc"))

#extract 3Dmatrices with ncvar_get

tas <- ncvar_get(tasNCDF, attributes(tasNCDF$var)$names[5])
tasMax <- ncvar_get(tasMaxNCDF, attributes(tasMaxNCDF$var)$names[5])
tasMin <- ncvar_get(tasMinNCDF, attributes(tasMinNCDF$var)$names[5])
prTot <- ncvar_get(prTotNCDF, attributes(prTotNCDF$var)$names[5])

rm(tasNCDF, tasMaxNCDF, tasMinNCDF, prTotNCDF)

for(year in years){
  
  indexYear = which(yearRep == year)
  
  WList <- vector(mode = "list", nReg*length(indexYear))
  
  for(id in 1:nReg){
    WDT<- data.table(
      ID = id,
      lat = domainPopDT[id, lat],
      pop = domainPopDT[id, pop],
      DOS = as.numeric(strftime(date[indexYear], format = "%j")),
      date = date[indexYear],
      pr = prTot[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct UM later
      tas = tas[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMax = tasMax[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear], # correct later
      tasMin = tasMin[domainPopDT[id, positionX]+1, domainPopDT[id, positionY]+1, indexYear] # correct later
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

