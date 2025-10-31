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

