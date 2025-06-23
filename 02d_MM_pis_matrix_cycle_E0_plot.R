#Plot E0 ----

# Inspired by 
# ModelMetelmann_pis_matrix_EOBS_cycle_plot.R

library(ggplot2)
library(reshape2) 
library(dplyr)
library(pracma)
library(sf)
library(data.table)

folderData = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim"

files = list.files(paste0(folderData,"/"))

namesAll = substring(files, 10, nchar(files)-9)
yearsAll = substring(files, nchar(files)-7, nchar(files)-4) 

# load for 1 dimension
E01 <- readRDS(paste0(folderData, "/", files[1]))

E0m = matrix(NA, ncol = length(E01), nrow = length(files))

for (i in 1:length(files)){
  file = files[i]
  E0v <- readRDS(paste0(folderData, "/", file))
  E0m[i,]= E0v
}

# Compute means per scenario----

##HIST----

name = "Hist"
year = 1996:2005 #:2005

nR <- which((namesAll == name) & (years %in% yearsAll))

E0mHist2000 <-  apply(E0m[nR,], 2,
                        function(x){exp(mean(log(x)))})

## SSP2 RCP 4.5 2055----

name = "ssp245"
years = 2050:2059

nR <- which((namesAll == name) & (years %in% yearsAll))

E0mSSP245_2050 <-  apply(E0m[nR,], 2,
                      function(x){exp(mean(log(x)))})

## SSP2 RCP 4.5 2085----

name = "ssp245"
years = 2080:2089

nR <- which((namesAll == name) & (years %in% yearsAll))

E0mSSP245_2080 <-  apply(E0m[nR,], 2,
                      function(x){exp(mean(log(x)))})

## SSP5 RCP 8.5 2055----

name = "ssp585"
years = 2050:2059

nR <- which((namesAll == name) & (years %in% yearsAll))

E0mSSP585_2050 <-  apply(E0m[nR,], 2,
                      function(x){exp(mean(log(x)))})

## SSP5 RCP 8.5 2085----

name = "ssp585"
years = 2080:2089

nR <- which((namesAll == name) & (years %in% yearsAll))

E0mSSP585_2080 <-  apply(E0m[nR,], 2,
                      function(x){exp(mean(log(x)))})

# plot means per scenario
