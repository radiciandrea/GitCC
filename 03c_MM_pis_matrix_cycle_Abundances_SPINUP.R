# Model by Metelmann 2019 ----
# to spinup abundance

# Running on SAFRAN

# Notes from "Esperimenti/Scenari climatici"

# inspired by ModelMetelmann_pis_matrix_EOBS_cycle

# per scenarios:

# Hist 1986-2005
# central + rcp 4.5 2045-2065
# central + rcp  4.5 2066-2085
# high + rcp  8.5 2045-2065
# high + rcp 8.5 2066-2085

# rm(list = ls())

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)
library(pracma)
library(sf)
library(data.table)

## Simulation settings ----

if(!exists("name")){
  name = "Hs99"
}

if(!exists("years")){
  years = 1986:2005 
}

if(!exists("IDsSubSet")){
  IDsSubSet = 1:8981 # put to compute only a subset of cells (8981 in total)
}

# folder names

if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
  folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_03"
} else {
  folderDrias = "DRIAS_elab"
  folderOut = "DRIAS_sim_03"
}

dir.create(folderOut)

# get ID, lat, lon
IDsDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", years[1], ".rds")) %>%
  distinct(ID, .keep_all = TRUE) %>%
  dplyr::select(c("ID", "lat", "lon", "pop")) %>%
  filter(ID %in% IDsSubSet)

nIDs = length(IDsSubSet)
IDs = IDsSubSet

# lat and lon
LAT = IDsDT$lat
LON = IDsDT$lon

## Model time-independent parameters ----

#parameters (Metelmann 2019)
CTTs = 11 #critical temperature over one week in spring (°C )
CPPs = 11.25 #critical photoperiod in spring
CPPa = 10.058 + 0.08965 * LAT # critical photperiod in autumn
deltaE = 1/7.1 #normal egg development rate (1/day)
# lambda = 10^6 # capacity parameter (larvae/day/ha)
lambdaM2 = 10^2 # capacity parameter (larvae/day/m2)

# advanced parameter for carrying capacity
alphaEvap = 0.9
alphaDens = 0.001
alphaRain = 0.00001

epsRat = 0.2
eps0 = 1.5
epsVar = 0.05
epsOpt = 8
epsDens = 0.01
epsFac = 0.01

## System initialization ----
E0 = rep(0, nIDs)
J0 = rep(0, nIDs)
I0 = rep(0, nIDs)
A0 = rep(0, nIDs)
Ed_0 = 10^3*rep(1, nIDs) # at 1st of January (10^6)

X0 = c(E0, J0, I0, A0, Ed_0) #per ha

#integration step during inactivity period(should be 1/100) (I)
iSI = 1/4
#integration step during diapause beginning and ending (D)
iSD = 1/120
#integration step during activty period (should be 1/100) (A)
iSA = 1/72

tic()
for (year in years){
  
  #Extract only tas in December -getting weather from previous year
  WdDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", max(year, years[1]), ".rds")) %>%
    filter(DOS >= (max(DOS)-30)) %>%
    filter(ID %in% IDsSubSet)
  
  #Getting weather from DRIAS
  WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) %>%
    filter(ID %in% IDsSubSet) 
  
  #Create a matrix over which integrate; each colums is a city, each row is a date
  DOSy = unique(WTotDT$DOS)
  
  # set simulation horizon
  tS = DOSy[1] 
  tEnd = tail(DOSy, n = 1)
  FoA = tEnd-152 # first of august: last day of diapause hatching
  FoJul = tEnd-183 # first of july: first day of (possible) diapause entrance
  FoM = tEnd-306
  
  date = WTotDT$date
  
  #dimensions
  nD = length(DOSy) # simulation length
  
  ### Extract weather ----
  tas = matrix(WTotDT$tas, nrow = nD)
  prec = matrix(WTotDT$pr, nrow = nD)
  tasDJF = rbind(matrix(WdDT$tasMin, nrow = 31),
                 matrix(WTotDT$tasMin[which(WTotDT$DOS <= FoM)], nrow = FoM))
  
  if (any(names(WTotDT)=="tasMax")){
    tasMax <- matrix(WTotDT$tasMax, nrow = nD)
    tasMin <- matrix(WTotDT$tasMin, nrow = nD)
  } else {
    cat("T_M and T_m are not available, repaced by T_av")
    tasMax <- tas
    tasMin <- tas
  }
  
  #reshape human matrix
  H =   matrix(rep(IDsDT$pop, nD), nrow = nD, byrow = T ) 
  # humans are corrected (from density expressed as h/m² to h/km²)
  H = H*10^6
  
  #elaborate tas and prec + sapply transpose matrices: need to t()
  tas7 = tas[1,]
  
  if(nIDs > 1){
    tas7 = rbind(tas7, t(sapply(2:nD,
                                function(x){return(colMeans(tas[max(1,(x-7)):x,]))}))) # tas of precedent 7 days
  } else {
    tas7 = c(tas7, sapply(2:nD,
                          function(x){mean(tas[max(1,(x-7)):x,])})) # tas of precedent 7 days
  }
  
  tasMinDJF = apply(tasDJF, 2, function(x){min(x)}) #min tas of last winter 
  
  # photoperiod PhP 
  SunTimesDF<- getSunlightTimes(data = data.frame("date" = as.Date(WTotDT$date), "lat"= rep(LAT, nD), "lon" = rep(LON, nD)), keep = c("sunrise", "sunset"))# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
  PhP = as.numeric(SunTimesDF$sunset - SunTimesDF$sunrise)
  tSr = as.numeric(SunTimesDF$sunrise- as.POSIXct(SunTimesDF$date) +2) # time of sunrise: correction needed since time is in UTC
  
  PhP = matrix(PhP, nrow = nD, byrow = F)
  tSr = matrix(tSr, nrow = nD, byrow = F)
  
  rm(WTotDT)
  
  ## Compute parameters ----
  sigma = 0.1 *(tas7 > CTTs)*(PhP > CPPs)*(matrix(rep(DOSy, nIDs), ncol = nIDs) < FoA) # spring hatching rate (1/day) (correction sigma = 0 after august)
  omega = 0.5 *(PhP < CPPa)*(matrix(rep(DOSy, nIDs), ncol = nIDs) > FoJul) # fraction of eggs going into diapause
  muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
  muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  gamma = 0.93*exp(-0.5*((tasMinDJF -11.68)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?
  
  h = (1-epsRat)*(1+eps0)*exp(-epsVar*(prec-epsOpt)^2)/
    (exp(-epsVar*(prec-epsOpt)^2)+ eps0) +
    epsRat*epsDens/(epsDens + exp(-epsFac*H))
  
  # Compute K per m2
  KM2 = sapply(1:nIDs, function(y){return(lambdaM2 * (1-alphaEvap)/(1 - alphaEvap^DOSy)*
                                            sapply(DOSy, function(x){return(sum(alphaEvap^(x:1-1) * (alphaDens*prec[1:x,y] + alphaRain*H[x,y])))}))
  }) 
  
  ## Call integration fucntion ----
  source("02b_MM_integration_functions.R") #02b2_MM_integration_functions_withcheck
  
  parms = list(omega = omega,
               h = h,
               K = KM2,
               muA = muA,
               deltaE = deltaE,
               sigma = sigma,
               gamma = gamma,
               tasMax = tasMax,
               tasMin = tasMin,
               nIDs = nIDs,
               tSr = tSr)
  
  #transform into log+1 AND giving names
  X0m2 <- X0/10^4 #per m2
  X0log1 = log(X0m2+1)
  
  names(X0log1) =as.character(1:(length(X0)))
  
  #set event: zeroing diapausing eggs on FoA
  
  eventZeroEd1 <- data.frame(var = names(X0log1)[(nIDs*4+1):(nIDs*5)], #1+(nIDs*4+1):(nIDs*5)
                             time = FoA,
                             value = 0,
                             method = "rep")
  
  # define finer integration grid during diapause haching
  tbDH = which(rowSums(sigma)>0)[1]-1
  tfDH = which(rowSums(sigma)== max(rowSums(sigma)))[1]+1
  
  # define finer integration grid during diapause enterin
  tbDE = which(rowSums(omega)>0)[1]-1
  tfDE = which(rowSums(omega)== max(rowSums(omega)))[1]+1
  
  # cbind integration grid
  DOSiS = c(seq(tS, tbDH-iSI, by = iSI),
            seq(tbDH, tfDH-iSD, by = iSD),
            seq(tfDH, tbDE-iSA, by = iSA),
            seq(tbDE, tfDE-iSD, by = iSD),
            seq(tfDE, tEnd, by = iSI))
  
  ## Integration  ----
  SimLog1DOSiS<- deSolve::ode(y = X0log1, 
                              times = DOSiS,
                              func = dfLog1, 
                              parms = parms,
                              method = "rk4",
                              events = list(data = eventZeroEd1))
  
  # extract values from finer grid
  whichDOSiS = which((DOSiS %% 1)==0)
  SimLog1 <-SimLog1DOSiS[whichDOSiS,]
  
  # untransform variables and transform to ha
  Sim = cbind(SimLog1[,1], 10^4*(exp(SimLog1[, 1+1:(nIDs*5)])-1))
  
  # update X0 (E0 are AT LEAST 1)
  X0 = c(rep(0, 4*nIDs), pmax(Sim[nrow(Sim), 1+(nIDs*4+1):(nIDs*5)], 1))
  X0[which(is.nan(X0))] = 1
  cat("UPDATE\nYear:", year, "\n")
  
  toc()
}

## Save results ----
saveRDS(X0, file = paste0(folderOut, "/X0_Drias_", name, "_", year, ".rds"))
