# model by Metelmann 2019 to estimate E0 ----
# Running on SAFRAN

# Notes from "Esperimenti/Scenari climatici"

# inspired by ModelMetelmann_pis_matrix_EOBS_cycle

# per scenarios:

# Hist 1996-2005
# SSP 2 2050-2059
# SSP 2 2080-2089
# SSP 5 2050-2059
# SSP 5 2050-2059

rm(list = ls())

library(deSolve)
library(ggplot2)
library(reshape2) 
library(dplyr)
library(suncalc)
library(pracma)
library(sf)

#load T and P

name = "Hist"

years = 1996:2005

# folder names

if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
  folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim"
} else {
  folderDrias = "DRIAS_elab"
  folderOut = "DRIAS_sim"
}

dir.create(folderOut)

# Getting weather from DRIAS
WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", years[1], ".rds")) 

# distinct space
IDsDT <- WTotDT %>% 
  distinct(ID, .keep_all = TRUE) %>%
  dplyr::select(c("ID", "lat", "lon", "pop"))
rm(WTotDT)

IDs = IDsDT$ID
nIDs = length(IDs) 

# lat and lon
LAT = IDsDT$lat
LON = IDsDT$lon

# Model time independent parameters

#parameters (Metelmann 2019)
CTTs = 11 #critical temperature over one week in spring (°C )
CPPs = 11.25 #critical photoperiod in spring
CPPa = 10.058 + 0.08965 * LAT # critical photperiod in autumn
deltaE = 1/7.1 #normal egg development rate (1/day)
lambda = 10^6 # capacity parameter (larvae/day/ha)

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

# System initialization
E0 = rep(0, nIDs)
J0 = rep(0, nIDs)
I0 = rep(0, nIDs)
A0 = rep(0, nIDs)
Ed_0 = 1*rep(1, nIDs) # at 1st of January (10^6)

#integration step
iS = 1/48

tic()
for (year in years){
  
  #getting weather from EOBS <- previous year
  WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", max(year, years[1]), ".rds"))
  
  #Extract only tas in December
  WdDT <- WTotDT %>%
    filter(DOS >= (max(DOS)-30))
  rm(WTotDT)
  
  #Getting weather from DRIAS
  WTotDT <- readRDS(paste0(folderDrias, "/Drias_", name, "_", year, ".rds")) 
  
  #Create a matrix over which integrate; each colums is a city, each row is a date
  DOSy = unique(WTotDT$DOS)
  
  # set simulation horizon
  tS = DOSy[1] 
  tEnd = tail(DOSy, n = 1)
  FoA = max(tEnd)-152 # first of august: last day of diapause hatching
  FoJ = max(tEnd)-183 # first of july: first day of (possible) diapause entrance
  
  date = WTotDT$date
  
  #dimensions
  nD = length(DOSy) # simulation length
  
  #exctract params
  tas = matrix(WTotDT$tas, nrow = nD)
  prec = matrix(WTotDT$pr, nrow = nD)
  tasDJF = rbind(matrix(WdDT$tasMin, nrow = 31),
                   matrix(WTotDT$tasMin[which(WTotDT$DOS <= (max(DOSy)-306))], nrow = (max(DOSy)-306)))
  
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
  
  #elaborate tas and prec + sapply transpose matrices: need to t()
  tas7 = tas[1,]
  tas7 = rbind(tas7, t(sapply(2:nD,
                                  function(x){return(colMeans(tas[max(1,(x-7)):x,]))}))) # tas of precedent 7 days
  tasMinDJF = apply(tasDJF, 2, function(x){min(x)}) #min tas of last winter 
  
  #photoperiod PhP 
  SunTimesDF<- getSunlightTimes(data = data.frame("date" = as.Date(WTotDT$date), "lat"= rep(LAT, nD), "lon" = rep(LON, nD)), keep = c("sunrise", "sunset"))# lat= 44.5, lon = 11.5 about all Emilia Romagna; # lat= 43.7, lon = 7.3 in Nice
  PhP = as.numeric(SunTimesDF$sunset - SunTimesDF$sunrise)
  tSr = as.numeric(SunTimesDF$sunrise- as.POSIXct(SunTimesDF$date) +2) # time of sunrise: correction needed since time is in UTC
  
  PhP = matrix(PhP, nrow = nD, byrow = T)
  tSr = matrix(tSr, nrow = nD, byrow = T)
  
  rm(WTotDT)
  
  #parameters (Metelmann 2019)
  sigma = 0.1 *(tas7 > CTTs)*(PhP > CPPs)*(matrix(rep(DOSy, nIDs), ncol = nIDs) < FoA) # spring hatching rate (1/day) (correction sigma = 0 after august)
  omega = 0.5 *(PhP < CPPa)*(matrix(rep(DOSy, nIDs), ncol = nIDs) > FoJ) # fraction of eggs going into diapause
  muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
  muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI
  
  gamma = 0.93*exp(-0.5*((tasMinDJF -11.68)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?
  
  h = (1-epsRat)*(1+eps0)*exp(-epsVar*(prec-epsOpt)^2)/
    (exp(-epsVar*(prec-epsOpt)^2)+ eps0) +
    epsRat*epsDens/(epsDens + exp(-epsFac*H))
  
  # Compute K
  K = sapply(1:nIDs, function(y){return(lambda * (1-alphaEvap)/(1 - alphaEvap^DOSy)*
                                         sapply(DOSy, function(x){return(sum(alphaEvap^(x:1-1) * (alphaDens*prec[1:x,y] + alphaRain*H[x,y])))}))
  }) 
  
  X0 = c(E0, J0, I0, A0, Ed_0)
  
  source("02b_MM_integration_functions.R")
  
  parms = list(omega = omega,
               h = h,
               K = K,
               muA = muA,
               deltaE = deltaE,
               sigma = sigma,
               gamma = gamma,
               tasMax = tasMax,
               tasMin = tasMin,
               nIDs = nIDs,
               tSr = tSr)
  
  #transform into log +1
  X0log1 = log(X0+1)
  
  # define finer integration grid
  DOSiS = seq(tS, tEnd, by = iS)
  
  #integrate
  SimLog1DOSiS<- deSolve::rk4(X0log1, DOSiS, dfLog1, parms)
  
  # extract values from finer grid
  SimLog1 <-SimLog1DOSiS[1+(0:(tEnd-tS))/iS,]
  
  # untransform variables
  Sim = cbind(SimLog1[,1], exp(SimLog1[, 1+1:(nIDs*5)])-1)
  
  #compute E0
  E0v = pmax(Sim[nrow(Sim), 1+(nIDs*4+1):(nIDs*5)], 0)/Ed_0
  
  save(Sim, file = paste0(folderOut, "/Sim_Drias_", name, "_", year, ".rds"))
  save(E0v, file = paste0(folderOut, "/E0_Drias_", name, "_", year, ".rds"))
  
  toc()
}