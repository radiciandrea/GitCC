# Model by Metelmann 2019 ----
# to simulate secondary cases within a month

# Running on SAFRAN

# Notes from "Esperimenti/Scenari climatici"

# inspired by ModelMetelmann_pis_matrix_EOBS_cycle AND MM_PM_EOBS_CC_01_Epidemic.R

# per scenarios:

# Hist 1996-2005
# SSP 2 2050-2059
# SSP 2 2080-2089
# SSP 5 2050-2059
# SSP 5 2050-2059

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
  name = "Hist"
}

if(!exists("years")){
  years = 1996:2005 
}

if(!exists("IDsSubSet")){
  IDsSubSet = 1:8981 # put to compute only a subset of cells (8981 in total)
}

if(!exists("AreaKm2")){
  AreaKm2 = 63.735 # approximate surface of each cell (max rel error: 0.44%)
}

if(!exists("NIntro")){
  NIntro = 1 # number of introduced infected people
}

if(!exists("IntroMonthCalendar")){
  IntroMonthCalendar = 1:12 # imported case: one for each month
}

# folder names

if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
  folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_04"
} else {
  folderDrias = "DRIAS_elab"
  folderOut = "DRIAS_sim_04"
}

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

### Epidemic parameters

# Epidemic parameters 1
bH2v = 0.31 # beta Mtl 2021 (dengue)
bv2H = 0.5 # b Blagrove 2020
phiAU = 0.9 # vector preference (urban)
phiAR = 0.5 # vector preference (rural) #Caminade 2016
RTh = 50 # threshold, in term of people density, to distinguish rural and urban

deltaM = 4.5 #ind/ha max number of mosquito bitten for (goniotrophic cycle)
DHV = 5 # 1/host recovery rate, duration of hte host viremia days (Benkimoun)

# Epidemic sysem initialization
AE0 = rep(0, nIDs) # exposed vectors
AI0 = rep(0, nIDs) # infected vectors

## System initialization ----
X0 = readRDS(file = paste0(folderOut, "/X0_Drias_", name, "_", years[1], ".rds"))

# and select subset:

X0 = c(X0[0*8981+IDsSubSet],
       X0[1*8981+IDsSubSet],
       X0[2*8981+IDsSubSet],
       X0[3*8981+IDsSubSet],
       X0[4*8981+IDsSubSet],
       AE0,
       AI0) # per ha

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
  
  ## Compute epidemic parameters ----
  A = (0.0043*tas + 0.0943)/2 #biting rate
  EIP = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021 (Dengue)
  ni = 1/EIP #of the vector
  phiA = phiAU*(H>RTh)+phiAR*(H<=RTh) #vector preference
  
  #Epidemic scenario
  InfectedHostDensity <- rep(0, nD)
  IntroDates <- yday(as.Date(paste0(year, "-", IntroMonthCalendar, "-01", "%d/%m/%y")))
  InfectedHostDates <- c(t(sapply(-1+1:DHV, function(x){IntroDates+x}))) # repeat for a duration of DHV
  InfectedHostDensity[InfectedHostDates] = NIntro/AreaKm2 #hab/km²
  InfectedHostDensityM = matrix(rep(InfectedHostDensity, nIDs), ncol = nIDs)
  InfectedHostPrevalenceM = InfectedHostDensityM/H
  
  SH0 = H[1,]/100 # susceptible hosts per ha
  X0 = c(X0, SH0) # included in the system
  
  ## Call integration fucntion ----
  source("04b_MM_SEI_integration_functions.R")
  
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
               tSr = tSr,
               A = A,
               phiA = phiA,
               bH2v = bH2v,
               bv2H = bv2H,
               deltaM = deltaM,
               ni = ni,
               IC = IntroDates,
               iCm = InfectedHostPrevalenceM,
               SH0 = SH0)
  
  #transform into log+1 AND giving names
  X0m2 <- X0/10^4 #per m2
  X0log1 = log(X0m2+1)
  
  names(X0log1) =as.character(1:(length(X0)))
  
  ## Set events ----
  
  #set event: zeroing diapausing eggs on FoA
  
  eventZeroEd1 <- data.frame(var = names(X0log1)[(nIDs*4+1):(nIDs*5)], #1+(nIDs*4+1):(nIDs*5)
                             time = FoA,
                             value = 0,
                             method = "rep")
  
  # event: zero mosquito infection. Beware, this way they are simply "killed" and not added to the "S". (They are very few btw)
  eventZeroAE <- data.frame(var = names(X0log1)[(nIDs*5+1):(nIDs*6)], 
                              time = rep(IntroDates, each = nIDs),
                              value = 0,
                              method = "rep")
  
  eventZeroAI <- data.frame(var = names(X0log1)[(nIDs*6+1):(nIDs*7)], 
                            time = rep(IntroDates, each = nIDs),
                            value = 0,
                            method = "rep")
  
  #event reset SH
  eventResetSH <- data.frame(var = names(X0log1)[(nIDs*7+1):(nIDs*8)], 
                            time = rep(IntroDates, each = nIDs),
                            value = log(SH0*10^-4+1) ,
                            method = "rep")
  
  #cbind and sort
  
  eventReset <- rbind(eventZeroEd1, eventZeroAE, eventZeroAI, eventResetSH) %>%
    arrange(time)
  
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
                              func = dfLogSEI, 
                              parms = parms,
                              method = "rk4",
                              events = list(data = eventReset))
  
  # extract values from finer grid
  whichDOSiS = which((DOSiS %% 1)==0)
  SimLog1 <-SimLog1DOSiS[whichDOSiS,]
  
  # untransform variables and transform to ha
  Sim = cbind(SimLog1[,1], 10^4*(exp(SimLog1[, 1+1:(nIDs*8)])-1))
  
  # update X0 (E0 are AT LEAST 1)
  X0 = c(rep(0, 4*nIDs), pmax(Sim[nrow(Sim), 1+(nIDs*4+1):(nIDs*5)], 1), rep(0, 2*nIDs))
  X0[which(is.na(X0))] = 1
  
  AI <- Sim[,1+(nIDs*6+1):(nIDs*7)]
  AE <- Sim[,1+(nIDs*5+1):(nIDs*6)]
  AS <- Sim[,1+(nIDs*3+1):(nIDs*4)]
  Atot <- AS + AE + AI
  
  # first checks, to be removed
  # PrevMosquito = AI/Atot
  # 
  # SH <- Sim[,1+(nIDs*7+1):(nIDs*8)]
  # SecondayCasesContCum = (H - SH*100)*AreaKm2
  # 
  # PrevHost = 100*SecondayCasesContCum/(H*AreaKm2)
  
  ## Save results ----
  saveRDS(Sim, file = paste0(folderOut, "/Sim_Drias_SEIS_", name, "_", year, ".rds"))
  
  cat("UPDATE\nYear:", year, "\n")
  
  toc()
}
