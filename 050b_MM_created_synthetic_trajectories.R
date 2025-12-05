# create fake climate with a sinuisoidal shape

library(data.table)
library(dplyr)
library(deSolve)
library(reshape2) 
library(suncalc)
library(pracma)

mod = ""

if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folderOut = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod,"_sim_050")
  folderData = paste0("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS", mod, "_elab")
} else {
  folderOut = paste0("DRIAS", mod,"_sim_050")
  folderData = paste0("DRIAS", mod, "_elab")
}

dir.create(folderOut)

mintasMinWin = -1
maxtasMinWin = 14

# change: max summer temperature
mintasMaxSum = 13
maxtasMaxSum = 33

# mintasAvgYea = 9
# maxtasAvgYea = 25

step = 1/10

# tasAvgYeaV = seq(mintasAvgYea, maxtasAvgYea, by = step)
tasMaxSumV = seq(mintasMaxSum, maxtasMaxSum, by = step)
tasMinWinV = seq(mintasMinWin, maxtasMinWin, by = step)


#DF with indicators
# IndDT = data.table(ID =NA,
#                    tasAvgYea = rep(tasAvgYeaV, each = length(tasMinWinV)),
#                    tasMinWin = rep(tasMinWinV, times = length(tasAvgYeaV)))

IndDT = data.table(ID =NA,
                   tasMaxSum = rep(tasMaxSumV, each = length(tasMinWinV)),
                   tasMinWin = rep(tasMinWinV, times = length(tasMaxSumV)))

# in this DT there are (few) places hotter in the winter than in the summer: remove impossible climates
# IndDT <- IndDT %>%
#   filter(tasAvgYea > tasMinWin)

# IndDT$ID = paste0(IndDT$tasAvgYea, "_", IndDT$tasMinWin)

IndDT$ID = paste0(IndDT$tasMaxSum, "_", IndDT$tasMinWin)

saveRDS(IndDT, file = paste0(folderOut, "/IndDTMaxMin.rds"))
  
lat = 46
lon = 3
pop = 3500 
surfHa = 6360
rainyDays = 1/5
DOS = 1:365
year = 2025
date = as.Date(paste0(year, "-12-31")) + DOS

# Shape of the average annual mmean, max, min, prec temperatures

mod = ""
nameSc = "Hs99"

scenarios <- c("Hs99")
cities <- c("Montpellier", "Nantes", "Rennes", "Lille", "Paris-est", "Lyon", "Grenoble", "Bordeaux", "Toulouse", "Marseille", "Nice", "Strasbourg", "Clermont-Ferrand")
IDsSubSet <- c(1040, 5243, 6482, 8915, 7542, 3500, 2936, 2472, 929, 642, 1249, 7379, 3564)

filesW= list.files(paste0(folderData,"/"), pattern = paste0("Drias_", nameSc))

tasAvM = matrix(NA, nrow = 365, ncol = length(filesW))
tasMaxAvM = matrix(NA, nrow = 365, ncol = length(filesW))
tasMinAvM = matrix(NA, nrow = 365, ncol = length(filesW))
prAvM = matrix(NA, nrow = 365, ncol = length(filesW))


for(i in 1:length(filesW)){
  
  WTotDT = readRDS(paste0(folderData, "/", filesW[i])) %>% filter(ID %in% IDsSubSet) %>%
    filter(DOS < 366)
  
  nD = nrow(WTotDT)/length(IDsSubSet)
  
  tas = matrix(WTotDT$tas, nrow = nD)
  tasMax = matrix(WTotDT$tasMax, nrow = nD)
  tasMin = matrix(WTotDT$tasMin, nrow = nD)
  pr = matrix(WTotDT$pr, nrow = nD)
  
  tasAv = rowMeans(tas)
  tasMaxAv = rowMeans(tasMax)
  tasMinAv = rowMeans(tasMin)
  prAv = sapply(1:365, function(t){sample(pr[t,], 1)})
  
  tasAvM[,i] = tasAv
  tasMaxAvM[,i] = tasMaxAv
  tasMinAvM[,i] = tasMinAv
  prAvM[,i] = prAv
  
}

tasAv = rowMeans(tasAvM)
tasMaxAv = rowMeans(tasMaxAvM)
tasMinAv = rowMeans(tasMinAvM)
prAv = sapply(1:365, function(t){sample(prAvM[t,], 1)})

tasMaxAvDelta = tasMaxAv - tasAv
tasMinAvDelta = tasMinAv - tasAv

tasNorm = (tasAv-min(tasAv))/(max(tasAv)-min(tasAv))

# Create list

WList <- vector(mode = "list", nrow(IndDT))

for(id in IndDT$ID){
  
  # tasAvgYea = IndDT %>% filter(ID == id) %>% pull(tasAvgYea)
  tasMaxSum = IndDT %>% filter(ID == id) %>% pull(tasMaxSum)
  tasMinWin  = IndDT %>% filter(ID == id) %>% pull(tasMinWin)
  
  # deltaTasYea = 2*(tasAvgYea - tasMinWin)
  
  tas = tasMinWin +(tasMaxSum-tasMinWin)*tasNorm 
  tasMin = tas + tasMinAvDelta
  tasMax = tas + tasMaxAvDelta
  prec = prAv
  
  WDT = data.table(ID = id,
                   lat = lat,
                   lon = lon,
                   pop = pop, 
                   surfHa = surfHa,
                   DOS = DOS,
                   date = date,
                   pr = prec,
                   tas = tas,
                   tasMax = tasMax,
                   tasMin= tasMin)
  
  WList[[id]]<-WDT
}

WTotDT <- data.table::rbindlist(WList)

saveRDS(WTotDT, file = paste0(folderOut, "/WTotDTMaxMin.rds"))

#### common code for suitability and concat simulations ----

IDsSubSet = IndDT$ID # put to compute only a subset of cells (8981 in total)

nIDs = length(IDsSubSet)
IDs = IDsSubSet

IDsDT <- WTotDT %>%
  distinct(ID, .keep_all = TRUE) %>%
  dplyr::select(c("ID", "lat", "lon", "pop", "surfHa")) %>%
  filter(ID %in% IDsSubSet)

# lat and lon
LAT = IDsDT$lat
LON = IDsDT$lon

## Model time-independent parameters 

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

#parameters for modified carrying capacity
lambda = 10^6 # capacity parameter (larvae/day/ha)
expH = 0.85

epsRat = 0.2
eps0 = 1.5
epsVar = 0.05
epsOpt = 8
epsDens = 0.01
epsFac = 0.01

#### run the code to estimate suitability (FROM 02 or 06)

#integration step (should be 1/100)
iS = 1/60

## System initialization 
E0 = rep(0, nIDs)
J0 = rep(0, nIDs)
I0 = rep(0, nIDs)
A0 = rep(0, nIDs)
Ed_0 = 1*rep(1, nIDs) # at 1st of January (10^6)

#### E0 with one year only ----

#Extract only tas in December -getting weather from previous year
WdDT <- WTotDT %>%
  filter(DOS >= (max(DOS)-30)) %>%
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

### Extract weather 
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

# reshape human matrix
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

## Compute parameters 
sigma = 0.1 *(tas7 > CTTs)*(PhP > CPPs)*(matrix(rep(DOSy, nIDs), ncol = nIDs) < FoA) # spring hatching rate (1/day) (correction sigma = 0 after august)
omega = 0.5 *(PhP < CPPa)*(matrix(rep(DOSy, nIDs), ncol = nIDs) > FoJul) # fraction of eggs going into diapause
muA = -log(0.677 * exp(-0.5*((tas-20.9)/13.2)^6)*tas^0.1) # adult mortality rate
muA[which(tas<=0)] = -log(0.677 * exp(-0.5*((tas[which(tas<=0)]-20.9)/13.2)^6))  #correct the problems due to negative values from SI

gamma = 0.93*exp(-0.5*((tasMinDJF -11.68)/15.67)^6) #survival probability of diapausing eggs (1:/inter) #at DOY = 10?

h = (1-epsRat)*(1+eps0)*exp(-epsVar*(prec-epsOpt)^2)/
  (exp(-epsVar*(prec-epsOpt)^2)+ eps0) +
  epsRat*epsDens/(epsDens + exp(-epsFac*H))

# Compute K 
KR = lambda* sapply(1:nIDs, function(y){return((1-alphaEvap)/(1 - alphaEvap^DOSy)*
                                                 sapply(DOSy, function(x){return(sum(alphaEvap^(x:1-1) *alphaDens*prec[1:x,y]))}))
})

KH = lambda*alphaRain*(H^expH)

K = KR+KH

X0 = c(E0, J0, I0, A0, Ed_0)

## Call integration fucntion 
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

#transform into log+1 AND giving names
X0log1 = log(X0+1)

names(X0log1) =as.character(1:(length(X0)))

#set event: zeroing diapausing eggs on FoA

eventZeroEd1 <- data.frame(var = names(X0log1)[(nIDs*4+1):(nIDs*5)], #1+(nIDs*4+1):(nIDs*5)
                           time = FoA,
                           value = 0,
                           method = "rep")

# define finer integration grid
DOSiS = seq(tS, tEnd, by = iS)

## Integration
SimLog1DOSiS<- deSolve::ode(y = X0log1,
                            times = DOSiS,
                            func = dfLog1,
                            parms = parms,
                            method = "rk4",
                            events = list(data = eventZeroEd1))

# extract values from finer grid
SimLog1 <-SimLog1DOSiS[1+(0:(tEnd-tS))/iS,]

# untransform variables
Sim = cbind(SimLog1[,1], exp(SimLog1[, 1+1:(nIDs*5)])-1)

#compute E0
E0v = pmax(Sim[nrow(Sim), 1+(nIDs*4+1):(nIDs*5)], 0)/Ed_0

## Save results

saveRDS(E0v, file = paste0(folderOut, "/020_E0_synthetic_MaxMin.rds"))

cat("UPDATE\nAverage E0:", mean(E0v), "\n")

toc()

# #### Adults, LTS and so on (FROM 04 or 06) ----
# 
# ### Epidemic parameters
# 
# # Epidemic parameters 1
# bH2v = 0.31 # beta Mtl 2021 (dengue)
# bv2H = 0.5 # b Blagrove 2020
# phiAU = 0.9 # vector preference (urban)
# phiAR = 0.5 # vector preference (rural) #Caminade 2016
# RTh = 50 # threshold, in term of people density, to distinguish rural and urban
# 
# deltaM = 4.5 #ind/ha max number of mosquito bitten for (goniotrophic cycle)
# DHV = 5 # 1/host recovery rate, duration of hte host viremia days (Benkimoun)
# 
# # Epidemic sysem initialization
# 
# # E0 = rep(0, nIDs)
# # J0 = rep(0, nIDs)
# # I0 = rep(0, nIDs)
# # A0 = rep(0, nIDs)
# Ed_0 = 10^4*rep(1, nIDs) # at 1st of January (10^6)
# AE0 = rep(0, nIDs) # exposed vectors
# AI0 = rep(0, nIDs) # infected vectors
# 
# NIntro = 1
# IntroCalendar = "01-08" # one intro in August
# 
# ## System initialization
# 
# # and select subset:
# 
# X0 = c(E0,
#        J0,
#        I0,
#        A0,
#        Ed_0,
#        AE0,
#        AI0) # per ha
# 
# #integration step during inactivity period(should be 1/100) (I)
# iSI = 1/4
# #integration step during diapause beginning and ending (D)
# iSD = 1/120
# #integration step during activty period (should be 1/100) (A)
# iSA = 1/72
# 
# fyears= 1:10
# 
# ## Call integration fucntion
# source("04b_MM_SEI_integration_functions.R")
# 
# #reshape area matrix (km²)
# AreaKm2 = matrix(rep(IDsDT$surfHa*10^-2, nD), nrow = nD, byrow = T )
# 
# ## Compute epidemic parameters
# A = (0.0043*tas + 0.0943)/2 #biting rate
# EIP = 1.03*(4*exp(5.15 - 0.123*tas)) #Metelmann 2021 (Dengue)
# ni = 1/EIP #of the vector
# phiA = phiAU*(H>RTh)+phiAR*(H<=RTh) #vector preference
# 
# #Epidemic scenario (as a matrix)
# InfectedHosts <- rep(NIntro, nD)  #hab
# InfectedHostDensityM = matrix(rep(InfectedHosts, nIDs), ncol = nIDs)/AreaKm2
# InfectedHostPrevalenceM = InfectedHostDensityM/H
# 
# IntroDates <- yday(as.Date(paste0(year, "-", IntroCalendar)))
# 
# SH0 = H[1,]/100 # susceptible hosts per ha
# 
# parms = list(omega = omega,
#              h = h,
#              K = K/10^4,
#              muA = muA,
#              deltaE = deltaE,
#              sigma = sigma,
#              gamma = gamma,
#              tasMax = tasMax,
#              tasMin = tasMin,
#              nIDs = nIDs,
#              tSr = tSr,
#              A = A,
#              phiA = phiA,
#              bH2v = bH2v,
#              bv2H = bv2H,
#              deltaM = deltaM,
#              ni = ni,
#              iCm = InfectedHostPrevalenceM,
#              SH0 = SH0)
# 
# # define finer integration grid during diapause haching
# tbDH = which(rowSums(sigma)>0)[1]-1
# tfDH = which(rowSums(sigma)== max(rowSums(sigma)))[1]+1
# 
# # define finer integration grid during diapause enterin
# tbDE = which(rowSums(omega)>0)[1]-1
# tfDE = which(rowSums(omega)== max(rowSums(omega)))[1]+1
# 
# # cbind integration grid
# DOSiS = c(seq(tS, tbDH-iSI, by = iSI),
#           seq(tbDH, tfDH-iSD, by = iSD),
#           seq(tfDH, tbDE-iSA, by = iSA),
#           seq(tbDE, tfDE-iSD, by = iSD),
#           seq(tfDE, tEnd, by = iSI))
# 
# tic()
# for (y in fyears){
# 
#   X0 = c(X0, SH0) # included in the system
# 
#   #transform into log+1 AND giving names
#   X0m2 <- X0/10^4 #per m2
#   X0log1 = log(X0m2+1)
# 
#   names(X0log1) =as.character(1:(length(X0)))
# 
#   ## Integration
#   SimLog1DOSiS<- deSolve::ode(y = X0log1,
#                               times = DOSiS,
#                               func = dfLogSEI,
#                               parms = parms,
#                               method = "rk4",
#                               events = list(data = eventZeroEd1))
# 
#   # extract values from finer grid
#   whichDOSiS = which((DOSiS %% 1)==0)
#   SimLog1 <-SimLog1DOSiS[whichDOSiS,]
# 
#   # untransform variables and transform to ha
#   Sim = cbind(SimLog1[,1], 10^4*(exp(SimLog1[, 1+1:(nIDs*8)])-1))
# 
#   # update X0 ((E0 are AT LEAST 1)) conserve # adults
#   X0 = c(pmax(Sim[nrow(Sim), 1+1:(nIDs*5)], 1), rep(0, 2*nIDs))
#   X0[which(is.na(X0))] = 1
# 
#   AI <- Sim[,1+(nIDs*6+1):(nIDs*7)]
#   AE <- Sim[,1+(nIDs*5+1):(nIDs*6)]
#   AS <- Sim[,1+(nIDs*3+1):(nIDs*4)]
#   Adults <- AS + AE + AI
# 
#   #and SH
#   SH <- Sim[,1+7*nIDs + 1:nIDs]
# 
#   cat("UPDATE\nYear:", y, "\n")
#   toc()
# }
# 
# ## Save results
# saveRDS(Adults, file = paste0(folderOut, "/040e_Adults_SEIS_MaxMin.rds"))
# saveRDS(SH, file = paste0(folderOut, "/040e_SH_SEIS_MaxMin.rds"))

### A0: suitability of a non-diapausing population ----

## System initialization 
# E0 = rep(0, nIDs)
# J0 = rep(0, nIDs)
# I0 = rep(0, nIDs)
A0 = 10*rep(1, nIDs) # at the 1st of July
Ed_0 = rep(0, nIDs) 

# we simply icreate a verlain of all matrices
# xnew <- x[M:end, 1:(M-1)]

#Create a matrix over which integrate; each colums is a city, each row is a date
DOSy = unique(WTotDT$DOS)

#dimensions
nD = length(DOSy) # simulation length

#reload tasMax and tasMin for size purposes
if (any(names(WTotDT)=="tasMax")){
  tasMax <- matrix(WTotDT$tasMax, nrow = nD)
  tasMin <- matrix(WTotDT$tasMin, nrow = nD)
} else {
  cat("T_M and T_m are not available, repaced by T_av")
  tasMax <- tas
  tasMin <- tas
}

### Extract weather 
omegaRev = 0*omega
hRev = h[c(FoJul:nD, 1:(FoJul-1)),, drop = FALSE]
KRev = K[c(FoJul:nD, 1:(FoJul-1)),, drop = FALSE]
muARev = muA[c(FoJul:nD, 1:(FoJul-1)),, drop = FALSE]
deltaERev = deltaE
sigmaRev = 0*sigma
gammaRev = gamma
tasMaxRev = tasMax[c(FoJul:nD, 1:(FoJul-1)),, drop = FALSE]
tasMinRev = tasMin[c(FoJul:nD, 1:(FoJul-1)),, drop = FALSE]

X0 = c(E0, J0, I0, A0, Ed_0)

## Call integration fucntion 
source("02b_MM_integration_functions.R")

parms = list(omega = omegaRev,
             h = hRev,
             K = KRev,
             muA = muARev,
             deltaE = deltaERev,
             sigma = sigmaRev,
             gamma = gammaRev,
             tasMax = tasMaxRev,
             tasMin = tasMinRev,
             nIDs = nIDs,
             tSr = tSr)

#transform into log+1 AND giving names
X0log1 = log(X0+1)

names(X0log1) =as.character(1:(length(X0)))

#set event: zeroing diapausing eggs on FoA

# define finer integration grid
DOSiS = seq(tS, tEnd, by = iS)

## Integration  
SimLog1DOSiS<- deSolve::ode(y = X0log1, 
                            times = DOSiS,
                            func = dfLog1, 
                            parms = parms,
                            method = "rk4")

# extract values from finer grid
SimLog1 <-SimLog1DOSiS[1+(0:(tEnd-tS))/iS,]

# untransform variables
Sim = cbind(SimLog1[,1], exp(SimLog1[, 1+1:(nIDs*5)])-1)

#compute A0
A0v = pmax(Sim[nrow(Sim), 1+(nIDs*3+1):(nIDs*4)], 0)/A0

## Save results 

saveRDS(A0v, file = paste0(folderOut, "/020_A0_synthetic_MaxMin.rds"))

