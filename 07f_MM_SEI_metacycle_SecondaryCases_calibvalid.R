# code to run the metacycle of the 07bSEI (running on météo france)

library(foreach)
library(dplyr)

rm(list = ls())


#### folders ----
# folder names
if(!exists("folderOut")){
  if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
    folderMF = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance_elab"
    folderX0 = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim"
    folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance_sim_07b"
  } else {
    folderMF = "MeteoFrance_elab"
    folderX0 = "DRIAS_sim"
    folderOut = "MeteoFrance_sim_07b"
  }
}

# years <- 2024----
years <- 2019:2024

# https://www.santepubliquefrance.fr/maladies-et-traumatismes/maladies-a-transmission-vectorielle/chikungunya/articles/donnees-en-france-metropolitaine/chikungunya-dengue-et-zika-donnees-de-la-surveillance-renforcee-en-france-hexagonale-2024

dfCities2024 = data.frame(name = c("LA CRAU", "SAINTE CECILE LES VIGNES", "FREJUS", "VALLAURIS"),
                      dep = c("83", "84", "83", "06"),
                      weather_station = c("HYERES","ORANGE", "FREJUS", "ANTIBES-GOLF"),
                      IntroCalendar  = c("08-05", "07-19", "08-08", "08-14"), # month, day
                      OutroCalendar  = c("09-05", "08-25", "10-11", "09-27"), # month, day
                      X0_E0 = c(191000, 21000, 156000, 1898000),
                      cases = c(25, 18, 15, 14), 
                      simCases = NA,
                      cell = c(582, 1858, 897, 1072),
                      popKm2 = c(513, 133, 572, 2163),
                      surfHa = c(3787, 1982, 10227, 1304))

dfSim2024 = data.frame(expH = seq(0.01, 1, by = 0.01),
                   simLaCrau = NA,
                   simSCecile = NA,
                   simFrejus = NA,
                   simVallauris = NA)

#### cycle ----

for(j in 1:nrow(dfSim2024)){
  # foreach(i = 1:nrow(dfCities2024)) %do% { # dopar
  expH = dfSim2024$expH[j]
  
  for(i in 1:nrow(dfCities2024)) { # dopar
    name = dfCities2024$name[i]
    IDsSubSet = dfCities2024$cell[i]
    IntroCalendar = dfCities2024$IntroCalendar[i] # this will be anticipated by 10 days
    OutroCalendar = dfCities2024$OutroCalendar[i]
    
    # let's consider: expH
    X0_E0 = (dfCities2024$X0_E0[i])^expH
    
    source("07b_MM_SEI_SecondaryCases.R")
    # plot((max(SH) - SH)*IDsDT$surfHa, main = name)
    # lines(rep(dfCities2024$cases[i], times = length(SH)), col = 'blue')
    
    dfCities2024$simCases[i] = (max(SH) - min(SH))*IDsDT$surfHa
    
    dfSim2024[j, i+1] = dfCities2024$simCases[i]
    
    rm(IDsDT)
  }
}

# years <- 2025----
years <- 2019:2025

dfCities2025 =  data.frame(name = c("ROGNAC", "AUBAGNE"),
                           dep = c("13", "13"),
                           weather_station = c("MARIGNANE", "AUBAGNE"),
                           IntroCalendar  = c("07-05", "08-23"),  # this will be anticipated by 10 days
                           OutroCalendar  = c("07-27", "09-14"),
                           X0_E0 = c(252000, 87200),
                           cases = c(5, 9),
                           simCases = NA,#to record cases 
                           cell = c(882, 721),
                           popKm2 = c(706, 869),
                           surfHa = c(1746, 5490))


dfSim2025 = data.frame(expH = seq(0.01, 1, by = 0.01),
                       simRognac = NA,
                       simAubagne = NA)

#### cycle ----

for(j in 1:nrow(dfSim2025)){
  # foreach(i = 1:nrow(dfCities2025)) %do% { # dopar
  expH = dfSim2025$expH[j]
  
  for(i in 1:nrow(dfCities2025)) { # dopar
    name = dfCities2025$name[i]
    IDsSubSet = dfCities2025$cell[i]
    IntroCalendar = dfCities2025$IntroCalendar[i]
    OutroCalendar = dfCities2025$OutroCalendar[i]
    
    # let's consider: expH
    X0_E0 = (dfCities2025$X0_E0[i])^expH
    
    source("07b_MM_SEI_SecondaryCases.R")
    # plot((max(SH) - SH)*IDsDT$surfHa, main = name)
    # lines(rep(dfCities2025$cases[i], times = length(SH)), col = 'blue')
    
    dfCities2025$simCases[i] = (max(SH) - min(SH))*IDsDT$surfHa
    
    dfSim2025[j, i+1] = dfCities2025$simCases[i]
    
    rm(IDsDT)
  }
}

dfSim = left_join(dfSim2024, dfSim2025)
dfCities = rbind(dfCities2024, dfCities2025)

#### save and load ----

saveRDS(dfSim, file = paste0(folderOut,"/dfSim2024_5.rds"))

### Part 2

dfSim <- readRDS(paste0(folderOut,"/dfSim2024_5.rds"))

# as matrix
mSim <- cbind(dfSim %>% pull(simLaCrau),
              dfSim %>% pull(simSCecile),
              dfSim %>% pull(simFrejus),
              dfSim %>% pull(simVallauris),
              dfSim %>% pull(simRognac),
              dfSim %>% pull(simAubagne))

mCases <- matrix(dfCities$cases, nrow = nrow(mSim), ncol = nrow(dfCities), byrow = T) # 4 or the number of cases

dfSim$RMSE = sqrt(rowMeans((mSim-mCases)^2))
dfSim$RMSLE = sqrt(rowMeans((log(1+mSim)-log(1+mCases))^2))


# by city
plot(dfSim$expH, dfSim$RMSLE, ylim = c(0,3))
colv = c('darkorange', 'darkgreen', 'darkblue', 'brown', 'purple', 'grey')
for(i in 1:nrow(dfCities)) {
  y = sqrt((log(1+mSim[,i])-log(1+mCases[,i]))^2)
  cat(dfCities$name[i], "optim expH value = ", dfSim$expH[which(y == min(y))], "\n")
  lines(dfSim$expH, y, col = colv[i])
  
}

minExpHRMSE = dfSim$expH[which(dfSim$RMSE==min(dfSim$RMSE))]
minExpHRMSLE = dfSim$expH[which(dfSim$RMSLE==min(dfSim$RMSLE))]


# Non-parametric bootstrap to estimate uncertainty

repB = 1000000
nSites = 4

minExpHRMSLEbootstrap = rep(NA, repB)

for(i in 1: repB){
  
  sitesB = sample(1:nSites, nSites, replace = T)
  mSimB = mSim[, sitesB]
  mCasesB = mCases[, sitesB]
  
  RMSLEB = sqrt(rowMeans((log(1+mSimB)-log(1+mCasesB))^2))
  
  minExpHRMSLEbootstrap[i] = dfSim$expH[which(RMSLEB==min(RMSLEB))]
}

summary(minExpHRMSLEbootstrap)


#re-simulate

expH = 0.46 #0.6 or #0.52

plot(mCases[1,], dfSim[(dfSim$expH == minExpHRMSLE),1 +1:4], xlim = c(0,30), ylim = c(0,30))
lines(0:30, 0:30)

for(i in 1:nrow(dfCities)) { # dopar
  name = dfCities$name[i]
  IDsSubSet = dfCities$cell[i]
  IntroCalendar = dfCities$IntroCalendar[i]
  
  # let's consider: expH
  X0_E0 = (dfCities$X0_E0[i])^expH
  
  source("07b_MM_SEI_SecondaryCases.R")
  plot((max(SH) - SH)*IDsDT$surfHa, main = name)
  lines(rep(dfCities$cases[i], times = length(SH)), col = 'blue')
}

# save comparison df

c(dfSim[which(dfSim$expH == minExpHRMSLE),1 +1:4])

dfCases =dfCities%>%
  select(c("name", "dep", "IntroCalendar", "OutroCalendar", "cases")) %>%
  mutate(casesSimOldK = as.numeric(c(dfSim[which(dfSim$expH == 1),1 +1:4]))) %>%
  mutate(casesSimNewK = as.numeric(c(dfSim[which(dfSim$expH == minExpHRMSLE),1 +1:4]))) %>%
  mutate(set = "calib")

saveRDS(dfCases, file = paste0(folderOut,"/dfCasesCalib2024_5.rds"))


ggplot() +
  geom_point(data = dfCases, aes(x = cases, y = casesSimNewK, shape = name, size = ENewK), color = 'lightblue') +
  geom_point(data = dfCases, aes(x = cases, y = casesSimOldK, shape = name, size = EOldK), color = 'lightblue')

