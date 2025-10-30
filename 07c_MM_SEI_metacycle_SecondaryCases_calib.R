# code to run the metacycle of the 07bSEI (running on météo france)

library(foreach)
library(dplyr)

rm(list = ls())

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

# years <- 2024
years <- 2019:2024

# https://www.santepubliquefrance.fr/maladies-et-traumatismes/maladies-a-transmission-vectorielle/chikungunya/articles/donnees-en-france-metropolitaine/chikungunya-dengue-et-zika-donnees-de-la-surveillance-renforcee-en-france-hexagonale-2024

dfCities = data.frame(name = c("LA CRAU", "SAINTE CECILE LES VIGNES", "FREJUS", "VALLAURIS"),
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

# expH= 0.9 # to saturate, values between 

dfSim = data.frame(expH = c(seq(0, 0.78, length.out = 40), seq(0.8, 1, length.out = 50), seq(1.05, 2, length.out = 20)),
                       simLaCrau = NA,
                       simSCeclie = NA,
                       simFrejus = NA,
                       simVallauris = NA)

for(j in 1:nrow(dfSim)){
  # foreach(i = 1:nrow(dfCities)) %do% { # dopar
  expH = dfSim$expH[j]
  
  for(i in 1:nrow(dfCities)) { # dopar
    name = dfCities$name[i]
    IDsSubSet = dfCities$cell[i]
    IntroCalendar = dfCities$IntroCalendar[i]
    
    # let's consider: expH
    X0_E0 = (dfCities$X0_E0[i])^expH
    
    source("07b_MM_SEI_SecondaryCases.R")
    plot((max(SH) - SH)*IDsDT$surfHa, main = name)
    lines(rep(dfCities$cases[i], times = length(SH)), col = 'blue')
    
    dfCities$simCases[i] = (max(SH) - min(SH))*IDsDT$surfHa
    
    dfSim[j, i+1] = dfCities$simCases[i]
    
    rm(IDsDT)
  }
}

saveRDS(dfSim, file = paste0(folderOut,"/dfSim.rds"))

### Part 2

dfSim <- readRDS(paste0(folderOut,"/dfSim.rds"))
mSim <- cbind(dfSim %>% pull(simLaCrau),
               dfSim %>% pull(simSCeclie),
               dfSim %>% pull(simFrejus),
               dfSim %>% pull(simVallauris))

mCases <- matrix(dfCities$cases, nrow = nrow(mSim), ncol = nrow(dfCities), byrow = T) # 4 or the number of cases

dfSim$RMSE = sqrt(rowMeans((mSim-mCases)^2))
dfSim$RMSLE = sqrt(rowMeans((log(1+mSim)-log(1+mCases))^2))


# by city
plot(dfSim$expH, dfSim$RMSLE, ylim = c(0,1))
colv = c('darkorange', 'darkgreen', 'darkblue', 'brown')
for(i in 1:nrow(dfCities)) {
  y = sqrt((log(1+mSim[,i])-log(1+mCases[,i]))^2)
  cat(dfCities$name[i], "optim expH value = ", dfSim$expH[which(y == min(y))], "\n")
  lines(dfSim$expH, y, col = colv[i])
  
}

minExpHRMSE = dfSim$expH[which(dfSim$RMSE==min(dfSim$RMSE))]
minExpHRMSLE = dfSim$expH[which(dfSim$RMSLE==min(dfSim$RMSLE))]

#re-simulate

expH = 0.85

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
