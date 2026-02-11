# code to run the metacycle of the 07bSEI (running on météo france)

# to validate the value of expH of 07c_MM_SEI_metacycle_SecondaryCases.R

library(foreach)
library(dplyr)
library(reshape2)

rm(list = ls())

# folder names
if(!exists("folderOut")){
  if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
    folderMF = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/MeteoFrance_elab"
    folderX0 = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim"
  } else {
    folderMF = "MeteoFrance_elab"
    folderX0 = "DRIAS_sim"
  }
}

expH = 0.6


# 2024----

years2024 <- 2019:2024

dfCities2024 = data.frame(name = c("LA CRAU", "SAINTE CECILE LES VIGNES", "FREJUS", "VALLAURIS"),
                      dep = c("83", "84", "83", "06"),
                      weather_station = c("HYERES","ORANGE", "FREJUS", "ANTIBES-GOLF"),
                      IntroCalendar  = c("08-05", "07-19", "08-08", "08-14"), # month, day
                      OutroCalendar  = c("12-31", "12-31", "12-31", "12-31"), # month, day
                      X0_E0 = c(191000, 21000, 156000, 1898000),
                      cases = c(25, 18, 15, 14), 
                      cell = c(582, 1858, 897, 1072),
                      popKm2 = c(513, 133, 572, 2163),
                      surfHa = c(3787, 1982, 10227, 1304))


for(i in 1:nrow(dfCities2024)) { # dopar
  name = dfCities2024$name[i]
  IDsSubSet = dfCities2024$cell[i]
  IntroCalendar = dfCities2024$IntroCalendar[i]
  OutroCalendar = dfCities2024$OutroCalendar[i]
  
  # let's consider: exp_H
  X0_E0 = (dfCities2024$X0_E0[i])^expH
  
  source("07b_MM_SEI_SecondaryCases.R")
  plot((max(SH) - SH)*IDsDT$surfHa, main = paste0(name, ", expH = ", expH))
  lines(rep(dfCities2024$cases[i], times = length(SH)), col = 'blue')
  
  dfCities2024$simCasesexpH[i] = (max(SH) - min(SH))*IDsDT$surfHa
}

# 2025----

years2025 <- 2019:2025

dfCities2025 =  data.frame(name = c("ROGNAC", "AUBAGNE"),
                       dep = c("13", "13"),
                       weather_station = c("MARIGNANE", "AUBAGNE"),
                       IntroCalendar  = c("07-05", "08-23"),
                       OutroCalendar  = c("12-31", "12-31"),
                       X0_E0 = c(252000, 87200),
                       cases = c(5, 9),
                       simCasesexpH = NA,#to record cases 
                       cell = c("882", "721"),
                       popKm2 = c(706, 869),
                       surfHa = c(1746, 5490))


for(i in 1:nrow(dfCities2025)) { # dopar
  name = dfCities2025$name[i]
  IDsSubSet = dfCities2025$cell[i]
  IntroCalendar = dfCities2025$IntroCalendar[i]
  OutroCalendar = dfCities2025$OutroCalendar[i]
  
  # let's consider: exp_H
  X0_E0 = (dfCities2025$X0_E0[i])^expH
  
  source("07b_MM_SEI_SecondaryCases.R")
  plot((max(SH) - SH)*IDsDT$surfHa, main = paste0(name, ", expH = ", expH))
  lines(rep(dfCities2025$cases[i], times = length(SH)), col = 'blue')
  
  dfCities2025$simCasesexpH[i] = (max(SH) - min(SH))*IDsDT$surfHa
}

dfCities<- rbind(dfCities2024, dfCities2025)


