# code to run the metacycle of the 07bSEI (running on météo france)

# to validate the value of expH of 07c_MM_SEI_metacycle_SecondaryCases.R

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
years <- 2019:2025

dfCities =  data.frame(name = c("ROGNAC", "AUBAGNE"),
                       dep = c("13", "13"),
                       weather_station = c("MARIGNANE", "AUBAGNE"),
                       IntroCalendar  = c("07-05", "08-23"),
                       OutroCalendar  = c("07-27", "09-14"),
                       X0_E0 = c(252000, 87200),
                       cases = c(5, 9),
                       simCases1 = NA, #to record cases
                       simCasesexpH = NA,#to record cases 
                       cell = c("882", "721"),
                       popKm2 = c(706, 869),
                       surfHa = c(1746, 5490))

# year = 2022
# name = "ROGNAC"
# WTotDT <- readRDS(paste0(folderMF, "/", name, "_", year, "_Safran.rds"))
# plot(WTotDT %>% pull(tas), col = 'red')
# mean(WTotDT %>% pull(tas))
# 
# name = "AUBAGNE"
# WTotDT <- readRDS(paste0(folderMF, "/", name, "_", year, "_Safran.rds"))
# points(WTotDT %>% pull(tas), col = 'blue')
# mean(WTotDT %>% pull(tas))


# X0 = readRDS(file = paste0(folderX0, "/X00_Drias_Cn35_2026.rds"))
# dfCities$X0091 = X0[4*8981+as.numeric(dfCities$cell)]

#re-simulate

expH = 0.85

for(i in 1:nrow(dfCities)) { # dopar
  name = dfCities$name[i]
  IDsSubSet = dfCities$cell[i]
  IntroCalendar = dfCities$IntroCalendar[i]
  
  # let's consider: exp_H
  X0_E0 = (dfCities$X0_E0[i])^expH
  
  source("07b_MM_SEI_SecondaryCases.R")
  plot((max(SH) - SH)*IDsDT$surfHa, main = paste0(name, ", expH = ", expH))
  lines(rep(dfCities$cases[i], times = length(SH)), col = 'blue')
  
  dfCities$simCasesexpH[i] = (max(SH) - min(SH))*IDsDT$surfHa
}


# compute error
cat("Error with expH =", expH, "\n")
sqrt(mean((dfCities$simCasesexpH-dfCities$cases)^2))
sqrt(mean((log(1+dfCities$simCasesexpH)-log(1+dfCities$cases))^2))

plot(dfCities$cases, dfCities$simCasesexpH, xlim = c(0,60), ylim = c(0,60))
lines(0:60, 0:60)

#re-simulate
X0 = readRDS(file = paste0(folderX0, "/X0_Drias_Cn35_2026.rds"))

dfCities$X01 = X0[4*8981+as.numeric(dfCities$cell)]

expH = 1

for(i in 1:nrow(dfCities)) { # dopar
  name = dfCities$name[i]
  IDsSubSet = dfCities$cell[i]
  IntroCalendar = dfCities$IntroCalendar[i]
  
  # let's consider: exp_H
  X0_E0 = (dfCities$X0_E0[i])^expH
  
  source("07b_MM_SEI_SecondaryCases.R")
  plot((max(SH) - SH)*IDsDT$surfHa, main = paste0(name, ", expH = ", expH))
  lines(rep(dfCities$cases[i], times = length(SH)), col = 'blue')
  
  dfCities$simCases1[i] = (max(SH) - min(SH))*IDsDT$surfHa
}

# compute error
cat("Error with expH =", expH, "\n")
sqrt(mean((dfCities$simCases1-dfCities$cases)^2))
sqrt(mean((log(1+dfCities$simCases1)-log(1+dfCities$cases))^2))


points(dfCities$cases, dfCities$simCases1, xlim = c(0,60), ylim = c(0,60), col = 'red')
