# code to run the metacycle of the 07bSEI (running on météo france)

library(foreach)

rm(list = ls())

years <- 2024
# years <- 2019:2024

# https://www.santepubliquefrance.fr/maladies-et-traumatismes/maladies-a-transmission-vectorielle/chikungunya/articles/donnees-en-france-metropolitaine/chikungunya-dengue-et-zika-donnees-de-la-surveillance-renforcee-en-france-hexagonale-2024

df_cities = data.frame(name = c("LA CRAU", "SAINTE CECILE LES VIGNES", "FREJUS", "VALLAURIS"),
                       dep = c("83", "84", "83", "06"),
                       weather_station = c("HYERES","ORANGE", "FREJUS", "ANTIBES-GOLF"),
                       IntroCalendar  = c("08-05", "07-19", "08-08", "08-14"),
                       OutroCalendar  = c("09-05", "08-25", "10-11", "09-27"),
                       X0_E0 = c(191000, 21000, 156000, 1898000),# month, day
                       cases = c(25, 18, 15, 14), 
                       simCases = NA,
                       cell = c(582, 1858, 897, 1072),
                       popKm2 = c(513, 133, 572, 2163),
                       surfHa = c(3787, 1982, 10227, 1304))

foreach(i = 1:nrow(df_cities)) %do% { # dopar
  
  name = df_cities$name[i]
  IDsSubSet = df_cities$cell[i]
  IntroCalendar = df_cities$IntroCalendar[i]
  # X0_E0 = df_cities$X0_E0[i]
  
  source("07b_MM_SEI_SecondaryCases.R")
  plot((max(SH) - SH)*IDsDT$surfHa, main = name)
  
  df_cities$simCases[i] = (max(SH) - min(SH))*IDsDT$surfHa
  
}


(df_cities$simCases - df_cities$cases)/df_cities$cases
