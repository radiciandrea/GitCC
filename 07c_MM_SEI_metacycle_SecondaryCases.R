# code to run the metacycle of the 07bSEI (running on météo france)

library(foreach)

rm(list = ls())

years <- 2024
years <- 2019:2024

df_cities = data.frame(name = c("LA CRAU", "SAINTE CECILE LES VIGNES"),
                       dep = c("83", "84"),
                       weather_station = c("HYERES","ORANGE"),
                       cell = c(582, 1858),
                       IntroCalendar  = c("08-05", "07-19"),
                       OutroCalendar  = c("09-05", "08-25"),
                       X0_E0 = sqrt(c(191000, 21000))) # month, day

foreach(i = 1:nrow(df_cities)) %do% { # dopar
  
  name = df_cities$name[i]
  IDsSubSet = df_cities$cell[i]
  IntroCalendar = df_cities$IntroCalendar[i]
  # X0_E0 = df_cities$X0_E0[i]
  
  source("07b_MM_SEI_SecondaryCases.R")
  plot((max(SH) - SH)*IDsDT$surfHa, main = name)
  
}
