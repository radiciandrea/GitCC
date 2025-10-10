# code to run the metacycle of the 07bSEI (running on météo france)

years <- 2019:2024

df_cities = data.frame(name = c("LA CRAU", "SAINTE CECILE LES VIGNES"),
                       dep = c("83", "84"),
                       weather_station = c("HYERES","ORANGE"),
                       cell = c(582, 1858),
                       IntroCalendar  = c("05-08", "19-07")) # month, day

for(i in 1:nrows(df_cities)){
  
  name = df_cities$name[i]
  IDsSubSet = df_cities$cell[i]
  IntroCalendar = df_cities$IntroCalendar[i]
  
  source("07b_MM_SEI_SecondaryCases.R")
}
