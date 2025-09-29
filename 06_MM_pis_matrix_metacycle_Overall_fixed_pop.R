# repeat codes 02 and 04, but with fixed population to 1999

rm(list = ls())

IDsSubSet = 1

if (file.exists("C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Codice/local.R")){
  folderOut = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_sim_06"
  folderDrias = "C:/Users/2024ar003/Desktop/Alcuni file permanenti/Post_doc/Dati/DRIAS_elab"
} else {
  folderOut = "DRIAS_sim_06"
  folderDrias = "/DRIAS_elab"
}

# get ID, lat, lon
IDsDT <- readRDS(paste0(folderDrias, "/Drias_Hs99_1986.rds")) %>%
  distinct(ID, .keep_all = TRUE) %>%
  dplyr::select(c("ID", "lat", "lon", "pop")) 


## Central RCP 4.5 short term----

name = "Cn35"
years = 2026:2045

source("02_MM_pis_matrix_cycle_E0.R")
source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## Central RCP 4.5 middle term----

name = "Cn55"
years = 2046:2065

source("02_MM_pis_matrix_cycle_E0.R")
source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## Central RCP 4.5 long term----

name = "Cn70"
years = 2066:2085

source("02_MM_pis_matrix_cycle_E0.R")
source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## High RCP 8.5 short term----

name = "Hg35"
years = 2026:2045

source("02_MM_pis_matrix_cycle_E0.R")
source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## High RCP 8.5 middle term----

name = "Hg55"
years = 2046:2065

source("02_MM_pis_matrix_cycle_E0.R")
source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## High RCP 8.5 long term----

name = "Hg70"
years = 2066:2085

source("02_MM_pis_matrix_cycle_E0.R")
source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")