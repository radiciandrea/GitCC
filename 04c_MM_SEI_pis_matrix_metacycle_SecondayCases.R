# metacycle to run cycle

rm(list = ls())

IDsSubSet = 1:8981 # put to compute only a subset of cells (8981 in total)

## historical ----

name = "Hs99"
years = 1986:2005 

source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## Central RCP 4.5 short term----

name = "Cn55"
years = 2046:2065

source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## Central RCP 4.5 long term----

name = "Cn70"
years = 2066:2085

source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## High RCP 8.5 short term----

name = "Hg55"
years = 2046:2065

source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")

## High RCP 8.5 long term----

name = "Hg70"
years = 2066:2085

source("04a_MM_SEI_pis_matrix_cycle_SecondayCases.R")