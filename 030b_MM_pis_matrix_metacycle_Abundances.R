# metacycle to run cycle to simulate abundances

rm(list = ls())

IDsSubSet = 1:8981 # put to compute only a subset of cells (8981 in total)

## historical ----

name = "Hs99"
years = 1986:2005 

source("030a_MM_pis_matrix_cycle_Abundances.R")

## Central RCP 4.5 middle term----

name = "Cn55"
years = 2046:2065

source("030a_MM_pis_matrix_cycle_Abundances.R")

## Central RCP 4.5 long term----

name = "Cn70"
years = 2066:2085

source("030a_MM_pis_matrix_cycle_Abundances.R")


## High RCP 8.5 middle term----

name = "Hg55"
years = 2046:2065

source("030a_MM_pis_matrix_cycle_Abundances.R")

## High RCP 8.5 long term----

name = "Hg70"
years = 2066:2085

source("030a_MM_pis_matrix_cycle_Abundances.R")