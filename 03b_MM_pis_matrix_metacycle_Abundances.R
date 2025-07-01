# metacycle to run cycle to simulate abundances

rm(list = ls())

IDsSubSet = 1:8981 # put to compute only a subset of cells (8981 in total)

nRep = 5

## HIST----

name = "Hist"
years = 1996:2005

source("03a_MM_pis_matrix_cycle_Abundances.R")

## SSP2 RCP 4.5 2055----

name = "ssp245"
years = 2050:2059

source("03a_MM_pis_matrix_cycle_Abundances.R")

## SSP2 RCP 4.5 2085----

name = "ssp245"
years = 2080:2089

source("03a_MM_pis_matrix_cycle_Abundances.R")

## SSP5 RCP 8.5 2055----

name = "ssp585"
years = 2050:2059

source("03a_MM_pis_matrix_cycle_Abundances.R")

## SSP5 RCP 8.5 2085----

name = "ssp585"
years = 2080:2089

source("03a_MM_pis_matrix_cycle_Abundances.R")