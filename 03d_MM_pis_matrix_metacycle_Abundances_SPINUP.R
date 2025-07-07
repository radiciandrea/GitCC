# metacycle to run cycle to spin up simulation

rm(list = ls())

IDsSubSet = 1:8981 # put to compute only a subset of cells (8981 in total)

nRep = 5

## HIST----

name = "Hist"
years = rep(1996, times = nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## SSP2 RCP 4.5 2055----

name = "ssp245"
years = rep(2050, times = nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## SSP2 RCP 4.5 2085----

name = "ssp245"
years = rep(2080, times = nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## SSP5 RCP 8.5 2055----

name = "ssp585"
years = rep(2050, times = nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## SSP5 RCP 8.5 2085----

name = "ssp585"
years = rep(2080, times = nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")