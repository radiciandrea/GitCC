# metacycle to run cycle to spin up simulation

rm(list = ls())

IDsSubSet = 1:8981 # put to compute only a subset of cells (8981 in total)

nRep = 5

## climatic model

mod = "" # "" = CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63, cold = MPI-M-MPI-ESM-LR_MPI-CSC-REMO2009, hot = MOHC-HadGEM2-ES_CLMcom-CCLM4-8-17

## historical ----

name = "Hs99"
years = rep(1986, nRep) 

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## Central RCP 4.5 short term----

name = "Cn35"
years = rep(2026, nRep) 

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## Central RCP 4.5 middle term----

name = "Cn55"
years = rep(2046, nRep) 

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## Central RCP 4.5 long term----

name = "Cn70"
years = rep(2066, nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## High RCP 8.5 short term----

name = "Hg35"
years = rep(2026, nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## High RCP 8.5 middle term----

name = "Hg55"
years = rep(2046, nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")

## High RCP 8.5 long term----

name = "Hg70"
years = rep(2066, nRep)

source("03c_MM_pis_matrix_cycle_Abundances_SPINUP.R")