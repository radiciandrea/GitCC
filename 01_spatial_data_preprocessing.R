# Pre-processing of spatial data: weather and population ----

# Notes from "Esperimenti/Scenari climatici"

# inspired by Read_EOBS_cycle.R

# climate projections:
# https://www.drias-climat.fr/drias_prod/accueil/okapiWebDrias/index.jsp?iddrias=climat

# After meeting Cyril Pachka Paul


## Settings ----

## climate model 

# "WRF381P_IPSL-CM5A"

## details in

# https://www.drias-climat.fr/accompagnement/sections/40
# https://www.drias-climat.fr/accompagnement/sections/240


## climate scenarios: 
# reference
# RCP 4.5
# RCP 8.5

## years

# 1996-2005
# 2050-2059
# 2080-2089

## spatial grid:

# safran: https://www.drias-climat.fr/drias_prod/_composantsHTML/simulations/refGeoSimulations/aide_safran_drias2021.html

### Download Saftan Historic 1996-2005 ----
# format: ncdf
# years: 1996-2005
# (8981 points) details in grilleSafran_complete_drias2021
# variables: temperature (max, min, mean), in K, precipitations, in kg/m^2/s
