## Load required libraries


library(lubridate)  ## this package helps with manage date fields 
library(dplyr)      ## R Package to use pipelines (%>%) analysis language 
library(tidyverse)
library(sf)         ## R Package for spatial analysis in R ( Simple Features )
library(ggplot2)    ## R Package for plotting and graphs
library( rnaturalearth ) ## Package with earth, country, etc. maps to use for plotting or analysis


# library(vmstools) use when it works (http://nielshintzen.github.io/vmstools/) . Otherwise use code below alternative: 
source('vmstools_functions.R')



outPath = 'C:/Users/RM12/OneDrive - CEFAS/Roi/projects/Welsh_Government_Fishing/welsh_gov_fishing_analysis_capacity/data/data_output'
