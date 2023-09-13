############################# Load Extracted EFALO and TACSAT data sets into R Environment ########################

## This script follows up from the EFLALO and TACSAT extraction SQL script. The EFLALO and TACSAT must be available in analysts computers with the following names : 

# -   eflalo_ft.csv
# -   eflalo_le.csv
# -   eflalo_spe.csv
# -   tacsat.csv


## Load required libraries

library(lubridate)  ## this package helps with manage date fields 
library(dplyr)      ## R Package to use pipelines (%>%) analysis language 
library(sf)         ## R Package for spatial analysis in R ( Simple Features )
library(ggplot2)    ## R Package for plotting and graphs

## SET YOUR R WORKING DIRECTORY

  ##check WD is in the desired data folder location
  getwd() 
  
  ## otherwise change to desired data folder location
  setwd('./../../data')



 ## SELECT THE ANALYSIS OPTION:
  
  analysis_type = 'welsh_waters' ## Options: ( 'welsh_fleet' , 'welsh_waters'  ) 
  
  if ( analysis_type == 'welsh_fleet')  { 

    ###  1. WELSH FLEET ACTIVITY ANALYSIS 
    
    data_folder_t3 = file.path( paste0 ( getwd() , '/welsh_fleet_data/t3' )  )
    data_folder_geofish = file.path( paste0 ( getwd() , '/welsh_fleet_data/geofish' ) ) 

  } else if  (analysis_type == 'welsh_waters' ) { 


  ## 2. WELSH WATERS ACTIVITY ANALYSIS 
  
  
  data_folder_t3 = file.path( paste0 ( getwd() , '/welsh_waters_data/t3' )  )
  data_folder_geofish = file.path( paste0 ( getwd() , '/welsh_waters_data/geofish' ))
  
  } 
  
  


## 1. Load T3/GeoFISH  EFLALO data set blocks and merge them into one R data frame. ----


## list.files(path = '.\\..\\data') # check the files in your directory 
           

eflalo_ft_t3 = read.csv(file = paste0(data_folder_t3, '\\eflalo_ft.csv') , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
eflalo_ft_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_ft.csv') , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(eflalo_ft_gf) = toupper( names(eflalo_ft_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 

 
## Convert the field VE_COU in a standard country acronym 
countries_id = data.frame( ve_fa =   c('Wales', 'England', 'Isle of Man', 'NULL', 'Scotland', 'North Ireland') , ve_cou_gb = c( 'GBW', 'GBE', 'GBI','NULL', 'GBS', 'GBN' ) ) 
eflalo_ft_t3 = eflalo_ft_t3 %>% left_join(countries_id, by =  c ( 'VE_FA' = 've_fa')) %>% mutate(VE_COU = ve_cou_gb) %>% select ( - ve_cou_gb)
 
## Filter GeoFISH data for Over 10 meter vessesl. Logbook info from <10 m in GeoFISH it comes from Sales Notes and is less reliable.

eflalo_ft_gf = eflalo_ft_gf %>% filter ( VE_LEN >= 10 )
eflalo_ft_t3 = eflalo_ft_t3 %>% filter ( as.numeric( VE_LEN)  < 10 )


 
 

# explore the loaded data, can change to gf data to check
  head (eflalo_ft_t3)
  str(eflalo_ft_t3)
  dim(eflalo_ft_t3)
  summary(eflalo_ft_t3)

 

# load eflalo_le from both sources ----
  eflalo_le_t3 = read.csv(file = paste0(data_folder_t3, '\\eflalo_le.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM', colClasses = c ( rep(NA, 10) , "character") )  
  eflalo_le_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_le.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM' , colClasses = c ( rep(NA, 10) , "character") )
  names(eflalo_le_gf) = toupper( names(eflalo_le_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 


  head (eflalo_le_t3)
  str (eflalo_le_t3)

# load eflalo_spe from both sources ----
eflalo_spe_t3 = read.csv(file = paste0(data_folder_t3, '\\eflalo_spe.csv' ), header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
eflalo_spe_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_spe.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(eflalo_spe_gf) = toupper( names(eflalo_spe_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 


## Merge  EFLALO data blocks into one using common fields ----

eflalo_t3  =    eflalo_ft_t3 %>%
                inner_join (eflalo_le_t3 , by =  c("FT_REF" = "EFLALO_FT_FT_REF"))%>%
                inner_join(eflalo_spe_t3, by = c("LE_ID" = "EFLALO_LE_LE_ID"   ))


eflalo_t3 = eflalo_t3 %>% mutate ( LE_VALUE = -9999, FLEET_SEG = analysis_type, SOURCE = 't3') %>% select  ( - VE_FA)

eflalo_gf =     eflalo_ft_gf %>%
                inner_join (eflalo_le_gf , by =  c("FT_REF" = "EFLALO_FT_FT_REF"))%>%
                inner_join(eflalo_spe_gf, by = c("LE_ID" = "EFLALO_LE_LE_ID"   ))

eflalo_gf = eflalo_gf %>% rename ( LE_VALUE = LE_EURO )  %>%  mutate (   FLEET_SEG = analysis_type, SOURCE = 'geofish') 


eflalo = rbind(eflalo_t3, eflalo_gf)

 
 

# Convert the fields in required formats 

eflalo$FT_DDAT =  ymd(eflalo$FT_DDAT)  ## ymd lubridate function to CAST date into Year Mond Day date format
eflalo$FT_LDAT =  ymd(eflalo$FT_LDAT) 

eflalo = eflalo %>% mutate (LE_CDAT  = substr (eflalo$LE_CDAT, 1, 10) )  ## To convert Log Event date just in Year- month - day. Time is not provided an is by default 00:00:00 
eflalo$LE_CDAT = ymd(eflalo$LE_CDAT) 

eflalo$FT_DDATIM = ymd_hms(eflalo$FT_DDATIM) 
eflalo$FT_LDATIM = ymd_hms(eflalo$FT_LDATIM) 

eflalo$VE_LEN = as.numeric(eflalo$VE_LEN)
eflalo$VE_KW = as.numeric(eflalo$VE_KW)
eflalo$VE_TON = as.numeric(eflalo$VE_TON)

eflalo$EFLALO_FT_FT_REF  = as.numeric(eflalo$EFLALO_FT_FT_REF)
eflalo$LE_VALUE = as.numeric(eflalo$LE_VALUE )

eflalo$Year = year(eflalo$FT_DDATIM )
eflalo$Month = month(eflalo$FT_LDATIM)
 
 

## 2. Load TACSAT data    ####

 
tacsat_t3 = read.csv(file = paste0(data_folder_t3, '\\tacsat.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
tacsat_t3 = tacsat_t3 %>% mutate ( FLEET_SEG = analysis_type, SOURCE = 't3') 


tacsat_gf = read.csv(file = paste0(data_folder_geofish, '\\tacsat.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(tacsat_gf) = toupper( names(tacsat_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 
tacsat_gf = tacsat_gf %>% mutate ( FLEET_SEG = analysis_type, SOURCE = 'geofish') 

 
tacsat = rbind(tacsat_t3, tacsat_gf)

                     
 
dim(tacsat)
head (tacsat)
str(tacsat) ##List of fields and field type



tacsat$SI_DATE  =  ymd( tacsat$SI_DATE   )  ### reformatting the data in required format . Change to dmy if your system date format is different
tacsat$SI_DATIM  = ymd_hms(tacsat$SI_DATIM  ) 
tacsat$SI_SP = as.numeric(tacsat$SI_SP)
tacsat$SI_HE = as.numeric(tacsat$SI_HE)
 

tacsat%>%filter(is.na(SI_SP))%>%dim()



## To be run only when "Welsh waters" analysis is processed. 
## This code will limit the analysis to fishing locations occurring within the Welsh Waters boundaries

if ( analysis_type == 'welsh_waters' ) { 
  
  welsh_marine_area = st_read ( dsn = '.\\spatial_layers\\wales_plan_area.geojson' )
  welsh_marine_area_geom = st_make_valid(st_union(welsh_marine_area  ))
  
  
  tacsat_geom = tacsat %>% st_as_sf( ., coords = c("SI_LONG" ,"SI_LATI"), crs = 4326 , remove = FALSE)
  tacsat_geom_ww =  tacsat_geom  %>%  filter(  st_intersects( .,  welsh_marine_area_geom , sparse = FALSE)  )
  
  
  trips_in_welsh_waters = tacsat_geom_ww %>% st_drop_geometry() %>%  distinct(SI_FT ) %>% pull()
  
  eflalo = eflalo %>% filter ( FT_REF  %in%  trips_in_welsh_waters) 
  tacsat = tacsat  %>% filter ( SI_FT  %in% trips_in_welsh_waters ) 
  

  ## To visualize in a GIS software save the TACSAT  as point geometry 
  
  

  dir.create(".\\workflow_outputs\\spatial")

  st_write( tacsat_geom_ww, dsn = ".\\workflow_outputs\\spatial\\tacsat_welsh_waters.geojson", layer = "tacsat_welsh_waters.geojson")
  
  
}

### The EFLALO and TACSAT have been formatted and ready for analysis!!

save(eflalo, file = ".\\workflow_outputs\\eflalo.RData")
save(tacsat, file = ".\\workflow_outputs\\tacsat.RData")




