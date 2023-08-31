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
library(ggplot2)    ## R Package for plotitng and graphs

## SET YOUR R WORKING DIRECTORY

  ##check WD is in the desired data folder location
  getwd() 
  
  ## otherwise change to desired data folder location
  setwd('./../../data')



 ## SELECT THE ANALYSIS OPTION:
  
  analysis_type = 'welsh_waters' ## replace for 'welsh_waters'  if needed
  
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

 
 
# explore the loaded data, can change to gf data to check
  head (eflalo_ft_t3)
  str(eflalo_ft_t3)
  dim(eflalo_ft_t3)
  summary(eflalo_ft_t3)

<<<<<<< Updated upstream
  head (eflalo_ft)
  str(eflalo_ft)
  dim(eflalo_ft)
  summary(eflalo_ft)


 
eflalo_le_t3 = read.csv(file = paste0(data_folder_t3, '\\eflalo_le.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM', colClasses = c ( rep(NA, 10) , "character") )  
eflalo_le_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_le.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM' , colClasses = c ( rep(NA, 10) , "character")  ) 
names(eflalo_le_gf) = toupper( names(eflalo_le_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 


  head (eflalo_le_t3)
  str(eflalo_le_t3)
  dim(eflalo_le_t3)
=======
# load eflalo_le from both sources ----
  eflalo_le_t3 = read.csv(file = paste0(data_folder_t3, '\\eflalo_le.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM', colClasses = c ( rep(NA, 10) , "character") )  
  eflalo_le_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_le.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM' , colClasses = c ( rep(NA, 10) , "character") )
  names(eflalo_le_gf) = toupper( names(eflalo_le_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 
>>>>>>> Stashed changes

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


eflalo_t3 = eflalo_t3 %>% mutate ( VE_COU = 'GBW', LE_VALUE = -9999, FLEET_SEG = analysis_type, SOURCE = 't3') %>% select  ( - VE_FA)

eflalo_gf =     eflalo_ft_gf %>%
                inner_join (eflalo_le_gf , by =  c("FT_REF" = "EFLALO_FT_FT_REF"))%>%
                inner_join(eflalo_spe_gf, by = c("LE_ID" = "EFLALO_LE_LE_ID"   ))

eflalo_gf = eflalo_gf %>% rename ( LE_VALUE = LE_EURO )  %>%  mutate ( VE_COU = 'GBW',  FLEET_SEG = analysis_type, SOURCE = 'geofish') 


eflalo = rbind(eflalo_t3, eflalo_gf)

 
 

# Convert the fields in required formats 

eflalo$FT_DDAT =  ymd( eflalo$FT_DDAT   )  ## ymd lubridate function to CAST date into Year Mond Day date format
eflalo$FT_LDAT =   ymd(eflalo$FT_LDAT  ) 

eflalo = eflalo %>% mutate ( LE_CDAT  = substr ( eflalo$LE_CDAT, 1, 10) )  ## To convert Log Event date just in Year- month - day. Time is not provided an is by default 00:00:00 
eflalo$LE_CDAT =   ymd (eflalo$LE_CDAT  ) 

eflalo$FT_DDATIM = ymd_hms( eflalo$FT_DDATIM   ) 
eflalo$FT_LDATIM = ymd_hms( eflalo$FT_LDATIM  ) 

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
<<<<<<< Updated upstream

=======
>>>>>>> Stashed changes


tacsat_gf = read.csv(file = paste0(data_folder_geofish, '\\tacsat.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(tacsat_gf) = toupper( names(tacsat_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 
<<<<<<< Updated upstream

=======
>>>>>>> Stashed changes
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

### The EFLALO and TACSAT have been formatted and ready for analysis!!

tacsat_uk_u10m = tacsat
eflalo_uk_u10m = eflalo

save(eflalo, file = "./workflow_outputs/eflalo.RData")
save(tacsat, file = "./workflow_outputs/tacsat.RData")

