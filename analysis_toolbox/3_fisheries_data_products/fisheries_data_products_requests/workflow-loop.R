library(lubridate)  ## this package helps with manage date fields 
library(dplyr)      ## R Package to use pipelines (%>%) analysis language 
library(sf)         ## R Package for spatial analysis in R ( Simple Features )
library(ggplot2)    ## R Package for plotting and graphs
library(tidyr)
library(vmstools)


## SET YOUR R WORKING DIRECTORY

##check WD is in the desired data folder location
getwd() 

## otherwise change to desired data folder location
setwd('./../../data')

years <- 2012:2022
# year <- 2022


########################## LOAD #######################################################################################################################

for (year in years) {

## SELECT THE ANALYSIS OPTION:

analysis_type = 'welsh_fleet' ## Options: ( 'welsh_fleet' , 'welsh_waters'  ) 


if ( analysis_type == 'welsh_fleet')  { 
  
  ###  1. WELSH FLEET ACTIVITY ANALYSIS 
  
  data_folder_t3 = file.path( paste0 ( getwd() , '/welsh_fleet_data/t3' )  )
  data_folder_geofish = file.path( paste0 ( getwd() , '/welsh_fleet_data/geofish/', year) ) 
  
} else if  (analysis_type == 'welsh_waters' ) { 
  
  
  ## 2. WELSH WATERS ACTIVITY ANALYSIS 
  
  
  data_folder_t3 = file.path( paste0 ( getwd() , '/welsh_waters_data/t3' )  )
  data_folder_geofish = file.path( paste0 ( getwd() , '/welsh_waters_data/geofish/', year))
  
} 

# cols2rm <- c("X.x", "X.y", "X")

## 1. Load T3/GeoFISH  EFLALO data set blocks and merge them into one R data frame. ----

if (year == '2022') {
  
  eflalo_ft_t3 = read.csv(file = paste0(data_folder_t3, '\\eflalo_ft.csv') , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
  eflalo_ft_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_ft.csv') , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
  names(eflalo_ft_gf) = toupper( names(eflalo_ft_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 
  
  
  ## Convert the field VE_COU in a standard country acronym 
  countries_id = data.frame( ve_fa =   c('Wales', 'England', 'Isle of Man', 'NULL', 'Scotland', 'North Ireland') , ve_cou_gb = c( 'GBW', 'GBE', 'GBI','NULL', 'GBS', 'GBN' ) ) 
  eflalo_ft_t3 = eflalo_ft_t3 %>% left_join(countries_id, by =  c ( 'VE_FA' = 've_fa')) %>% mutate(VE_COU = ve_cou_gb) %>% select ( - ve_cou_gb)
  
  ## Filter GeoFISH data for Over 10 meter vessesl. Logbook info from <10 m in GeoFISH it comes from Sales Notes and is less reliable.
  
  eflalo_ft_t3 %>% filter ( VE_LEN != 'NULL') %>% mutate ( VE_LEN =  as.numeric(VE_LEN) ) 
  
  
  eflalo_ft_gf = eflalo_ft_gf %>% filter ( VE_LEN >= 10 )
  eflalo_ft_t3 = eflalo_ft_t3 %>%  filter ( VE_LEN != 'NULL') %>% mutate ( VE_LEN =  as.numeric(VE_LEN) ) %>%
    filter(as.numeric( VE_LEN)  < 10 )
  str(eflalo_ft_t3)
  
  
  
  
  
  
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
  
  # eflalo_gf <- eflalo_gf[, !names(eflalo_gf) %in% cols2rm]
  
  eflalo = rbind(eflalo_t3, eflalo_gf)

  } else {



## list.files(path = '.\\..\\data') # check the files in your directory 


eflalo_ft_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_ft.csv') , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(eflalo_ft_gf) = toupper( names(eflalo_ft_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 


## Convert the field VE_COU in a standard country acronym 
countries_id = data.frame( ve_fa =   c('Wales', 'England', 'Isle of Man', 'NULL', 'Scotland', 'North Ireland') , ve_cou_gb = c( 'GBW', 'GBE', 'GBI','NULL', 'GBS', 'GBN' ) ) 

## Filter GeoFISH data for Over 10 meter vessesl. Logbook info from <10 m in GeoFISH it comes from Sales Notes and is less reliable.

eflalo_ft_gf = eflalo_ft_gf %>% filter ( VE_LEN >= 10 )



# load eflalo_le from both sources ----
eflalo_le_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_le.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM' , colClasses = c ( rep(NA, 10) , "character") )
names(eflalo_le_gf) = toupper( names(eflalo_le_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 


# load eflalo_spe from both sources ----
eflalo_spe_gf = read.csv(file = paste0(data_folder_geofish, '\\eflalo_spe.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(eflalo_spe_gf) = toupper( names(eflalo_spe_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 


## Merge  EFLALO data blocks into one using common fields ----

eflalo_gf =     eflalo_ft_gf %>%
  inner_join (eflalo_le_gf , by =  c("FT_REF" = "EFLALO_FT_FT_REF"))%>%
  inner_join(eflalo_spe_gf, by = c("LE_ID" = "EFLALO_LE_LE_ID"   ))

eflalo_gf = eflalo_gf %>% rename ( LE_VALUE = LE_EURO )  %>%  mutate (   FLEET_SEG = analysis_type, SOURCE = 'geofish') 

# eflalo_gf <- eflalo_gf[, !names(eflalo_gf) %in% cols2rm]

}


if (year == '2022') {
  
  eflalo = rbind(eflalo_t3, eflalo_gf)
  
} else {
  
  eflalo = eflalo_gf
  
  }




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

if (year == 2022) {
  # tacsat_t3 = read.csv(file = paste0(data_folder_t3, '\\tacsat.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
  # tacsat_t3 = tacsat_t3 %>% mutate ( FLEET_SEG = analysis_type, SOURCE = 't3')
  
  load("Z:\\FISHERIES M MoU\\Working_Area\\spatial_fisheries_data\\welsh_gov_srf2\\tacsatActivity_t32022.RData")
  
  tacsatp = tacsatp %>% select (1:15)
  
  tacsat_t3 = tacsatp
  
  tacsat_gf = read.csv(file = paste0(data_folder_geofish, '\\tacsat.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
  names(tacsat_gf) = toupper( names(tacsat_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 
  tacsat_gf = tacsat_gf %>% mutate ( FLEET_SEG = analysis_type, SOURCE = 'geofish') 
  
  cols2rm <- c("GID", "GEOM")
  tacsat_gf <- tacsat_gf[, !names(tacsat_gf) %in% cols2rm]

  
  
  tacsat = rbind(tacsat_t3, tacsat_gf)
  
} else {

  tacsat_gf = read.csv(file = paste0(data_folder_geofish, '\\tacsat.csv' ) , header = T, sep = ','  , fileEncoding = 'UTF-8-BOM')
  names(tacsat_gf) = toupper( names(tacsat_gf ) )  ## Column names are lower case in geofish , needs to be changed to upper case 
  tacsat_gf = tacsat_gf %>% mutate ( FLEET_SEG = analysis_type, SOURCE = 'geofish') 
  
  cols2rm <- c("GID", "GEOM")
  tacsat_gf <- tacsat_gf[, !names(tacsat_gf) %in% cols2rm]

  tacsat = tacsat_gf

}

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

## Do you want to keep logbook records iwth no related VMS/IVMS ( TACSAT) records ? .
## Otehrwise select 'no' to keep only eflalo records iwth asscociated tacsat ( to ensure trips have a vessel location within Welsh Waters ) 

retain_eflalo_with_no_tacsat = 'yes'

if ( analysis_type == 'welsh_waters'  ) { 
  
  welsh_marine_area = st_read ( dsn = '.\\spatial_layers\\wales_plan_area.geojson' )
  welsh_marine_area_geom = st_make_valid(st_union(welsh_marine_area  ))
  
  
  tacsat_geom = tacsat %>% st_as_sf( ., coords = c("SI_LONG" ,"SI_LATI"), crs = 4326 , remove = FALSE)
  tacsat_geom_ww =  tacsat_geom  %>%  filter(  st_intersects( .,  welsh_marine_area_geom , sparse = FALSE)  )
  
  
  trips_in_welsh_waters = tacsat_geom_ww %>% st_drop_geometry() %>%  distinct(SI_FT ) %>% pull()
  
  if ( retain_eflalo_with_no_tacsat == 'no' ) { 
    
    eflalo = eflalo %>% filter ( FT_REF  %in%  trips_in_welsh_waters) 
    
  } 
  
  tacsat = tacsat  %>% filter ( SI_FT  %in% trips_in_welsh_waters ) 
  
  
  
  ## To visualize in a GIS software save the TACSAT  as point geometry 
  
  
  #st_write( tacsat_geom_ww, dsn = paste0(".\\workflow_outputs\\spatial\\tacsat_welsh_waters_SRF2_", year, ".geojson"), layer = paste0("tacsat_welsh_waters_SRF2_", year, ".geojson"))
  
  
}

### The EFLALO and TACSAT have been formatted and ready for analysis!!

save(eflalo, file = paste0(".\\workflow_outputs\\eflalo_", analysis_type, "_", year, ".RData"))
save(tacsat, file = paste0(".\\workflow_outputs\\tacsat_", analysis_type, "_", year, ".RData"))


#rm(list = ls())

########################## PREPROCESSING #######################################################################################################################

load(paste0(".\\workflow_outputs\\eflalo_", analysis_type, "_", year, ".RData"))
load(paste0(".\\workflow_outputs\\tacsat_", analysis_type, "_", year, ".RData"))

### Define the fleet segment to be analysed from the Analysis Option chosen in "0_DATA_ACCESS" toolbox section.

## Fleet segment: 
## - Over 12 m vessels ( source GeoFISH)
## - Under 12 m vessels ( source T3)
## - Combined O12m and U12m fleets

### If you want to analyse a specific section of the data, apply the following if statement


fleet_segment = 'all' ## Options: ( 'over12', 'under12', 'all' )


if ( fleet_segment == 'over12')  { 
  
  ###  1. Over 12m vessels analysis
  
  eflalo_fs = eflalo %>% filter(VE_LEN >= 12) %>% mutate  ( FLEET_SEG = paste0 ( FLEET_SEG, '_', fleet_segment))
  tacsat_fs = tacsat %>% filter(VE_REF %in% eflalo_fs$VE_REF)
  #tacsat = tacsat %>% filter(SI_FT %in% eflalo$FT_REF)
  
  
} else if  (fleet_segment == 'under12' ) { 
  
  
  ### 2. Under 12m vessels analysis 
  
  eflalo_fs = eflalo %>% filter(VE_LEN < 12) %>% mutate  ( FLEET_SEG = paste0 ( FLEET_SEG, '_', fleet_segment))
  tacsat_fs = tacsat %>% filter(VE_REF %in% eflalo_fs$VE_REF)
  
} else if  (fleet_segment == 'all' ) { 
  
  
  ### 3. All vessels 
  
  eflalo_fs = eflalo 
  tacsat_fs = tacsat  
  
} 



#### EXPLORATION:  Initial EFLALO and TACSAT exploration ########

## Explore the stats from EFLALO to get fleet statistics and fleet characterisation


## Explore vessels nationality in the EFLALO records 


## Create the VESSEL LENGTH CAGTEGORY to explore data by vessel length classes

if ( fleet_segment == 'over12')  {  
  
  eflalo_fs$VE_LEN_CAT = cut(eflalo_fs$VE_LEN , include.lowest = T,
                             breaks=c(-Inf, 12, 15, 18, 24, 40,  Inf), 
                             labels=c("<12m",  "12-15m","15-18m", "18-24m", "24-40m" ,"=>40m"))
  
  
  
  
  
} else if  (fleet_segment == 'under12' ) { 
  
  
  eflalo_fs$VE_LEN_CAT = cut(eflalo_fs$VE_LEN , include.lowest = T,
                             breaks=c(-Inf, 4.5, 4.999 , 6.999, 8.999, 11.999 , Inf), 
                             labels=c("<4.5m","4.5-5m", "5-7m", "7-9m", "9-12m", "=>12m"))
  
} else if ( fleet_segment == 'all') { 
  
  
  eflalo_fs$VE_LEN_CAT = cut(eflalo_fs$VE_LEN , include.lowest = T,
                             breaks=c(-Inf, 6, 8, 10, 12, 15, 18, 24, 40,  Inf), 
                             labels=c("<6m","6-8m", "8-10m", "10-12m", "12-15m","15-18m", "18-24m", "24-40m" ,"=>40m"))
  
  
  
  
}



## Create the field "trip_days" with duration of each trips as   number of days  

eflalo_fs  = eflalo_fs %>% mutate ( trip_days = as.numeric(difftime(eflalo_fs$FT_LDATIM, eflalo_fs$FT_DDATIM), units = "days") ) 

head(eflalo_fs)
str(eflalo_fs)



## Plot the categories to understand your fleet composition

## PLOT 1: Number of trips by v

## PLOT 3: Main species captured

## Rank the main species captured by gear type

# res1 = eflalo_fs %>%
#   group_by(  LE_GEAR, LE_SPE) %>%
#   summarise(le_kg_total = sum( LE_KG) )  %>%
#   mutate ( rank = dense_rank( desc(le_kg_total) )) %>%
#   arrange(LE_GEAR, desc(le_kg_total) ) %>%
#   filter(rank <= 5) %>%
#   ungroup() 
# 
# 
# write.csv( x = res1, file = paste0(".\\workflow_outputs\\species_kg_ranked_by_gear_bd_", year, ".csv"), row.names=FALSE)

## Save the intermediate EFLALO and TACSAT datasets

save(eflalo_fs, file = paste0(".\\workflow_outputs\\eflalo_fs_", analysis_type, "_", year, ".RData"))
save(tacsat_fs, file = paste0(".\\workflow_outputs\\tacsat_fs_", analysis_type, "_", year, ".RData"))



#rm(list = ls())


########################## QUALITY CONTROL #######################################################################################################################


load(paste0(".\\workflow_outputs\\eflalo_fs_", analysis_type, "_", year, ".RData"))
load(paste0(".\\workflow_outputs\\tacsat_fs_", analysis_type, "_", year, ".RData"))


#### QUALITY CONTROL:  Clean data with potential  outlines ########



# 1. Clean the EFLALO (CR)  data  ============================================================================


## 1.1  Number of EFLALO records and fishing Log Events records

## Q1.1: How many overall records are by  trip?  

# res2 = eflalo_fs%>%group_by(FT_REF) %>% summarise(n = n()) %>% arrange(desc(n))
# ggplot(res2, aes( n )) + geom_histogram()
# 
# 
# ## Q1.2: How many log events are by  trip?  ( LOG EVENTS ONLY EXIST IN E-LOGBOOKS from GEOFISH SOURCE / CR does not collect LOG EVENTS)
# 
# 
# res21= eflalo_fs%>%distinct (FT_REF, LE_ID , trip_days) %>%group_by(FT_REF, trip_days)%>%summarise(n = n())%>%arrange(desc(n))
# ggplot(res21, aes( n )) + geom_histogram()


# Q1.2.1 . How we assign GEAR and ICES RECTANGLE to iVMS/VMS TACSAT records . Example of different cases: 


###  1st: FISHING TRIP WITH > 1 GEARS REPORTED 

## Identify trips with more than 1 gear 

trips_with_more_than_1_gear = eflalo_fs %>% distinct(FT_REF, LE_GEAR )%>% group_by(FT_REF ) %>% mutate ( rn = row_number ( ) ) %>% filter ( rn > 1) 


## SOLUTION: The best solution for those fishing trips using more than 1 Gear is to unify them into a unique combined gear 
## So these trips will have a merged gear and metier defined: e.g. a trip using GN and FPO would have a gear defined as : GN_FPO

eflalo_fs_mult_gears = eflalo_fs %>% filter(FT_REF %in% trips_with_more_than_1_gear$FT_REF )

trips_mult_gears_comb = eflalo_fs_mult_gears %>% 
  distinct(FT_REF, LE_GEAR, LE_MET ) %>% arrange( FT_REF , LE_GEAR, LE_MET) %>%
  group_by( FT_REF  )  %>% 
  mutate ( LE_GEAR_C = paste0  ( LE_GEAR, collapse =  "_"), LE_MET_C =  ifelse( LE_MET != 'NULL',  paste0  ( LE_MET, collapse =  "_"), LE_MET  ) ) %>%
  select (FT_REF, LE_GEAR_C, LE_MET_C )


## 1.2 Duration of the fishing trips 
## Filter out those outlying trip durations

trips_outliers =  eflalo_fs %>% 
  filter(SOURCE == 't3' & trip_days > 5 ) %>% 
  select(FT_REF) %>%
  distinct(FT_REF) %>%
  pull()


length ( trips_outliers )

# < 5 days trips : 9194
# < 10 days trips : 9207
# total trips : 9228

eflalo_ti = eflalo_fs %>% filter ( ( ! FT_REF %in% trips_outliers ) ) 

dim ( eflalo_ti )
dim(eflalo_fs)

## Q2.2: Why we have done a 2 steps filtering?



# 1.3 Warn for outlying catch records ----------------------------------------------------------

## Select a logarithm scale landing weight threshold ( e.g. 1.5)

landingThreshold = 1.5

## Identify the records with species captures over expected thresholds 
# 1.6  Remove non-unique trip numbers -----------------------------------------------------------------------------


duplicate_analysis = eflalo_fs%>%
  distinct(VE_REF , FT_REF,  FT_DDAT, FT_LDAT)%>%
  group_by(VE_REF, FT_DDAT, FT_LDAT)%>%
  summarise( number_trips = n() ) %>%
  mutate ( duplicated = ifelse( number_trips >  1, TRUE , FALSE    )  )  


## Explore duplicated trips details


## Identify the trip id's that are duplicated 

duplicate_trips =  eflalo_fs%>%
  left_join (duplicate_analysis , by = c ( 'VE_REF', 'FT_DDAT', 'FT_LDAT'))%>%
  group_by(FT_REF)%>%
  mutate ( number_records = n ())%>% 
  ungroup()%>%
  filter(duplicated == TRUE) %>%
  distinct(VE_REF, FT_REF, FT_DDAT ,  FT_LDAT , number_records) %>%
  arrange( VE_REF,   FT_DDAT ,  FT_LDAT , desc( number_records) )%>%
  ungroup()%>%
  group_by(VE_REF, FT_DDAT,FT_LDAT)%>%
  mutate(r_num = row_number() )%>%
  filter(r_num > 1)%>%
  select(FT_REF)%>%
  pull()

## Filter out the duplicated trips

eflalo_fs =   eflalo_fs %>%
  filter( ! FT_REF %in% duplicate_trips   ) 






# 1.7 Remove records with arrival date before departure date  ------------------------------------------------------------


## Use the field created with trip duration: trip_days
## A value of 0 or negative would means departure and landing dates are same or reversed
# 2.3.7 Remove trip with overlap with another trip --------------------------------------------------------------------------- 

## Identify the vessel trips that have a departure date overlapping with previous return/landing date


overlaps_analysis =  eflalo_fs%>%distinct(VE_REF, FT_REF, FT_DDATIM, FT_LDATIM)%>%
  arrange(VE_REF, FT_DDATIM)%>%
  group_by(VE_REF)%>%
  mutate( overlaps = FT_LDATIM > lead (FT_DDATIM)   )



## Check what vessel and trips dates are overlapping

overlaps_analysis%>%
  filter ( overlaps == TRUE)



## Filter the trips details for the vessel during the overlapping dates 
## Replace VE_REF and DATES based on previous code line results. 



## Q1: What are you doing with the overlapping trips? Do we remove them or fix them?
## This will impact the VMS locations associated to these trips , so to avoid duplication and conflicts , correct or delete overlapping trips
## In the example above the ideal solution would be remove 2 out of the 3 duplicated trips recorded




# 2 Clean  TACSAT data  ----------------------------------------------------------------------------------


# 1.1 Load spatial auxiliary data ===========================================

## LIBRARY SF required for spatial analysis 


welsh_marine_area = st_read ( dsn = '.\\spatial_layers\\wales_plan_area.geojson' )
port_500m = st_read( dsn = '.\\spatial_layers\\welsh_ports_ammended_0_005.geojson')
land = st_read ( dsn = '.\\spatial_layers\\Europe_coastline_poly.shp')
europe_aoi = st_read ( dsn = '.\\spatial_layers\\europe_aoi.geojson')  ###load the layer with crop are of interest 
ICESareas = st_read(dsn = '.\\spatial_layers\\ICES_rectangles.geojson')
ices_rect_welsh = st_read(dsn = '.\\spatial_layers\\ICES_rectangle_welsh.geojson')

## explore connection to WFS/WMS services ( Welsh Portal ,  OSGB )

welsh_marine_area %>% st_crs()  ## WGS 84 EPSG: 4326
port_500m %>% st_crs()   ## WGS 84
land  %>% st_crs() 
ICESareas = ICESareas %>% st_transform(4326)

land_4326 = land %>% st_transform( 4326 )   ## reproject the sf object ( spatial layer ) into a new coordinate system 



## Define an object bbox ( bounding box )  with the area of interest 


aoi = st_bbox( c( xmin = -15, xmax = 3, ymax = 60, ymin = 47), crs = st_crs( 4326 )) ## Define our area of intenrest.  4326 is id for WGS1984 unprojected coordinate system 

europe_aoi = st_crop (x = land_4326, y = aoi)  ## clip/crop the whole European layer to our of interest

# 2 Clean the TACSAT and EFLALO data  ----------------------------------------------------------------------------------



## Filter TACSAT data with the trips result from cleaning EFLALO data

trips_in_clean_eflalo = eflalo_fs %>% distinct(FT_REF)%>%pull() 


tacsat_fs -> bk
tacsat_fs = tacsat_fs %>% filter( SI_FT %in%  trips_in_clean_eflalo  ) 


# Q1 : Number of trips in tacsat (iVMS) present in eflalo 

tacsat_fs %>% mutate(inboth = SI_FT %in% trips_in_clean_eflalo )%>%select(inboth)%>%table()

# Q2 : Number of trips in eflalo present in tacsat (iVMS)  

eflalo_fs %>%distinct(FT_REF)%>% mutate(inboth = FT_REF %in% ( tacsat_fs%>%distinct(SI_FT)%>%pull())  )%>%select(inboth)%>%table()

## Why are so many trips with not iVMS information

## Analyse the trips with not TACSAT related

ft_ref_not_in_tacsat = eflalo_fs %>%distinct(FT_REF)%>% mutate(inboth = FT_REF %in% ( tacsat_fs%>%distinct(SI_FT)%>%pull())  )%>%
  filter ( inboth == FALSE)

res1 = eflalo_fs %>% filter( FT_REF %in% (ft_ref_not_in_tacsat %>% select (FT_REF) %>% pull() ) ) %>% 
  filter( FT_DDAT > '2022-02-15')


# 2.2 Take only VMS pings in the ICES areas ==============================================

ia =  ICESareas%>% 
  sf::st_as_sf() %>% 
  sf::st_make_valid() %>% 
  sf::st_transform(4326)  

overs = 
  tacsat_fs  %>% 
  sf::st_as_sf(coords = c("SI_LONG", "SI_LATI")) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_intersects(ia)

tacsat_fs = tacsat_fs[lengths(overs) > 0,]



#   2.2 Clean the tacsat data  ============================================================================

# 2.2.3 Remove points that cannot be possible -----------------------------------------------------------

#tacsat_bk = tacsat_fs
#tacsat_fs = tacsat_bk

tacsat_fs = tacsat_fs %>% filter(abs(SI_LATI) < 90 | abs(SI_LONG) < 180)
tacsat_fs = tacsat_fs %>% filter(SI_HE >= 0 | SI_HE <= 360)



# 2.2.4 Remove points which are pseudo duplicates as they have an interval rate < x minutes/second  ------------------

## Convert the TACSAT into a spatial object (SF package)

tacsat_fs_geom = tacsat_fs %>% st_as_sf( ., coords = c("SI_LONG" ,"SI_LATI"), crs = 4326 , remove = FALSE)

# st_write( tacsat_fs_geom, dsn = paste0(".\\workflow_outputs\\tacsat_fs_", analysis_type, "_", year, ".geojson"),
#           layer = paste0("tacsat_fs_", analysis_type, "_", year, ".geojson"), append = FALSE)

## Q1: What is the minimum expected time interval between iVMS positions

## Use that value to filter potential pseudo-duplicates ( values below the minimum expected interval)

minInterval = 1 / 60 ## 1 minute converted in hours (0.01666667 hours)

tacsat_fs_geom = tacsat_fs_geom%>%ungroup()%>%
  group_by(VE_REF, SI_FT)%>% arrange (VE_REF, SI_DATIM)%>%
  mutate (INTV =  difftime(SI_DATIM , lag(SI_DATIM), units = "hours" )  )%>% ## Calcualte the difference between a iVMS loction time stamp and previous location to calcualte a fishign effort in a given location
  mutate(interval_mean = mean(INTV , na.rm = T))%>% ## Calcualte the mean to replace the NA's interval when a VMS location is the 1st of a trip and cannot calculate with a prev. iVMS location
  mutate(INTV = ifelse( is.na(INTV), interval_mean, INTV ))%>%  ## Convert the NA's into a effort represented by the mean of that vessel durign given trip
  select(- interval_mean) %>%ungroup()



##Q2: Check the structure of TACSAT . Did the field types changed?

# 2.2.5 Remove points in harbour -----------------------------------------------------------------------------

##Q1: Are the vessel iVMS positions in/nearby a harbour?

## Use a SPATIAL JOIN to link spatially the iVMS locations within Port locations.
## The spatial relatioship is the intersection between a iVMS poitn and a polygon representing the area buffered 3Km around the port location

tacsat_fs_ports = tacsat_fs_geom %>%
  st_join ( port_500m %>% select ( Name, District_N) , join = st_intersects, left = T) %>%
  mutate  ( SI_HARB  = ifelse ( is.na ( Name  ), FALSE , TRUE))  %>%
  select ( - names(port_500m%>% select ( Name, District_N)))


getwd()
# st_write( tacsat_fs_ports, dsn = paste0(".\\workflow_outputs\\spatial\\tacsat_port_welsh_", year, ".geojson"), 
#           layer = paste0("tacsat_port_welsh_", analysis_type, "_", year, ".geojson"), append = FALSE)

# 2.2.6 Remove points on land -----------------------------------------------------------------------------

tacsat_fs_land = tacsat_fs_ports %>% 
  st_join( europe_aoi , join = st_intersects, left = T )%>%
  mutate  ( SI_LAND  = ifelse ( is.na ( Id ), FALSE ,TRUE))%>%
  select ( - names(europe_aoi) )  

# st_write( tacsat_fs_land, dsn = paste0(".\\workflow_outputs\\spatial\\tacsat_fs_land_", analysis_type, "_", year,".geojson"), 
#           layer = "tacsat_fs_land", analysis_type, "_", year,".geojson", append = FALSE )      


##Q1: How many points are detected on land? 

tacsat_fs_land%>%st_drop_geometry()%>%group_by(SI_LAND)%>%count()



##Q2: Plot the iVMS locations when in land


## Remove the fields taken from the ports datasets . Not needed for our analysis example


# tacsat_fs_land = tacsat_fs_land %>% mutate ( SI_STATE = ifelse  ( SI_SP  >= 1 & SI_SP <= 6 , 'f', 's'  ))


tacsat_fs_df = tacsat_fs_land %>% filter(SI_LAND == FALSE & SI_HARB == FALSE )


# st_write( tacsat_fs_df, dsn = paste0(".\\workflow_outputs\\tacsat_fs_df_", analysis_type, "_", year, ".geojson", 
#                                      layer = paste0("tacsat_fs_df_", analysis_type, "_", year, ".geojson")))


#   Save the cleaned EFLALO file 

save(eflalo_fs, file = paste0('.\\workflow_outputs\\eflalo_fs_qc_', analysis_type, "_", year, '.RData'))


#   Save the cleaned TACSAT file 


tacsat_fs = tacsat_fs_df

save(tacsat_fs, file = paste0('.\\workflow_outputs\\tacsat_fs_qc_', analysis_type, "_", year, '.RData'))


## Check how match the reported LE_RECT in EFLALO and the actual rectangle the  VMS location is located

tac_geom = tacsat_fs_df %>% st_as_sf( ., coords = c("SI_LONG" ,"SI_LATI"), crs = 4326 )

tacsat_fs_df_geom = tac_geom %>%
  st_join ( ices_rect_welsh%>% select ( icesname ) , join = st_intersects, left = T) %>%
  mutate  ( SI_RECT = icesname) 


#rm(list = ls())

########################## TOOLBOX LITE #######################################################################################################################


load(paste0(".\\workflow_outputs\\eflalo_fs_qc_", analysis_type, "_", year, ".RData"))
load(paste0(".\\workflow_outputs\\tacsat_fs_qc_", analysis_type, "_", year, ".RData"))

eflalo = eflalo_fs
tacsat = tacsat_fs 

### 1. Define the fleet segment to be analysed from the Analysis Option chosen in "0_DATA_ACCESS" toolbox section ####

## Fleet segment: 
## - Over 12 m vessels ( source GeoFISH)
## - Under 12 m vessels ( source T3)
## - Combined O12m and U12m fleets

### If you want to analyse a specific section of the data, apply the following if statement


fleet_segment = 'all' ## Options: ( 'over12', 'under12', 'all' )


if ( fleet_segment == 'over12')  { 
  
  ###  1. Over 12m vessels analysis
  
  eflalo_fs = eflalo %>% filter(VE_LEN >= 12) %>% mutate  ( FLEET_SEG = paste0 ( FLEET_SEG, '_', fleet_segment))
  tacsat_fs = tacsat %>% filter(VE_REF %in% eflalo_fs$VE_REF)
  #tacsat = tacsat %>% filter(SI_FT %in% eflalo$FT_REF)
  
  
} else if  (fleet_segment == 'under12' ) { 
  
  
  ### 2. Under 12m vessels analysis 
  
  eflalo_fs = eflalo %>% filter(VE_LEN < 12) %>% mutate  ( FLEET_SEG = paste0 ( FLEET_SEG, '_', fleet_segment))
  tacsat_fs = tacsat %>% filter(VE_REF %in% eflalo_fs$VE_REF)
  
} else if  (fleet_segment == 'all' ) { 
  
  
  ### 3. All vessels 
  
  eflalo_fs = eflalo 
  tacsat_fs = tacsat  
  
} 





## 2. Create the VESSEL LENGTH CAGTEGORY to explore data by vessel length classes ####

if ( fleet_segment == 'all')  {  
  
  eflalo_fs$VE_LEN_CAT = cut(eflalo_fs$VE_LEN , include.lowest = T,
                             breaks=c(-Inf, 12, 15, 18, 24, 40,  Inf), 
                             labels=c("<12m",  "12-15m","15-18m", "18-24m", "24-40m" ,"=>40m"))
  
  
  
  
  
} else if  (fleet_segment == 'under12' ) { 
  
  
  eflalo_fs$VE_LEN_CAT = cut(eflalo_fs$VE_LEN , include.lowest = T,
                             breaks=c(-Inf, 4.5, 4.999 , 6.999, 8.999, 11.999 , Inf), 
                             labels=c("<4.5m","4.5-5m", "5-7m", "7-9m", "9-12m", "=>12m"))
  
} else if ( fleet_segment == 'all') { 
  
  
  eflalo_fs$VE_LEN_CAT = cut(eflalo_fs$VE_LEN , include.lowest = T,
                             breaks=c(-Inf, 6, 8, 10, 12, 15, 18, 24, 40,  Inf), 
                             labels=c("<6m","6-8m", "8-10m", "10-12m", "12-15m","15-18m", "18-24m", "24-40m" ,"=>40m"))
  
  
  
  
}



## 3. Create the field "trip_days" with duration of each trips as   number of days  ####

eflalo_fs  = eflalo_fs %>% mutate ( trip_days = as.numeric(difftime(eflalo_fs$FT_LDATIM, eflalo_fs$FT_DDATIM), units = "days") ) 




### 4. DATA QUALITY CONTROL ( Follow the script in '1_data_preprocessing/1_eflalo_tacsat_quality_control.R' )  ############

## Load eflalo and tacsat cleaned data 

##   load(file = '.\\workflow_outputs\\eflalo_fs_qc.RData' )
##   load(file = '.\\workflow_outputs\\tacsat_fs_qc.RData' )

### 5. ANALYSIS OF EFLAO AND TACSAT COMBINED ( Is assumed the data input is already clean and quality controlled) ####


# 5.1 Merge the TACSAT and EFLALO data together --------------------------------------------

# Merge eflalo and tacsat =================================


# RENAME LE_VALUE to LE_EURO to proceed with VMSTOOLS format analysis 



eflalo_fs = rename (eflalo_fs, LE_EURO = LE_VALUE)

eflalo_fs = eflalo_fs %>% mutate(LE_EURO = ifelse(LE_EURO == -9999, 0, LE_EURO))

eflalo_fs %>% filter(LE_EURO < 0) %>% tally()

## Convert EFLALO from LARGE format into WIDE format. This complies with the format required by VMSTOOL 'splitamongpings' function
## Depending the format selected we have to choose Option 1 or Option 2 in step '2.3 Dispatch landings/catches among VMS pings'
## As a general approach , if we want to analyse all species together choose large format if we want to anlyse a number of species choose wide format

eflalo_fs = eflalo_fs%>%
  pivot_wider(id_cols = c( FT_REF, FT_DCOU, FT_DHAR, FT_DDAT, FT_DTIME, FT_DDATIM,
                           FT_LCOU, FT_LHAR, FT_LDAT, FT_LTIME, FT_LDATIM, VE_REF,
                           VE_FLT, VE_COU, VE_LEN, VE_KW, VE_TON, FT_YEAR,
                           LE_ID, LE_CDAT, LE_STIME, LE_ETIME, LE_SLAT, LE_GEAR,
                           LE_MSZ, LE_RECT, LE_DIV, LE_MET, EFLALO_FT_FT_REF, Year,
                           Month, VE_LEN_CAT, SOURCE, FLEET_SEG ),
              names_from = c(LE_SPE), values_from = c(LE_KG, LE_EURO))



# Assign gear and length to tacsat =================================
# Consider that one fishing trip can use more than one geear, 
# we use log events instead of fishing trip to linkboth dataset

## 1. Assign LOG EVENT  and FISHING TRIP information to VMS locations 

## Select the fields required from EFLALO to be transferred into TACSAT

eflalo_sel =  eflalo_fs %>% 
  select(FT_REF, LE_CDAT, LE_GEAR,LE_MSZ, LE_RECT, LE_MET, VE_LEN, VE_KW, VE_COU) %>%
  distinct()



tacsatp =  tacsat_fs%>%  left_join( eflalo_sel  , by = c("SI_FT" = "FT_REF"       , "SI_DATE" = "LE_CDAT"  )   )

tacsatp%>%filter ( is.na(LE_GEAR)) ## must be 0 , that means all iVMS records have an associated EFLALO records 



## Get the first most used gear by fishing trip to fill VMS records with  not associated Log Event.
## As a result some VMS records have not gear associated

ft =    eflalo_sel %>%arrange( FT_REF , LE_CDAT) %>% 
  select (FT_REF, LE_GEAR)%>% rename( LE_GEAR2 = LE_GEAR)%>%
  group_by(FT_REF)%>%slice(1)

## Assign  the main of gear of each trip to the VMS locations with not gear assigned

tacsatp =  tacsatp %>% left_join ( ft , by = c( "SI_FT" = "FT_REF"))%>%
  mutate ( LE_GEAR = ifelse( is.na (LE_GEAR), LE_GEAR2 , LE_GEAR  ))%>%
  select (-LE_GEAR2)


## The result of this query must be 0 records. Meaning all records have an associated gear

tacsatp %>% filter( is.na( LE_GEAR ) )

## Fill the information for vessel characteristics for those locations with not associated LOG EVENT

ve_c = eflalo_sel%>%distinct(FT_REF, VE_LEN, VE_KW, VE_COU)%>%rename(VE_LEN2 = VE_LEN, VE_KW2 = VE_KW, VE_COU2 = VE_COU )

tacsatp =  tacsatp %>% left_join ( ve_c , by = c( "SI_FT" = "FT_REF"))%>%
  mutate ( VE_LEN = ifelse(is.na (VE_LEN), VE_LEN2 , VE_LEN  ), 
           VE_KW = ifelse( is.na (VE_KW), VE_KW2 , VE_KW  ) , 
           VE_COU = ifelse(is.na (VE_COU), VE_COU2 , VE_COU  ) )%>%
  select (-c(VE_LEN2, VE_KW2, VE_COU2))





## To proceed with analysis we change the format to a data.frame

tacsatp = tacsatp %>% as.data.frame()

tacsatp %>% filter(SOURCE == 't3')

tacsatp = tacsatp %>% select(!geometry)

#write.csv(tacsatp, paste0("Z:\\FISHERIES M MoU\\Working_Area\\spatial_fisheries_data\\welsh_gov_srf2\\tacsatp_", year, "_", analysis_type, ".csv"), row.names = FALSE)
save(tacsatp, file = paste0(".\\workflow_outputs\\tacsatp_", analysis_type, "_", year, ".RData"))

save(eflalo_fs, file = paste0('.\\workflow_outputs\\eflalo_fs_qc_', analysis_type, "_wide_", year, '.RData'))

print(paste0("Finished ", year))
}


## 5.2 Define the fishing activity ( This is based on expert criteria and can be defined using the script in 2_eflalo_tacsat_analysis\ 2_eflalo_tacsat_analysis.R from line 14 to 321)  ######

year = 2013

analysis_type = 'welsh_waters' ### welsh_fleet / welsh_waters

for (year in years) {
  
  #load(paste0(".\\workflow_outputs\\eflalo_fs_qc_", analysis_type, "_", year, ".RData"))
  load(paste0(".\\workflow_outputs\\eflalo_fs_qc_", analysis_type, "_wide_", year, '.RData'))
  
  load(paste0(".\\workflow_outputs\\tacsatp_", analysis_type, "_", year, ".RData"))
  
  # 5.3 Dispatch landings/catches of merged eflalo at the VMS/iVMS ping scale  -------------------------------------------------
  
  eflalo_format = 'wide' ## defautl format following toolbox workflow 
  species_analysis_type = 'all_species_separated' # options: ( all_species_sum, all_species_separated ,   selected_species_sum, selected_species_separated )
  
  if ( eflalo_format == 'wide' ) { 
    
    ## For all the species together 
    
    
    ## Option 1: Names in wide format . Each species catch value has its own column 
    
    idxkg  =  grep("KG", colnames(eflalo_fs ) )  
    idxval =  grep("VALUE", colnames(eflalo_fs )  ) 
    
    
    
    ## Option 1.1 : all landings for all species sum together 
    if (species_analysis_type == 'all_species_sum') {
      
      eflalo_fs_tot = eflalo_fs %>% mutate(LE_KG_TOT = select(.,idxkg) %>% rowSums(., na.rm = TRUE) ) %>% select ( -c ( idxkg, idxval) ) %>% mutate(LE_EURO_TOT = NA ) %>% as.data.frame()       
      
    } else if (species_analysis_type == 'all_species_separated') {
      
      eflalo_fs_tot = eflalo_fs%>% as.data.frame()  # %>% mutate(LE_KG_TOT = select(.,idxkg) %>% rowSums(., na.rm = TRUE) ) %>% select ( -c ( idxkg, idxval) ) %>% mutate(LE_EURO_TOT = NA ) %>% as.data.frame()       
      
    } else if (species_analysis_type == 'selected_species_sum') {
      
      ## Option 1.2 : by selected species totals
      eflalo_fs_tot = eflalo_fs %>%   mutate(LE_KG_TOT = select(.,contains(c('KG_SOL', 'KG_MAC')) ) %>% rowSums(., na.rm = TRUE) ) %>% select ( -c ( idxkg, idxval) ) %>% as.data.frame()       
      
    } else if (species_analysis_type == 'selected_species_separated') {
      
      ## Option 1.3 : Analysis by species separately 
      eflalo_fs_tot = eflalo_fs %>% select(., -idxkg , -idxval, contains(c('SOL', 'MAC') ) ) %>% filter_at ( vars (contains( c('SOL', 'MAC') ) ) , any_vars(!is.na(.)) ) %>%  as.data.frame()
      
    }
    
    dim( eflalo_fs_tot)
    
    
    
  }  else if   (eflalo_format == 'long'  ) { 
    
    
    ## Option 2: Species names in long format. All  species in LE_SPE and  catch value in LE_KG columns
    
    
    captures_total = eflalo_fs %>% group_by(FT_REF, LE_ID) %>% summarise(LE_KG_TOT = sum( LE_KG), LE_EURO_TOT = 0 ) # LE_KG_TOT = sum( LE_KG) if VALUE data is available
    
    
    eflalo_fs_tot = eflalo_fs %>% select(-c (LE_SPE, LE_KG, LE_VALUE)) %>% distinct() %>% inner_join(captures_total , by = c("FT_REF", "LE_ID" ))
    
  } 

  tacsatp %>% select(SI_STATE) %>% filter(SI_STATE == 'f') %>% tally()
  
  tacsatp = tacsatp %>% mutate( FT_REF = SI_FT )
  
  eflaloM =  subset(eflalo_fs_tot,FT_REF %in% unique(tacsatp$FT_REF))
  eflaloNM = subset(eflalo_fs_tot,!FT_REF %in% unique(tacsatp$FT_REF))
  
  
  tacsatp  = tacsatp %>% mutate ( SI_STATE = ifelse (SI_STATE == 'f', 1, 0   )  )  
  
  tacsatp %>% select(SI_STATE) %>% filter(SI_STATE == '1') %>% tally()
  
  
  ##Filter only records when vessel is detected as fishing 
  
  tacsatEflalo  = tacsatp  %>% filter( SI_STATE == 1) 
  
  
  tacsatEflalo = tacsatEflalo %>% filter ( FT_REF %in%  ( eflaloM%>%distinct(FT_REF)%>%pull())   )
  
  
  ## if LE_KG or LE_VALUE is NA , replace by 0s'
  
  
  #eflaloM = eflaloM %>% mutate ( LE_VALUE_TOT = 0 )
  #eflaloM = eflaloM %>% mutate ( LE_EURO_TOT = LE_VALUE_TOT   )
  
  tacsatEflalo  = tacsatEflalo %>% as.data.frame()
  eflaloM = eflaloM%>% as.data.frame()
  
  
  #- Split among ping the landings to iVMS locations
  
  
  
  tacsatEflalo =  splitAmongPings(
    tacsat = tacsatEflalo,
    eflalo = eflaloM,
    variable = "all", # "kgs",
    level = "day",
    conserve = FALSE
  )
  
  
  tacsatEflalo$Csquare_05   =  CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  tacsatEflalo$Year         =  year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month        =  month(tacsatEflalo$SI_DATIM)
  tacsatEflalo$kwHour       =  as.numeric(tacsatEflalo$VE_KW) * tacsatEflalo$INTV  
  #tacsatEflalo$cpue        =  tacsatEflalo$LE_KG_TOT / tacsatEflalo$INTV   
  tacsatEflalo$INTV         =  tacsatEflalo$INTV 
  
  
  
  save(tacsatEflalo, file = paste0(".\\workflow_outputs\\srf-2\\tacsatEflalo_", analysis_type, "_", year, "_SRF_2_FDP.RData"))
  
  # 2.5 Assign  year, month, quarter, area and create table 2 ----------------------------------------
  
  
  
  eflalo_output = eflalo_fs_tot
  
  eflalo_output$Year      = year(eflalo_output$FT_LDATIM)
  eflalo_output$Month     = month(eflalo_output$FT_LDATIM)
  eflalo_output$INTV      = 1 # 1 day
  
  eflalo_output = eflalo_output %>% group_by(VE_REF, LE_CDAT) %>% mutate ( nr = row_number() ) %>% 
    mutate ( nr = max(nr) ) %>% 
    mutate ( INTV = INTV / nr)   %>% 
    arrange ( VE_REF , LE_CDAT)  %>% as.data.frame()
  
  
  
  
  eflalo_output$kwDays =  as.numeric(eflalo_output$VE_KW) * eflalo_output$INTV
  eflalo_output$tripInTacsat = ifelse(eflalo_output$FT_REF %in% tacsatp$FT_REF, "Y", "N") # Y = Yes and N = No
  
  
  save(eflalo_output,file = paste0("workflow_outputs\\srf-2\\eflalo_output_", analysis_type, "_", year, "_SRF_2_FDP.RData"))
  
  print(paste0("Finished ", year))

}
