options(dplyr.width = Inf, dplyr.print_min = 500)
library(vmstools)
library(dplyr)
library(ggplot2)
library(sf)

## Save the intermediate EFLALO and TACSAT datasets

getwd()

# if you opened from the file explorer, you will likele need this, may have to edit, needs to be pointed to data folder
setwd('./../../data')


load(".\\workflow_outputs\\eflalo_fs_ww.RData")
load(".\\workflow_outputs\\tacsat_fs_ww.RData")


#### QUALITY CONTROL:  Clean data with potential  outlines ########



# 1. Clean the EFLALO (CR)  data  ============================================================================


## 1.1  Number of EFLALO records and fishing Log Events records

## Q1.1: How many overall records are by  trip?  

res2 = eflalo_fs%>%group_by(FT_REF) %>% summarise(n = n()) %>% arrange(desc(n))
ggplot(res2, aes( n )) + geom_histogram()


## Q1.2: How many log events are by  trip?  ( LOG EVENTS ONLY EXIST IN E-LOGBOOKS from GEOFISH SOURCE / CR does not collect LOG EVENTS)


res21= eflalo_fs%>%distinct (FT_REF, LE_ID , trip_days) %>%group_by(FT_REF, trip_days)%>%summarise(n = n())%>%arrange(desc(n))
ggplot(res21, aes( n )) + geom_histogram()

 
# Q1.2.1 . How we assign GEAR and ICES RECTANGLE to iVMS/VMS TACSAT records . Example of different cases: 


      ###  1st: FISHING TRIP WITH > 1 GEARS REPORTED 

        ## Identify trips with more than 1 gear 

         trips_with_more_than_1_gear = eflalo_fs %>% distinct(FT_REF, LE_GEAR )%>% group_by(FT_REF ) %>% mutate ( rn = row_number ( ) ) %>% filter ( rn > 1) 
         trips_with_more_than_1_gear%>% arrange( rn , FT_REF , LE_GEAR)
         
         eflalo_fs %>% filter( FT_REF %in%  ( trips_with_more_than_1_gear %>%select (FT_REF ) %>%  pull() ) ) %>% distinct (FT_REF , LE_GEAR) %>%  arrange ( FT_REF )
      
         
         
         ## Catch Recording can collect multiple gears used during a fishing trip and the related Statistical Rectangle and species captured with these gears.
         ## This is what define the LOG EVENT in CR and would be the most equivalent to Electronic  Log Book Log Events. 

      ## Look at more detail to a specific trip . Replace the FT_REF number with results list from previous line of code. 
         
         eflalo_fs %>% filter(FT_REF == 610715898 )%>%mutate ( tt = FT_DDATIM   + ( ( FT_LDATIM - FT_DDATIM )   / 2))
         
         
         ## SOLUTION: The best solution for those fishing trips using more than 1 Gear is to unify them into a unique combined gear 
            ## So these trips will have a merged gear and metier defined: e.g. a trip using GN and FPO would have a gear defined as : GN_FPO
         
         eflalo_fs_mult_gears = eflalo_fs %>% filter(FT_REF %in% trips_with_more_than_1_gear$FT_REF )
         
         trips_mult_gears_comb = eflalo_fs_mult_gears %>% 
                                 distinct(FT_REF, LE_GEAR, LE_MET ) %>% arrange( FT_REF , LE_GEAR, LE_MET) %>%
                                 group_by( FT_REF  )  %>% 
                                 mutate ( LE_GEAR_C = paste0  ( LE_GEAR, collapse =  "_"), LE_MET_C =  ifelse( LE_MET != 'NULL',  paste0  ( LE_MET, collapse =  "_"), LE_MET  ) ) %>%
                                 select (FT_REF, LE_GEAR_C, LE_MET_C )
         
         eflalo_fs_mult_gears %>% inner_join(trips_mult_gears_comb, by  = 'FT_REF')
         
         
         eflalo_fs %>% filter(FT_REF %in% trips_with_more_than_1_gear$FT_REF ) %>%  group_by( FT_REF  ) %>%mutate (LE_KG_TOT  = sum ( LE_KG)) %>%
           group_by( FT_REF, LE_GEAR ) %>% reframe ( LE_KG_TOT = LE_KG_TOT, LE_KG_TOT_GEAR =  sum ( LE_KG )) %>%
           distinct ( FT_REF , LE_GEAR, LE_KG_TOT, LE_KG_TOT_GEAR) %>% 
           mutate (LE_KT_PROP = LE_KG_TOT_GEAR / LE_KG_TOT  ) %>% print(n = 110 )
         
      eflalo_fs %>% filter(FT_REF == 610715898 )%>%mutate ( tt = FT_DDATIM   + ( ( FT_LDATIM - FT_DDATIM )   / 2))
         
      eflalo_sel = eflalo_fs %>% filter(FT_REF == 610715898 ) %>% as.data.frame()
      
      tacsat_sel = tacsat_fs %>% filter(SI_FT == 610715898 & SI_SP >=0 & SI_SP <= 5) %>% arrange( SI_DATIM) %>% left_join( eflalo_sel %>% filter(FT_REF == 610715898), by = c("SI_FT" = "FT_REF", "SI_DATE" = "LE_CDAT"))
      
      tacsat_plot = tacsat_fs %>% filter(SI_FT == 610809285  & SI_SP >=2 & SI_SP <= 4) %>% arrange( SI_DATIM) 
      
      ### 2 GEARS WITH VMS 
      eflalo_fs %>% filter(FT_REF == 610715898) %>% as.data.frame()


      ##NOT iVMS record associated to trips with more than 1 log event records( use of multiple gears) 
      
      
      ### EXAMPLE OF A TRIP WITH 3 DAYS DURATION AND ONE GEAR REPORTED
      
      eflalo_sel %>% filter(FT_REF == 610691287)
      eflalo_fs %>% filter(FT_REF == 610691287) %>%as.data.frame()
      


## Q1.3: Do we find a range of values that aren't realist when compared with the whole data stats?

## Select a trip with more record associated and explore the data 

eflalo_fs %>% filter(FT_REF == 610696950)




## 1.2 Duration of the fishing trips 

## Observe the trips with more days duration by data SOURCE . Identify outliers and potential errors in Departure and Landing dates. 

eflalo_fs %>% select(trip_days, SOURCE, FT_REF, FT_DDATIM,FT_LDATIM,  VE_LEN) %>% arrange( SOURCE, desc( trip_days )) %>% filter( SOURCE == 't3')

## Q2.1: If outliers have been identified? Should those entries be removed or can the data be fixed?

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

eflalo_fs %>%
  group_by(  LE_GEAR, LE_SPE) %>%
  arrange( LE_GEAR, LE_SPE,  LE_KG  ) %>%
  mutate (diff_le_kg =   lead ( log10 ( LE_KG ), n = 1) - log10 ( LE_KG) ) %>% #select (rr1, rr2)
  filter(  ! is.na ( diff_le_kg ) ) %>%              ## filter those values that are not NA ( NULL ) 
  summarise ( max = max(diff_le_kg) , min = min(diff_le_kg) ) %>%
  filter ( max > landingThreshold ) %>%
print(n=200)



## Explore those captures with high difference identified and decide if must be removed from records 

eflalo_fs %>% filter( LE_GEAR == 'GTR' & LE_SPE == 'SOL') %>% select ( LE_KG) %>% arrange( desc(LE_KG) )
eflalo_fs %>% filter(LE_GEAR == 'FPO' & LE_SPE == 'TGS') %>% select ( LE_KG)%>%arrange(desc(LE_KG))
eflalo_fs %>% filter(LE_GEAR == 'OTB' & LE_SPE == 'BSS') %>% select ( LE_KG)%>%arrange(desc(LE_KG))

## Explore specific fishing trip information for outliers values

eflalo_fs %>% filter( LE_GEAR == 'GTR' & LE_SPE == 'SOL' & LE_KG  > 1000)  
eflalo_fs %>% filter( FT_REF == 610699215 ) ## change trip id based in the analysis
## Filter values no required out 

 



# 1.4 Check for NA's in catch records ----------------------------------------------------------


eflalo_fs %>% filter(  is.na ( LE_KG ))

# 1.5 Check for NA's in time stamp  ----------------------------------------------------------------------------


eflalo_fs %>% filter( is.na(FT_DDATIM) | is.na(FT_LDATIM) )  




# 1.6  Remove non-unique trip numbers -----------------------------------------------------------------------------


duplicate_analysis = eflalo_fs%>%
  distinct(VE_REF , FT_REF,  FT_DDAT, FT_LDAT)%>%
  group_by(VE_REF, FT_DDAT, FT_LDAT)%>%
  summarise( number_trips = n() ) %>%
  mutate ( duplicated = ifelse( number_trips >  1, TRUE , FALSE    )  )  


## Explore duplicated trips details

## How many are there?
eflalo_fs %>% left_join (duplicate_analysis , by = c ( 'VE_REF', 'FT_DDAT', 'FT_LDAT'))%>%
  filter(duplicated == TRUE) %>% 
  arrange(VE_REF, FT_REF, LE_SPE) %>% length()

## Explore them 
eflalo_fs %>% left_join (duplicate_analysis , by = c ( 'VE_REF', 'FT_DDAT', 'FT_LDAT'))%>%
  filter(duplicated == TRUE) %>% 
  arrange(VE_REF, FT_REF, LE_SPE)



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

eflalo_fs %>% filter(trip_days <= 0 )


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

overlaps_analysis %>%
  filter( VE_REF == 'A17444' & FT_DDATIM > '2022-03-19' & FT_DDATIM < '2022-04-22'  ) %>%
  arrange( VE_REF, FT_DDATIM)

eflalo_fs%>%
  filter( VE_REF == 'A17444' & FT_DDATIM > '2022-03-19' & FT_DDATIM < '2022-04-22'  ) %>%
  group_by(FT_REF)%>%
  summarise(numb = n())

## Following this analysis we have found out that these trips are duplicated 
## The three overlapping trips have the same reported species and details

eflalo_fs %>% filter( FT_REF %in% c(10343311296 , 10343333399  ))
eflalo_fs %>% filter( FT_REF %in% c(610787207, 610791817))
eflalo_fs %>% filter( FT_REF %in% c(610738857, 610735265)) 



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

plot( land_4326)



## Define an object bbox ( bounding box )  with the area of interest 


aoi = st_bbox( c( xmin = -15, xmax = 3, ymax = 60, ymin = 47), crs = st_crs( 4326 )) ## Define our area of intenrest.  4326 is id for WGS1984 unprojected coordinate system 

europe_aoi = st_crop (x = land_4326, y = aoi)  ## clip/crop the whole European layer to our of interest

plot( europe_aoi)

head(eflalo_fs)

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

res1 %>%distinct(FT_DDAT)%>%pull()
res1 %>%distinct(VE_REF, FT_REF ) %>% group_by(VE_REF)%>%tally() %>%arrange(desc(n))
eflalo_fs %>%distinct(VE_REF)
res1  %>% distinct(VE_REF)
res1%>% summarise(min = min (FT_DDAT), max = max( FT_DDAT))

eflalo_fs %>% filter( VE_REF == 'C20757') %>% distinct(FT_REF)
tacsat_fs %>% filter( VE_REF == 'C20757') %>% distinct(SI_FT)

tacsat_fs %>% filter( VE_REF == 'C20757')%>%dim()

tacsat_fs %>% summarise(min = min (SI_DATE), max = max( SI_DATE))

    ## trips with not TACSAT are from dates when iVMS have not been implemented(16-02-2022 )


dim(tacsat_fs)

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



# 2.2.2 Remove duplicate records ---------------------------------------------------------------------- 


tacsat_fs%>%
  group_by(VE_REF, SI_LATI, SI_LONG , SI_DATIM ) %>%
  filter( n() > 1) %>%
  arrange(VE_REF, SI_DATIM, SI_LATI, SI_LONG)

## From the overlap trip analysis done with EFLALO we can identify if the duplicated VMS locations correspond to those overlapping trips 

overlaps_analysis %>% filter(FT_REF %in% c( '610736051', '610738638') ) 

##  This confirms that these trips overlaps and therefore the VMS location is duplicated and assigned to the both trips
## To fix this error will require correct overlapping trips and reassign the trip identifiers to VMS locations 





# 2.2.3 Remove points that cannot be possible -----------------------------------------------------------

#tacsat_bk = tacsat_fs
#tacsat_fs = tacsat_bk

tacsat_fs = tacsat_fs %>% filter(abs(SI_LATI) < 90 | abs(SI_LONG) < 180)
tacsat_fs = tacsat_fs %>% filter(SI_HE >= 0 | SI_HE <= 360)
tacsat_fs %>% filter(SI_SP > 30 ) 



# 2.2.4 Remove points which are pseudo duplicates as they have an interval rate < x minutes/second  ------------------

## Convert the TACSAT into a spatial object (SF package)

tacsat_fs_geom = tacsat_fs %>% st_as_sf( ., coords = c("SI_LONG" ,"SI_LATI"), crs = 4326 , remove = FALSE)

st_write( tacsat_fs_geom, dsn = ".\\workflow_outputs\\tacsat_fs.geojson", layer = "tacsat_fs.geojson")

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

str(tacsat_fs_geom) 

##Q3: Calculate the minimum , maximum  and mean intervals in our data 

tacsat_fs_geom%>%filter(!is.na(INTV))%>%group_by(SOURCE) %>% summarise(minIntv = min(INTV), maxIntv = max (INTV), meanIntv = mean(INTV))#*60


tacsat_fs_geom%>%filter(!is.na(INTV))%>%group_by(INTV)%>%tally()%>%arrange(desc(n))

##Q4: Explore the intervals with a histogram. Outliers are detected? Can you fix or remove the wrong records?

ggplot(tacsat_fs_geom%>%filter(INTV < 4), aes(INTV) ) + geom_histogram(bins = 50)

## Q5: Use the filter to explore the large intervals records. Take the VE_REF and SI_FT to explore the trip 

tacsat_fs_geom%>%filter(INTV >5 )%>%arrange(VE_REF, SI_DATIM)%>%print(n= 100)%>%select(VE_REF, SI_FT) %>%
as.data.frame()

##Q5.1: Can you check the resulting intervals for a given vessel and trip id           

tacsat_fs_geom%>%filter ( VE_REF == 'C20757' &  SI_FT == '610808700'  ) %>%arrange(SI_DATIM)

##Q5.2: Plot the locations of the given trip to understand spatial patterns of a given trip

tacsat_fs_geom%>%filter ( VE_REF == 'C20757' &  SI_FT == '610808700'  )%>%
  mutate(largeIntv   = ifelse(INTV > 1 , TRUE , FALSE)) %>%
  ggplot( ) + geom_sf(aes(color = largeIntv)) + 
  geom_sf_label ( aes(label = ifelse ( INTV > 30, round(INTV, 1), NA )),  nudge_x = 0.05  ) + theme_minimal()
  
  
  
  
  
  # 2.2.5 Remove points in harbour -----------------------------------------------------------------------------

##Q1: Are the vessel iVMS positions in/nearby a harbour?

## Use a SPATIAL JOIN to link spatially the iVMS locations within Port locations.
## The spatial relatioship is the intersection between a iVMS poitn and a polygon representing the area buffered 3Km around the port location

tacsat_fs_ports = tacsat_fs_geom %>%
  st_join ( port_500m %>% select ( Name, District_N) , join = st_intersects, left = T) %>%
  mutate  ( SI_HARB  = ifelse ( is.na ( Name  ), FALSE , TRUE))  %>%
  select ( - names(port_500m%>% select ( Name, District_N)))

 



getwd()
st_write( tacsat_fs_ports, dsn = paste0(".\\workflow_outputs\\spatial\\tacsat_port_welsh.geojson"), layer = "tacsat_port_welsh.geojson")      

##Q2: Plot the ports and iVMS locations when in port

ggplot() + 
  geom_sf ( data = welsh_marine_area) + 
  geom_sf(data = port_500m %>% filter (Name %in% c( 'Milford Haven', 'Cardigan') )) +
  geom_sf ( data = tacsat_fs_ports %>% slice(1:50000), aes( color  = SI_HARB ) )+ 
  theme_minimal() + 
  coord_sf( xlim = c(- 5.5, -4), ylim = c(51.6, 52.2) )

ggplot() + 
  geom_sf ( data = welsh_marine_area) + 
  geom_sf(data = port_500m %>% filter (Name %in% c( 'Milford Haven', 'Cardigan') )) +
  geom_sf ( data = tacsat_fs_ports %>% slice(1:50000), aes( color  = SI_HARB ) )+ 
  theme_minimal() + 
  coord_sf( xlim = c(- 5.5, -4), ylim = c(51.6, 52.2) )





# 2.2.6 Remove points on land -----------------------------------------------------------------------------




tacsat_fs_land = tacsat_fs_ports %>% 
  st_join( europe_aoi , join = st_intersects, left = T )%>%
  mutate  ( SI_LAND  = ifelse ( is.na ( Id ), FALSE ,TRUE))%>%
  select ( - names(europe_aoi) )  

st_write( tacsat_fs_land, dsn = ".\\workflow_outputs\\spatial\\tacsat_fs_land.geojson", layer = "tacsat_fs_land.geojson", append=FALSE )      


##Q1: How many points are detected on land? 

tacsat_fs_land%>%st_drop_geometry()%>%group_by(SI_LAND)%>%count()



##Q2: Plot the iVMS locations when in land

ggplot() + 
  geom_sf ( data = welsh_marine_area) + 
  #geom_sf(data = port_3km%>%filter (port %in% c( 'Milford Haven', 'Cardigan') )) +
  geom_sf ( data = tacsat_fs_land%>%slice(1:50000), aes(color  = SI_LAND ))+ theme_minimal() + 
  coord_sf(xlim = c(- 5.5, -4), ylim = c(51.6, 52.2))

## Remove the fields taken from the ports datasets . Not needed for our analysis example


tacsat_fs_land = tacsat_fs_land %>% mutate ( SI_STATE = ifelse  (  SI_SP  >= 1 & SI_SP <= 6 , 'f', 's'  ))


tacsat_fs_df = tacsat_fs_land %>% filter(SI_LAND == FALSE & SI_HARB == FALSE )


st_write( tacsat_fs_df, dsn = ".\\workflow_outputs\\tacsat_fs_df.geojson", layer = "tacsat_fs_df.geojson")








#   Save the cleaned EFLALO file 

  save(eflalo_fs, file = '.\\workflow_outputs\\eflalo_fs_qc_ww.RData')


#   Save the cleaned TACSAT file 


  tacsat_fs = tacsat_fs_df
  
  save(tacsat_fs, file = '.\\workflow_outputs\\tacsat_fs_qc_ww.RData')






## Check how match the reported LE_RECT in EFLALO and the actual rectangle the  VMS location is located

tac_geom = tacsat_fs_df %>% st_as_sf( ., coords = c("SI_LONG" ,"SI_LATI"), crs = 4326 )


eflalo_fs %>%  filter ( VE_REF == 'A16337') %>% distinct(LE_CDAT) %>% arrange(LE_CDAT)
ggplot(tac_geom) + geom_sf(aes( color = SI_DATE))



tacsat_fs_df_geom = tac_geom %>%
  st_join ( ices_rect_welsh%>% select ( icesname ) , join = st_intersects, left = T) %>%
  mutate  ( SI_RECT = icesname) 





#tacsat_fs_df_geom = tacsat_fs_df_geom %>% mutate ( RECT_MATCH = ifelse ( LE_RECT == SI_RECT, TRUE , FALSE ))%>%st_drop_geometry()
#tacsat_fs_df_geom = tacsat_fs_df_geom %>%st_drop_geometry()


#sum_stat %>%st_drop_geometry() %>% group_by(RECT_MATCH) %>% tally()


#tac_geom_rect = tac_geom %>% group_by(SI_DATE)%>%distinct(icesname, SI_RECT)


#st_write( tac_geom_rect, dsn = ".\\..\\..\\data\\example_trip_3_days_1_LE.geojson", layer = "example_trip_3_days_1_LE.geojson")






