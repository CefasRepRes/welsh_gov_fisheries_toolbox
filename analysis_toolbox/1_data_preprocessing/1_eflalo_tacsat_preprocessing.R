options(dplyr.width = Inf, dplyr.print_min = 500)
library(vmstools)


#### QUALITY CONTROL:  Clean data with potential  outliers ########



# 1. Clean the EFLALO data  ============================================================================


  ## 1.1  Number of EFLALO records and fishing Log Events records

    ## Q1.1: How many overall records are by  trip?  
    
     res2 = eflalo_gbw%>%group_by(FT_REF) %>% summarise(n = n()) %>% arrange(desc(n))
      ggplot(res2, aes( n )) + geom_histogram()
    
    ## Q1.2: How many log events are by  trip?  
    
    
    eflalo_gbw%>%distinct (FT_REF, LE_ID , trip_days) %>%group_by(FT_REF, trip_days)%>%summarise(n = n())%>%arrange(desc(n))
    
    ## Q1.3: Do we find a range of values that aren't realist when compared with the whole data stats?
    
      ## Select a trip with more record associated and explore the data 
      
      eflalo_gbw %>% filter( FT_REF == 10343309346      )



  
  ## 1.2 Duration of the fishing trips 
  
  ## Observe the trips with more days duration . Identify outliers and potential errors in Departure and Landing dates. 
  
  eflalo_gbw %>% select(trip_days, FT_REF, FT_DDATIM,FT_LDATIM,  VE_LEN) %>% arrange( desc( trip_days ))
  
  ## Q2.1: If outliers have been identified? Should be removed those entried records or data can be fixed?
  
  ## Filter out those outlier trips durations
  
   trips_in = eflalo_gbw%>%filter(trip_days <= 10)%>%select(FT_REF)%>%pull()

   eflalo_gbw = eflalo_gbw%>%filter(FT_REF %in% trips_in)
   
    ## Q2.2: Why we have done a 2 steps filtering?
   
   
   
   
   
    
   # 1.3 Warn for outlying catch records ----------------------------------------------------------
   
    ## Select a logarithm scale landing weight threshold ( e.g. 1.5)
   
   landingThreshold = 1.5
   
   ## Identify the records with species captures over expected thresholds 
   
      eflalo_gbw%>%group_by(  LE_GEAR, LE_SPE)%>%arrange( LE_GEAR, LE_SPE,  LE_KG  )%>%
      mutate (diff_le_kg =   lead (log10(LE_KG), n = 1) - log10 ( LE_KG) ) %>% #select (rr1, rr2)
      filter(!is.na(diff_le_kg))%>%
      summarise(max = max(diff_le_kg), min = min(diff_le_kg) ) %>%
      filter(max > landingThreshold)
      print(n=200)
     
     
     
     ## Explore those captures with high difference identified and decide if must be removed from records 
     
     eflalo_gbw%>%filter(LE_GEAR == 'GTR' & LE_SPE == 'SOL')%>%select ( LE_KG)%>%arrange(desc(LE_KG))
     eflalo_gbw%>%filter(LE_GEAR == 'FPO' & LE_SPE == 'TGS')%>%select ( LE_KG)%>%arrange(desc(LE_KG))
     
   
  # 1.4 Check for NA's in catch records ----------------------------------------------------------
     
    
     eflalo_gbw%>%filter(is.na(LE_KG))
    
  # 1.5 Check for NA's in time stamp  ----------------------------------------------------------------------------
     
     
     eflalo_gbw%>%filter(is.na(FT_DDATIM) | is.na(FT_LDATIM))
     
     
  
  
   # 1.6  Remove non-unique trip numbers -----------------------------------------------------------------------------
   
     
     duplicate_analysis =   eflalo_gbw%>%distinct(VE_REF,FT_REF,  FT_DDAT, FT_LDAT)%>%
                            group_by(VE_REF, FT_DDAT, FT_LDAT)%>%
                            summarise(number_trips = n())%>%
                            mutate(duplicated = ifelse( number_trips >  1, TRUE , FALSE) )
     
     ## Explore duplicated trips details
     
     eflalo_gbw%>%left_join (duplicate_analysis , by = c ( 'VE_REF', 'FT_DDAT', 'FT_LDAT'))%>%
              filter(duplicated == TRUE)%>%arrange(VE_REF, FT_REF, LE_SPE)
     
     ## Identify the trip id's that are duplicated 
     
      duplicate_trips = eflalo_gbw%>%
                         left_join (duplicate_analysis , by = c ( 'VE_REF', 'FT_DDAT', 'FT_LDAT'))%>%
                         group_by(FT_REF)%>%mutate ( number_records = n ())%>%  ungroup()%>%
                         filter(duplicated == TRUE)%>%
                         distinct(VE_REF, FT_REF, FT_DDAT ,  FT_LDAT , number_records)%>%
                         arrange(VE_REF,   FT_DDAT ,  FT_LDAT , desc(number_records) )%>%ungroup()%>%
                         group_by(VE_REF, FT_DDAT,FT_LDAT)%>%
                         mutate(r_num = row_number() )%>%
                         filter(r_num > 1)%>%
                         select(FT_REF)%>%
                         pull()
     
      ## Filter out the duplicated trips
      
      eflalo_gbw =   eflalo_gbw %>%
                    filter( ! FT_REF %in% duplicate_trips   ) 
   
    
   
    
   
   
   # 1.7 Remove records with arrival date before departure date  ------------------------------------------------------------
   
     
      ## Use the field created with trip duration: trip_days
      ## A value of 0 or negative would means departure and landing dates are same or reversed
     
      eflalo_gbw%>%filter(trip_days <= 0 )
   
   
   # 2.3.7 Remove trip with overlap with another trip --------------------------------------------------------------------------- 
   
    ## Identify the vessel trips that have a departure date overlapping with previous return/landing date
   
   
      overlaps_analysis =  eflalo_gbw%>%distinct(VE_REF, FT_REF, FT_DDATIM, FT_LDATIM)%>%
                           arrange(VE_REF, FT_DDATIM)%>%
                           group_by(VE_REF)%>%
                           mutate( overlaps = FT_LDATIM > lead(FT_DDATIM)   )
     
     
     
     ## Check what vessel and trips dates are overlapping
     
       overlaps_analysis%>%
       filter ( overlaps == TRUE)
       
       
   
   
       ## Filter the trips details for the vessel during the overlapping dates 
       
       overlaps_analysis%>%
       filter( VE_REF == 'A15264' & FT_DDATIM > '2022-10-20' & FT_DDATIM < '2022-11-20'  ) %>%
         arrange( VE_REF, FT_DDATIM)
       
       eflalo_gbw%>%
         filter( VE_REF == 'A15264' & FT_DDATIM > '2022-10-20' & FT_DDATIM < '2022-11-20'  ) %>%
         group_by(FT_REF)%>%
         summarise(numb = n())
       
       ## Following this analysis we have found out that these trips are duplicated 
       ## The three overlapping trips have the  same reported species and details
       
       eflalo_gbw%>%filter( FT_REF %in% c( '10343392884', '10343392891' ))
     
        
      ## Q1: What are you doing with the overlapping trips? Do we remove them or fix them?
        ## This will impact the VMS locations associated to these trips , so to avoid duplication and conflicts , correct or delete overlapping trips
        ## In the example above the ideal solution would be remove 2 out of the 3 duplicated trips recorded
   
   

       
       
       
# 2 Clean  TACSAT data  ----------------------------------------------------------------------------------
       
       
# 1.1 Load spatial auxiliary data ===========================================

       
## Define an object bbox with the area of interest 
aoi = st_bbox(c(xmin = -15, xmax = 3, ymax = 60, ymin = 40), crs = st_crs(4326))
       
welsh_marine_area = st_read(dsn = '.\\..\\data\\wales_plan_area.geojson')
port_3km = st_read(dsn = '.\\..\\data\\mmo_landing_ports_3km_buffer.geojson')
land = st_read(dsn = '.\\..\\data\\Europe_coastline_poly.shp')
land_4326 = land%>%st_transform(4326)


europe_aoi = st_crop(x = land_4326, y = aoi)

plot( europe_aoi)
 
 
 
 

# 2 Clean the TACSAT and EFLALO data  ----------------------------------------------------------------------------------
 
   

## Filter TACSAT data with the trips result from cleaning EFLALO data

tacsat_gbw = tacsat_gbw%>%filter(SI_FT %in% ( eflalo_gbw%>%distinct(FT_REF)%>%pull()) ) 
  
  
  # 2.2 Take only VMS pings in the ICES areas ==============================================
  
  ia <- ICESareas%>% 
    sf::st_as_sf() %>% 
    sf::st_make_valid() %>% 
    sf::st_transform(4326)  
  overs <- 
    tacsat  %>% 
    sf::st_as_sf(coords = c("SI_LONG", "SI_LATI")) %>% 
    sf::st_set_crs(4326) %>% 
    sf::st_intersects(ia)
  
  tacsat <- tacsat[lengths(overs) > 0,]
  
  
 
  #   2.2 Clean the tacsat data  ============================================================================
  
  
  
  # 2.2.2 Remove duplicate records ---------------------------------------------------------------------- 
  
  
  tacsat_gbw%>%
    group_by(VE_REF, SI_LATI, SI_LONG , SI_DATIM )%>%
    filter(n()> 1)%>%
    arrange(VE_REF, SI_DATIM, SI_LATI, SI_LONG)
  
  ## From the overlap trip analysis done with EFLALO we can identify if the duplicated VMS locations correspond to those overlaping trips 
  
  overlaps_analysis %>%filter(FT_REF %in% c( '10343392891', '10343392884') ) 
  
  ##  This confirms that these trips overlaps and therefore the VMS location is duplicated and assigned to the both trips
  ## To fix this error will require correct overlaping trips and reasign the trip identifiers to VMS locations 
  
  
    
  
  
  # 2.2.3 Remove points that cannot be possible -----------------------------------------------------------
  
  
  
  tacsat_gbw%>%filter(abs(SI_LATI) > 90 || abs(SI_LONG) > 180)
  tacsat_gbw%>%filter(SI_HE < 0 || SI_HE > 360)
  tacsat_gbw%>%filter(SI_SP > 25 ) 
  
  
  
  # 2.2.4 Remove points which are pseudo duplicates as they have an interval rate < x minutes ------------------
  
  ## Convert the TACSAT into a spatial object (SF package)
  
  tacsat_gbw_geom = tacsat_gbw%>%ungroup()%>%st_as_sf(., coords = c("SI_LONG" ,"SI_LATI"), crs = 4326 )
  
    ## Q1: What is the minimum expected time interval between iVMS positions
  
    ## Use that value to filter potential pseudo-duplicates ( values below the minimum expected interval)
  
  minInterval = 1 / 60 ## 1 minute converted in hours (0.01666667 hours)
  
  tacsat_gbw_geom = tacsat_gbw_geom%>%ungroup()%>%
                    group_by(VE_REF, SI_FT)%>% arrange (VE_REF, SI_DATIM)%>%
                    mutate (INTV =  difftime(SI_DATIM , lag(SI_DATIM), units = "hours" )  )%>% ## Calcualte the difference between a iVMS loction time stamp and previous location to calcualte a fishign effort in a given location
                    mutate(interval_mean = mean(INTV , na.rm = T))%>% ## Calcualte the mean to replace the NA's interval when a VMS location is the 1st of a trip and cannot calculate with a prev. iVMS location
                    mutate(INTV = ifelse(is.na(INTV), interval_mean, INTV ))%>%  ## Convert the NA's into a effort represented by the mean of that vessel durign given trip
                    select(- interval_mean) %>%ungroup()
  
  

      ##Q2: Check the structure of TACSAT . Did the field types changed?
  
      str(tacsat_gbw_geom) 
      
      ##Q3: Calculate the minimum , maximum  and mean intervals in our data 
      
      tacsat_gbw_geom%>%filter(!is.na(INTV))%>%summarise(minIntv = min(INTV), maxIntv = max (INTV), meanIntv = mean(INTV))#*60
  
      ##Q4: Explore the intervals with a histogram. Outliers are detected? Can you fix or remove the wrong records?
      
      ggplot(tacsat_gbw_geom%>%filter(INTV < 1), aes(INTV) ) + geom_histogram(bins = 50)
      
      ## Q5: Use the filter to explore the large intervals records. Take the VE_REF and SI_FT to explore the trip 
      
      tacsat_gbw_geom%>%filter(INTV >50 )%>%arrange(VE_REF, SI_DATIM)%>%print(n= 100)%>%select(VE_REF, SI_FT)
        as.data.frame()
      
        ##Q5.1: Can you check the restultd intervals for a given vessel and trip id           
        
      tacsat_gbw_geom%>%filter ( VE_REF == 'C21140' &  SI_FT == '10343398870'  ) %>%arrange(SI_DATIM)
        
        ##Q5.2: Plot the location sof teh given trip to udnerstand spatial patterns of a given trip
        
          tacsat_gbw_geom%>%filter ( VE_REF == 'C21140' &  SI_FT == '10343398870'  )%>%
          mutate(largeIntv   = ifelse(INTV > 1 , TRUE , FALSE))%>%
          ggplot( ) + geom_sf(aes(color = largeIntv)) + 
          geom_sf_label ( aes(label = ifelse ( INTV > 30, round(INTV, 1), NA )),  nudge_x = 0.05  ) + theme_minimal()
        
  
  

  
  # 2.2.5 Remove points in harbour -----------------------------------------------------------------------------
  
  ##Q1: Are the vessel iVMS positions in/nearby a harbour?
          
    ## Use a SPATIAL JOIN to link spatially the iVMS locations with Port locations.
    ## The spatial relatioship is the intersection between a iVMS poitn and a polygon representing the area buffered 3Km around the port location
          
  tacsat_gbw_ports = st_join(tacsat_gbw_geom, port_3km, join = st_intersects, left = T)%>%
                     mutate  ( SI_HARB  = ifelse ( is.na (port), FALSE , TRUE))%>%
                     select ( - names(port_3km))

 
          
          
    ##Q2: Plot the ports and iVMS locations when in port
          
        ggplot() + 
        geom_sf ( data = welsh_marine_area) + 
        geom_sf(data = port_3km%>%filter (port %in% c( 'Milford Haven', 'Cardigan') )) +
        geom_sf ( data = tacsat_gbw_ports%>%slice(1:50000), aes(color  = SI_HARB ))+ theme_minimal() + 
        coord_sf(xlim = c(- 5.5, -4), ylim = c(51.6, 52.2))
      
   
 
  
  
  # 2.2.6 Remove points on land -----------------------------------------------------------------------------
  
  

  
    tacsat_gbw_land = st_join(tacsat_gbw_ports, land_4326, join = st_intersects, left = T)%>%
      mutate  ( SI_LAND  = ifelse ( is.na (Id), FALSE ,TRUE))%>%
      select ( - names(land_4326))  
        
      
    ##Q1: How many points are detected on land? 
        
    tacsat_gbw_land%>%st_drop_geometry()%>%group_by(SI_LAND)%>%count()
    
    
    
    ##Q2: Plot the iVMS locations when in land
    
    ggplot() + 
      geom_sf ( data = welsh_marine_area) + 
      #geom_sf(data = port_3km%>%filter (port %in% c( 'Milford Haven', 'Cardigan') )) +
      geom_sf ( data = tacsat_gbw_land%>%slice(1:50000), aes(color  = SI_LAND ))+ theme_minimal() + 
      coord_sf(xlim = c(- 5.5, -4), ylim = c(51.6, 52.2))
    
    ## Remove the fields taken from the ports datasets . Not needed for our analysis example
    
    
    
    tacsat_gbw_df = tacsat_gbw_land%>% filter(SI_LAND == FALSE )
  
  
  
  
  
  #   Save the cleaned EFLALO file 
    
   
  
  save(
    eflalo_gbw, file = '.\\..\\data\\eflalo_gbw.RData'
  )


  #   Save the cleaned TACSAT file 
  

    tacsat_gbw = tacsat_gbw_df
  
  save(
    tacsat_gbw_df, file = '.\\..\\data\\tacsat_gbw.RData'
  )
  

