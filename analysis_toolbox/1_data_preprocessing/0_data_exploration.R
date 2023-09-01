########### DATA EXPLORATION, QUALITY CONTROL AND CLEANING ##########################

library(dplyr)
library(ggplot2)

setwd('./../../data')

load("./workflow_outputs/eflalo.RData")
load("./workflow_outputs/tacsat.RData")

### Define the fleet segment to be analysed from the Analysis Option chosen in "0_DATA_ACCESS" toolbox section.

## Fleet segment: 
## - Over 12 m vessels ( source GeoFISH)
## - Under 12 m vessels ( source T3)
## - Combined O12m and U12m fleets

### If you want to analyse a specific section of the data, apply the following if statement

fleet_segment = 'over12' ## replace for 'welsh_waters'  if needed

if ( fleet_segment == 'over12')  { 
  
  ###  1. Over 12m vessels analysis
  
  eflalo_fs = eflalo %>% filter(VE_LEN >= 12)
  tacsat_fs = tacsat %>% filter(VE_REF %in% eflalo_fs$VE_REF)
  #tacsat = tacsat %>% filter(SI_FT %in% eflalo$FT_REF)
  
  
} else if  (fleet_segment == 'under12' ) { 
  
  
  ### 2. Under 12m vessels analysis 
  
  eflalo_fs = eflalo %>% filter(VE_LEN < 12)
  tacsat_fs = tacsat %>% filter(VE_REF %in% eflalo_fs$VE_REF)
  
} else if  (fleet_segment == 'all' ) { 
  
  
  ### 3. All vessels 
  
  eflalo_fs = eflalo 
  tacsat_fs = tacsat  
  
} 



#### EXPLORATION:  Initial EFLALO and TACSAT exploration ########

## Explore the stats from EFLALO to get fleet statistics and fleet characterisation


  ## Explore vessels nationality in the EFLALO records 

  eflalo_fs %>% distinct(VE_COU)
  eflalo_fs %>% distinct(FT_DCOU)
  eflalo_fs %>% distinct(VE_REF)
  
  ## Filter (if necessary) the vessels from nationality required using the Vessel Country field (VE_COU)
  # The country code can be changed to your requirements, this is not editing the data, just exploring it
  eflalo_fs %>% filter(VE_COU == 'GBE')
  
  
  ## Check the size change in dimensions of the dataset you chose
  
  eflalo_fs %>% distinct(FT_REF) %>% dim()
 
  
  ## Check vessel length categories
  
  eflalo_fs %>% select(VE_LEN)%>%mutate( VE_LEN = as.numeric(VE_LEN)) %>% summary()
  eflalo_fs %>% distinct(VE_REF, VE_LEN) %>% mutate( VE_LEN = round(VE_LEN, 0 )) %>% group_by(VE_LEN) %>% tally() %>% ggplot(., aes(VE_LEN, n)) + geom_bar(stat = "identity") +scale_x_continuous( breaks = seq(1:32) )
  eflalo_fs %>% ggplot( . , aes( VE_LEN)) + geom_histogram()
  
  
  
  
  ## Create the VESSEL LENGTH CAGTEGORY to explore data by vessel length classes
  
  eflalo_fs$VE_LEN_CAT = cut(eflalo_fs$VE_LEN , include.lowest = T,
                               breaks=c(-Inf, 4.5, 4.999 , 6.999, 8.999, 11.999 , Inf), 
                               labels=c("<4.5m","4.5-5m", "5-7m", "7-9m", "9-12m", "=>12m"))
  

  eflalo_fs %>% select ( FT_REF , VE_REF, VE_LEN, VE_LEN_CAT)
  
  
  ## Create the field "trip_days" with duration of each trips as   number of days  

  eflalo_fs$trip_days = as.numeric(eflalo_fs$FT_LDATIM - eflalo_fs$FT_DDATIM)
  head(eflalo_fs$trip_days)
  str(eflalo_fs)
  
  head(eflalo_fs)
  
    
   

  ## Plot the categories to understand your fleet composition
  
    ## PLOT 1: Number of trips by vessel length category 
    
    ggplot(data = eflalo_fs %>% distinct(FT_REF, VE_LEN_CAT), aes(VE_LEN_CAT)) +
    geom_bar()
    
    ## PLOT 2: Number of trips by gear category 
    
    ggplot(data = eflalo_fs%>%distinct(FT_REF, LE_GEAR), aes(LE_GEAR)) +
    geom_bar()
    
    ## PLOT 2: Number of trips by gear category, using facet wrap for each length category
    
    ggplot(data = eflalo_fs%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR), aes(LE_GEAR)  ) +
      geom_bar() + 
      facet_wrap( ~ VE_LEN_CAT)
    
    ## Now for each geat type
    
    ggplot(data = eflalo_fs%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR) , aes(VE_LEN_CAT)  ) +
      geom_bar() + 
      facet_wrap( ~ LE_GEAR)

    
    ## PLOT 3: Main species captured
    
      ## Rank the main species captured by gear type
    
    res1 = eflalo_fs %>%
    group_by(  LE_GEAR, LE_SPE) %>%
    summarise(le_kg_total = sum( LE_KG) )  %>%
    mutate ( rank = dense_rank( desc(le_kg_total) )) %>%
    arrange(LE_GEAR, desc(le_kg_total) ) %>%
    filter(rank <= 5) %>%
    ungroup() 
    
   
    ggplot( data = res1 , aes( y =le_kg_total, x = LE_SPE)) + 
      geom_bar(stat = "identity") + 
      facet_wrap(~ LE_GEAR, scales = "free" )
    
    
    ## PLOT 4: Fishing trip duration and fish duration by vessel length. 
    
      #Q1: Do we expect longer trips from larger vessels?
      #Q2: Is there any trips duration identified as outliers? 
    
    ggplot(data = eflalo_fs %>% filter( trip_days < 500 ) ) +
    geom_histogram( aes( trip_days ) )
    
    ggplot(data = eflalo_fs %>% filter(trip_days < 50) , aes ( x = trip_days, y = VE_LEN )) + 
    geom_point(  )
    
    
    write.csv( x = res1, file =  ".\\workflow_outputs\\species_kg_ranked_by_gear.csv", row.names=FALSE)
    
    
    ## Save the intermediate EFLALO and TACSAT datasets
    
    save ( eflalo_fs , file = paste0('.\\workflow_outputs\\eflalo_fs_', fleet_segment, '.RData' ))
    save ( tacsat_fs , file = paste0('.\\workflow_outputs\\tacsat_fs_', fleet_segment, '.RData' ))
    
    