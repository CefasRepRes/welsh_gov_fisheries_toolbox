########### DATA EXPLORATION, QUALITY CONTROL AND CLEANING ##########################


#### EXPLORATION:  Initial EFLALO and TACSAT exploration ########

## Explore the stats from EFLALO to get fleet statistics and fleet characterization


  ## Explore vessels nationality in the EFLALO records 
  
  eflalo %>% distinct(VE_COU)
  eflalo%>%distinct(VE_FA)
  eflalo%>%distinct(FT_DCOU)
  
  ## Filter (if necessary) the vessels from nationality required using the Fishing Authority field (VE_FA)
  
  eflalo_gbw = eflalo %>% filter( VE_FA == 'Wales')  ##Vessel Fishing authority 
  
  eflalo_wz = eflalo %>%    ## Spatial filter , any fishign trip in eflalo with a ICES rect within Welsh Zone 
  
  ## Compare the size change in dimensions of the filtered and non-filtered dataset
  
  eflalo %>% distinct( FT_REF) %>% dim() 
  eflalo_gbw %>% distinct(FT_REF)%>%dim() 
  
  
  ## We need to filter the TACSAT data that is related to the filtered trips 
  
  
  ft_eflalo_gbw = eflalo_gbw %>% select(FT_REF) %>% pull()  ## get the unique FT id's in the eflalo with welsh fleet records

  tacsat_gbw = tacsat %>% filter( SI_FT %in%   ft_eflalo_gbw )  ## filter TACSAT (ivms) for welsh fleet trips in eflalo
  
  dim(tacsat)
  dim(tacsat_gbw)
  
  
  ## Create the VESSEL LENGTH CAGTEGORY to explore data by vessel length classes
  
  eflalo_gbw$VE_LEN_CAT = cut( eflalo_gbw$VE_LEN , include.lowest = T,
                          breaks=c(-Inf, 4.5, 5 , 6, 7, 8, 9, 10 , Inf), 
                          labels=c("<4.5 m","4.5 - 5 m", "5- 6 m","6 - 7 m", "7 - 8 m", "8 - 9 m", "9 - 10 m", "=> 10  m"))
  
  eflalo_gbw$VE_LEN_CAT = cut( eflalo_gbw$VE_LEN , include.lowest = T,
                               breaks=c(-Inf, 4.5, 5 , 7, 9, 10 , Inf), 
                               labels=c("<4.5 m","4.5 - 5 m", "5 - 7 m", "7 - 9 m", "9 - 10 m", "=> 10  m"))
  

  eflalo_gbw %>% select ( FT_REF , VE_REF, VE_LEN, VE_LEN_CAT)
  
  ## Create the field "trip_days" with duration of each trips as   number of days  
  
  eflalo_gbw$trip_days  =  as.numeric( eflalo_gbw$FT_LDATIM -  eflalo_gbw$FT_DDATIM )
  
  head( eflalo_gbw )
  
    
   

  ## Plot the categories to understand your fleet composition
  
    ## PLOT 1: Number of trips by vessel length category 
    
    ggplot( data = eflalo_gbw %>% distinct(FT_REF, VE_LEN_CAT) ,   aes( VE_LEN_CAT )   ) +
    geom_bar()
    
    ## PLOT 2: Number of trips by gear category 
    
    ggplot( data = eflalo_gbw%>%distinct(FT_REF, LE_GEAR)  , aes( LE_GEAR ) ) +
    geom_bar()
    
    ## PLOT 2: Number of trips by gear category 
    

    ggplot(data = eflalo_gbw%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR), aes(LE_GEAR)  ) +
      geom_bar() + 
      facet_wrap( ~ VE_LEN_CAT)
    
    ggplot(data = eflalo_gbw%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR) , aes(VE_LEN_CAT)  ) +
      geom_bar() + 
      facet_wrap( ~ LE_GEAR)

    ggplot(eflalo_gbw%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR), aes(LE_GEAR)) + 
      geom_bar() + 
      facet_wrap( ~ VE_LEN_CAT)

    
    ## PLOT 3: Main species captured
    
      ## Rank the main species captured by gear type
    
    res1 = eflalo_gbw %>%
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
    
    ggplot(data = eflalo_gbw %>% filter( trip_days < 500 ) ) +
    geom_histogram( aes( trip_days ) )
    
    ggplot(data = eflalo_gbw%>%filter(trip_days < 50) , aes ( x = trip_days, y = VE_LEN )) + 
    geom_point(  )
    
    
    write.csv( x = res1, file =  ".\\..\\data\\data_output\\species_kg_ranked_by_gear.csv", row.names=FALSE)
    
    
    ## Save the inermediate EFLALO and TACSAT datasets
    
    save ( eflalo_gbw , file = '.\\..\\data\\eflalo_gbw.RData' )
    save ( tacsat_gbw , file = '.\\..\\data\\tacsat_gbw.RData' )
    
    