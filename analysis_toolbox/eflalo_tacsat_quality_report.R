##### EFLALO TACSAT QUALITY CHECK #####

library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)

setwd('./../../data')
setwd("C:\\Users\\md09\\Documents\\git\\welsh_gov_fisheries_toolbox\\data")

load(".\\workflow_outputs\\eflalo.RData")
load(".\\workflow_outputs\\tacsat.RData")
load(".\\workflow_outputs\\tacsatEflalo_output_2022.RData")

# Prior to quality checking, the fleet segment must be selected

fleet_segment = 'under12' ## Options: ( 'over12', 'under12', 'all' )


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


#### EFLALO ####

### Vessel exploration

## Vessel origins

eflalo_fs %>% distinct(VE_COU) # Country of ownership
eflalo_fs %>% distinct(FT_DCOU) # Country of departure

eflalo_fs %>% filter(VE_COU == 'GBW') # Filter to a specified country of ownership

eflalo_fs %>% group_by(VE_COU, SOURCE )%>% tally ( ) %>% arrange(VE_COU) # Counts per data source


## Vessel lengths

# See how many entries there are where the vessel length is over 10m and the source is GeoFISH
# This number should be similar to the total count for GeoFISH
eflalo_fs %>% filter( VE_LEN >= 10  & SOURCE == 'geofish')
eflalo_fs %>% filter( VE_LEN >= 10  & SOURCE == 'geofish') %>% tally()

# Summary statistics for vessel lengths of the dataset
eflalo_fs %>% select(VE_LEN)%>%mutate( VE_LEN = as.numeric(VE_LEN)) %>% summary()

# Categorise data for further analysis - based on the fleet segment, assign a vessel length category to be used for plots and grouping

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

eflalo_fs %>% select (FT_REF, VE_REF, VE_LEN, VE_LEN_CAT)


## Trip length

# Add a column with the number of days each trip lasted for
eflalo_fs  = eflalo_fs %>% mutate ( trip_days = as.numeric(difftime(eflalo_fs$FT_LDATIM, eflalo_fs$FT_DDATIM), units = "days") )
head(eflalo_fs)

# Plot fishing trip duration as a histogram,. excluding trips which are supposedly longer than 500 days
ggplot(data = eflalo_fs %>% filter( trip_days < 500 ) ) +
  geom_histogram( aes( trip_days ) )

# Plot trips of less than 50 days, on a scatter graph vs vessel length
# This will show whether larger vessels tend to make longer trips and vice versa
ggplot(data = eflalo_fs %>% filter(trip_days < 50) , aes ( x = trip_days, y = VE_LEN , color = SOURCE )) + 
  geom_point(  )


## Fleet composition

# Number of trips per vessel length category
ggplot(data = eflalo_fs %>% distinct(FT_REF, VE_LEN_CAT), aes(VE_LEN_CAT)) +
  geom_bar()

# Number of trips per gear category
ggplot(data = eflalo_fs%>%distinct(FT_REF, LE_GEAR), aes(LE_GEAR)) +
  geom_bar()

# Number of trips per gear category, separated into one graph per length category
ggplot(data = eflalo_fs%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR), aes(LE_GEAR)  ) +
  geom_bar() + 
  facet_wrap( ~ VE_LEN_CAT)

# Number of trips per length category, separated into one graph per gear category 
ggplot(data = eflalo_fs%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR) , aes(VE_LEN_CAT)  ) +
  geom_bar() + 
  facet_wrap( ~ LE_GEAR)



## Species capture

# Rank the species captured, by gear type, and plot to explore distribution
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



## Record quantities - how many log events are there?

eflalo_fs %>% dim()

# Number of records per trip
res2 = eflalo_fs%>%group_by(FT_REF) %>% summarise(n = n()) %>% arrange(desc(n))
ggplot(res2, aes( n )) + geom_histogram()

# Number of log events per trip (Only available from GeoFISH-sourced data)
res21= eflalo_fs%>%distinct (FT_REF, LE_ID , trip_days) %>%group_by(FT_REF, trip_days)%>%summarise(n = n())%>%arrange(desc(n))
ggplot(res21, aes( n )) + geom_histogram()




#### TACSAT ####

tacsat_fs %>% dim()

## Time Frame

# Assign columns to allow for time frame exploration
tacsat_fs$QUARTER = quarter(tacsat_fs$SI_DATE)
tacsat_fs$MONTH = month(tacsat_fs$SI_DATE)

# Number of pings per month
tacsat_fs %>% group_by(MONTH) %>% tally()

ggplot(data = tacsat_fs %>% distinct(SI_FT, MONTH), aes(MONTH)) +
  xlab('Month') +
  geom_bar()

# Number of pings per quarter
tacsat_fs %>% group_by(QUARTER) %>% tally()

# Summary statistics
tacsat_fs$SI_SP %>% summarise()

## Data Source

# Number of entries per source
tacsat_fs %>% group_by(SOURCE) %>% tally()




#### TACSAT EFLALO ####

# Check dimensions of tacsatEflalo data
tacsatEflalo %>% dim()

# Distribution of rows across months
tacsatEflalo %>% as.data.frame() %>% st_drop_geometry() %>% group_by(Month) %>% tally()

ggplot(data = tacsatEflalo %>% distinct(SI_FT, Month), aes(Month)) +
  xlab('Month') +
  geom_bar()


# Welsh waters ICES analysis
# If the analysis is for Welsh waters, it may be useful to see the distribution of catch across the ICES rectangles in the Welsh zone
tacsatEflalo %>% as.data.frame() %>% st_drop_geometry() %>% group_by(LE_RECT) %>% tally()
tacsatEflalo %>% as.data.frame() %>% st_drop_geometry() %>% group_by(Csquare) %>% tally()
