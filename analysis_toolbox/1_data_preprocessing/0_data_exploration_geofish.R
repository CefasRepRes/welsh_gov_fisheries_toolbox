library(lubridate)
library(dplyr)
library(sf)
library(ggplot2)


# Explore vessels nationality in the EFLALO records ----

eflalo%>%distinct(VE_COU)   # extracts a list of unique entries in the VE_COU column, not listing duplicates
eflalo%>%distinct(VE_COU)    # same for VE_FA column
eflalo%>%distinct(FT_DCOU)  # same again for FT_DCOU column

eflalo_gbw = eflalo %>% filter( VE_COU == 'GBW')  ##Vessel Fishing authority 



eflalo %>% distinct( FT_REF) %>% dim() 
eflalo_gbw %>% distinct(FT_REF)%>%dim() 


ft_eflalo_gbw = eflalo_gbw %>% select(FT_REF) %>% pull()  ## get the unique FT id's in the eflalo with welsh fleet records

# now filter the tacsat data to include only entries with a matching FT_REF/SI_FT values
tacsat_gbw = tacsat %>% filter( SI_FT %in%   ft_eflalo_gbw )  ## filter TACSAT (ivms) for welsh fleet trips in eflalo

# show the dimensions to see how many results were lost in the matching process
dim(tacsat)
dim(tacsat_gbw)

## Create the VESSEL LENGTH CAGTEGORY to explore data by vessel length classes ----

eflalo_gbw$VE_LEN_CAT = cut( eflalo_gbw$VE_LEN , include.lowest = T,
                             breaks=c(-Inf, 4.5, 5 , 7, 9, 10 , Inf), 
                             labels=c("<4.5 m","4.5 - 5 m", "5 - 7 m", "7 - 9 m", "9 - 10 m", "=> 10  m"))

# view the columns selected within the brackets
eflalo_gbw %>% select ( FT_REF , VE_REF, VE_LEN, VE_LEN_CAT) 


## Create the field "trip_days" with duration of each trips as number of days ----

eflalo_gbw$trip_days  =  as.numeric( eflalo_gbw$FT_LDATIM -  eflalo_gbw$FT_DDATIM )

head( eflalo_gbw )


## Plotting ----

# Plot the categories/columns to understand the fleet composition.

# Plot 1 is a bar graph which shows the number of trips made per vessel category.

## PLOT 1: Number of trips by vessel length category 
ggplot( data = eflalo_gbw %>% distinct(FT_REF, VE_LEN_CAT) ,   aes( VE_LEN_CAT )   ) +
  geom_bar()


## PLOT 2: Number of trips by gear category 
ggplot( data = eflalo_gbw%>%distinct(FT_REF, LE_GEAR)  , aes( LE_GEAR ) ) +
  geom_bar()


# using facet wrap to create individual plots for each length category
ggplot(data = eflalo_gbw%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR), aes(LE_GEAR)  ) +
  geom_bar() + 
  facet_wrap( ~ VE_LEN_CAT)

# facet wrap by gear type
ggplot(data = eflalo_gbw%>%distinct(FT_REF,VE_LEN_CAT, LE_GEAR) , aes(VE_LEN_CAT)  ) +
  geom_bar() + 
  facet_wrap( ~ LE_GEAR)


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
  facet_wrap(~ LE_GEAR, scales = "free" ) # scales =  free allows each plot to assign its Y axis scale however it likes, otherwise some plots 
                                          # would be useless if they have significantly smaller numbers than others.


## PLOT 4: Fishing trip duration and fish duration by vessel length. 

#Q1: Do we expect longer trips from larger vessels?
#Q2: Is there any trips duration identified as outliers? 

ggplot(data = eflalo_gbw %>% filter( trip_days < 500 ) ) +
  geom_histogram( aes( trip_days ) )

ggplot(data = eflalo_gbw%>%filter(trip_days < 50) , aes ( x = trip_days, y = VE_LEN )) + 
  geom_point(  )

ggplot(data = eflalo_gbw%>%filter(trip_days < 50) , aes ( x = trip_days, y = VE_LEN )) + 
  geom_point(  )


# write the output to csv ----
write.csv( x = res1, file =  ".\\..\\data\\data_output\\species_kg_ranked_by_gear.csv", row.names=FALSE)


## Save the intermediate EFLALO and TACSAT datasets
save ( eflalo_gbw , file = '.\\..\\data\\eflalo_gbw.RData' )
save ( tacsat_gbw , file = '.\\..\\data\\tacsat_gbw.RData' )
