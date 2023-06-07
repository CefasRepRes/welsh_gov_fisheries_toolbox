year = 2022

# 3.1 Load TABLE 1 (VMS) and TABLE 2 (LOGBOOK) --------------------------------------------

#######
# DELETE BEFORE PUSH #
#######

library(tidyverse)
library(lubridate)
library(xlsx)

 
#######

load(file = paste0(outPath, "/eflalo_output_", year , ".RData")  )
load(file = paste0(outPath, "/tacsatEflalo_output_", year , ".RData")  )

## 3.2 Welsh Gov Fisheries Data Product 


head(tacsatEflalo)

# table1Save <-
#   table1 %>%
# 
#   group_by(RT,VE_COU,Year,Month,Csquare,LE_GEAR, met5, LE_MET,LENGTHCAT) %>%
#   summarise(
#     mean_si_sp       = mean(SI_SP),
#     sum_intv         = sum(INTV, na.rm=TRUE),
#     mean_intv        = mean(INTV, na.rm=TRUE),
#     mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
#     mean_ve_kf       = mean(VE_KW, na.rm = TRUE),
#     sum_kwHour       = sum(kwHour, na.rm=TRUE),
#     sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
#     sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
#     n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
#     vessel_ids       = ifelse (
#       n_distinct(VE_ID) < 3,
#       paste(unique(VE_ID), collapse = ";"),
#       'not_required'
#     )
#   ) %>%  relocate( n_vessels,vessel_ids, .before = Csquare)%>%
#   mutate (AverageGearWidth = NA%>%as.numeric()  )%>% ## If this information is available modify this line of the script. By default is assumed not existing gear width information
#   as.data.frame()

# TABLE 1.

#Fishing activity indicators: 

  # Total Fishing effort hours
  # Total Fishing effort hours * kw
  # Total Fishing catch weight
  # Fishing catch value 
  # Average speed
  # Average length 
  # Average Kw 


# aggregated by: 

  # Temporal resolution: year, month 
  # Spatial resolution:  c_square 0.05
  # Fleet resolution: m4, m5 and vessel length classes





# Create vessel length class: Add the vessel length category using  LENGTHCAT field

len_labels = c("0-5m", "5-7m", "7-10m", ">10m")

tacsatEflalo$LENGTHCAT <- tacsatEflalo$VE_LEN %>% cut(breaks=c(0, 5, 7, 10, 'inf'), 
                                                      right = FALSE ,include.lowest = TRUE,
                                                      labels = len_labels
                                                      )


quar_labels = c("1", "2", "3", "4")
tacsatEflalo$QUARTER = tacsatEflalo$Month %>% cut(breaks = c(0, 4, 7, 10, 12),
                                                  right = F, include.lowest = T,
                                                  labels = quar_labels
                                                  )

tacsatEflalo$Csquare_01 = CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.01)



# New field added for the 2020 datacall including unique vessels id's  #
# This vessel id is used to calculate unique vessels in a c-square and  #
VE_lut <- data.frame(VE_REF = unique(c(tacsatEflalo$VE_REF, eflalo_output$VE_REF)))
fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
VE_lut$VE_ID <- paste0(tacsatEflalo$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!


# join onto data tables
table1 <- left_join(tacsatEflalo, VE_lut)
table2 <- left_join(eflalo_output, VE_lut)

table1$VE_KW = as.numeric(table1$VE_KW)

# Table 1

### Quarter, 0.05 Csquare, gear, metier

table1.1 = 
  table1 %>%
  group_by(VE_COU,QUARTER,Csquare,LE_GEAR ) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kw       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
    #sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .before = Csquare) %>%
  #mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()








write.xlsx(table1.1, "./data-products/table1.xlsx",sheetName = "1.1", row.names = F, append = T)


### Year, 0.05 Csquare, gear, metier
table1.2 = 
  table1 %>%
  group_by(VE_COU,Year,Csquare,LE_GEAR) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kf       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
    #sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .before = Csquare) %>%
  mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()

write.xlsx(table1.2, "./data-products/table1.xlsx",sheetName = "1.2", row.names = F, append = T)


### Month, 0.05 Csquare, gear, metier

table1.3 = 
  table1 %>%
  group_by(VE_COU,Year,Csquare,LE_GEAR) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kf       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
    #sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .before = Csquare) %>%
  mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()

write.xlsx(table1.3, "./data-products/table1.xlsx",sheetName = "1.3", row.names = F, append = T)








### Year, 0.01 Csquare, gear, metier, length category

table1.4 = 
  table1 %>%
  group_by(VE_COU,Year,Csquare_01,LE_GEAR,LENGTHCAT) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kf       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
    #sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .before = Csquare_01) %>%
  #mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()

write.xlsx(table1.4, "./data-products/table1.xlsx",sheetName = "1.4", row.names = F, append = T)


### Year, 0.01 Csquare, metier, length category

table1.5 = 
  table1 %>%
  group_by(VE_COU,Year,Csquare_01,LE_MET,LENGTHCAT) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kf       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
    #sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .before = Csquare_01) %>%
  #mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()

write.xlsx(table1.5, "./data-products/table1.xlsx",sheetName = "1.5", row.names = F, append = T)



### Month, 0.05 Csquare, metier

table1.7 = 
  table1 %>%
  group_by(VE_COU,Month,Csquare,LE_GEAR ) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kf       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
    #sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .before = Csquare) %>%
  #mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()

write.xlsx(table1.7, "./data-products/table1.xlsx",sheetName = "1.7", row.names = F, append = T)







### Quarter, 0.01 Csquare, metier

table1.6 = 
  table1 %>%
  group_by(VE_COU,QUARTER,Csquare_01,LE_GEAR ) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kf       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
    #sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .before = Csquare_01) %>%
  #mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()

write.xlsx(table1.6, "./data-products/table1.xlsx",sheetName = "1.6", row.names = F, append = T) ##save as csv instead 



 





### DATA PRODUCTS VISUALIZATION ###############




## bounding box area limits definition

lats = c ( min(table1$SI_LATI), max(table1$SI_LATI) ) 
lons = c ( min(table1$SI_LONG), max(table1$SI_LONG) ) 

## Welsh boundaries

lats = c ( 51, 53.5) 
lons = c ( -6, -2 ) 

 
coord_bbox = data.frame(lons, lats)
bbox_aoi =  coord_bbox %>% 
  st_as_sf(coords = c("lons","lats"), crs = 4326) %>%
  st_bbox() %>% st_as_sfc() %>% 
  st_sf( id  = 1, label = 'bbox' ) %>%
  st_set_crs(4326)


## create the grids at different resolutions 

cell_resolution = 0.05 
csquare_resolution = 0.05

##spatial grid 0_05 

grid_0_05  = st_make_grid( bbox_aoi,  cellsize = cell_resolution, square = TRUE, offset = c(min(lons),min(lats))  ) #%>% as(Class = "Spatial")
 
grid_0_05_centroid  = st_sf (grid_0_05) %>%  st_centroid (.) %>% st_coordinates() 
grid_0_05csquares =  CSquare ( grid_0_05_centroid[,"X"], grid_0_05_centroid[,"Y"],  csquare_resolution )

csquares_0_05_split_centroids = st_bind_cols( grid_0_05, 
                                         grid_res = cell_resolution, 
                                         csquare = grid_0_05csquares, 
                                         lon = round ( grid_0_05_centroid[,"X"],5 ), 
                                         lat = round ( grid_0_05_centroid[,"Y"],5)  )

##spatial grid 0_01

cell_resolution = 0.01 
csquare_resolution = 0.01

grid_0_01  = st_make_grid( bbox_aoi,  cellsize = cell_resolution, square = TRUE, offset = c(min(lons),min(lats))  ) #%>% as(Class = "Spatial")

grid_0_01_centroid  = st_sf (grid_0_01) %>%  st_centroid (.) %>% st_coordinates() 
grid_0_01csquares =  CSquare ( grid_0_01_centroid[,"X"], grid_0_01_centroid[,"Y"],  csquare_resolution )

csquares_0_01_split_centroids = st_bind_cols( grid_0_01, 
                                              grid_res = cell_resolution, 
                                              csquare = grid_0_01csquares, 
                                              lon = round ( grid_0_01_centroid[,"X"],5 ), 
                                              lat = round ( grid_0_01_centroid[,"Y"],5)  )









## Plot the tables 


world <- ne_countries(scale = "large", returnclass = "sf")



## Add the spatial grid geomtry to the fisheries data products tables: 

table1.1_geom = csquares_0_05_split_centroids%>%inner_join(table1.1 , by = c("csquare" = "Csquare"))
st_write(table1.1_geom, ) ##save as geojson 

ggplot() + geom_sf( data = world   ) +
  geom_sf (data = table1.1_geom  , aes(color = sum_intv,  fill = sum_intv) ) +
  theme_bw() +
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) + 
  facet_wrap( ~ LE_GEAR + QUARTER)





table1.6_geom = csquares_0_01_split_centroids%>%inner_join(table1.6 , by = c("csquare" = "Csquare_01"))



ggplot() + geom_sf( data = world   ) +
  geom_sf (data = table1.6_geom  , aes(color = sum_intv,  fill = sum_intv) ) +
  theme_bw() +
  coord_sf(xlim = lons, ylim = lats, expand = FALSE)




## 2 more examples of graphs 








