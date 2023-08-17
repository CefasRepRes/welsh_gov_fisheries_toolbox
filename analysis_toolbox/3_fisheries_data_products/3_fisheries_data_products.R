# 3.1 Load TABLE 1 (VMS) and TABLE 2 (LOGBOOK) --------------------------------------------

library(tidyverse)
library(lubridate)
library(vmstools)
library(sf)
library(rnaturalearth)
library(devtools)

# function for plotting data products - to be used later, it is just common practice to put functions at the top of scripts
plot_function = function ( data_plot , col_plot ,col_facet1 = NULL  , col_facet2 = NULL  ) {
  
  gg1 =  ggplot() + geom_sf( data =   world     ) +
    geom_sf (data = data_plot , aes(color = sum_intv,  fill = sum_intv) ) +
    theme_bw() +
    coord_sf(xlim = lons, ylim = lats, expand = FALSE) #+
    #labs(fill = "Effort") # +
  # facet_wrap( ~ get( col_facet1) + get( col_facet2)  )
  if ( is.null( col_facet2 )  )  {
    gg1 =  gg1 +
      facet_wrap( ~ get( col_facet1)  )
  } else  {
    gg1 =  gg1 +
      facet_wrap( ~ get( col_facet1) +get( col_facet2)   )
  }
  return(gg1)
}


# plot_function = function ( data_plot , col_plot ,col_facet1 = NULL  , col_facet2 = NULL  ) {
#   
#   gg1 =  ggplot() + geom_sf( data =   world     ) +
#     geom_sf (data = data_plot , aes(color = get( col_plot),  fill = get(col_plot)) ) +
#     theme_bw() +
#     coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
#     labs(fill = "Effort") # +
#   # facet_wrap( ~ get( col_facet1) + get( col_facet2)  )
#   if ( is.null( col_facet2 )  )  {
#     gg1 =  gg1 +
#       facet_wrap( ~ get( col_facet1)  )
#   } else  {
#     gg1 =  gg1 +
#       facet_wrap( ~ get( col_facet1) +get( col_facet2)   )
#   }
#   return(gg1)
# }

# create folders for outputs if they do not exist 

# input folder
inPath = "C:\\Users\\md09\\OneDrive - CEFAS\\projects\\C8529A-welsh-gov\\data\\data-2-output"

# data products folder
dir.create("C:\\Users\\md09\\Documents\\git\\welsh_gov_fisheries_toolbox\\analysis_toolbox\\3_fisheries_data_products\\data-products")
outPath = "C:\\Users\\md09\\Documents\\git\\welsh_gov_fisheries_toolbox\\analysis_toolbox\\3_fisheries_data_products\\data-products\\"

# geojson output folder
dir.create(paste0(outPath, "geojson"))
gjPath = paste0(outPath, "geojson\\")

# plots folder
dir.create(paste0(outPath, "plots"))
plotsPath = paste0(outPath, "plots\\")

#######

year = 2022

load(file = paste0(inPath, "/eflalo_output_", year , ".RData")  )
load(file = paste0(inPath, "/tacsatEflalo_output_", year , ".RData")  )

## 3.2 Welsh Gov Fisheries Data Product ----

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

### Quarter, 0.05 Csquare, gear

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

write.csv(table1.1, paste0(outPath, year, "_table1_1.csv"))


### Year, 0.05 Csquare, gear
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

write.csv(table1.2, paste0(outPath, year, "_table1_2.csv"))


### Month, 0.05 Csquare, gear

table1.3 = 
  table1 %>%
  group_by(VE_COU,Month,Csquare,LE_GEAR) %>%
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

write.csv(table1.3, paste0(outPath, year, "_table1_3.csv"))


### Quarter, 0.01 Csquare, gear, length category

table1.4 = 
  table1 %>%
  group_by(VE_COU,QUARTER,Csquare_01,LE_GEAR,LENGTHCAT) %>%
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

write.csv(table1.4, paste0(outPath, year, "_table1_4.csv"))


### Year, 0.01 Csquare, gear, length category

table1.5 = 
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

write.csv(table1.5, paste0(outPath, year, "_table1_5.csv"))


### Month, 0.01 Csquare, gear, length category

table1.6 = 
  table1 %>%
  group_by(VE_COU,Month,Csquare_01,LE_GEAR,LENGTHCAT) %>%
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

write.csv(table1.6, paste0(outPath, year, "_table1_6.csv"))



### DATA PRODUCTS VISUALIZATION ###############

## bounding box area limits definition

# find the min and max limits of the table's data
lats = c ( min(table1$SI_LATI), max(table1$SI_LATI) ) 
lons = c ( min(table1$SI_LONG), max(table1$SI_LONG) ) 

## Welsh boundaries
# expand the limits to give a better field of view
lats = c ( 51, 53.5) 
lons = c ( -6, -2 ) 

# create a bounding box 
coord_bbox = data.frame(lons, lats)
bbox_aoi =  coord_bbox %>% 
  st_as_sf(coords = c("lons","lats"), crs = 4326) %>%
  st_bbox() %>% st_as_sfc() %>% 
  st_sf( id  = 1, label = 'bbox' ) %>%
  st_set_crs(4326)


## create the grids at different resolutions 

cell_resolution_005 = 0.05 
csquare_resolution_005 = 0.05

##spatial grid 0_05 

grid_0_05  = st_make_grid( bbox_aoi,  cellsize = cell_resolution_005, square = TRUE, offset = c(min(lons),min(lats)))#%>% as(Class="Spatial")
 
grid_0_05_centroid  = st_sf (grid_0_05) %>%  st_centroid (.) %>% st_coordinates() 
grid_0_05csquares =  CSquare ( grid_0_05_centroid[,"X"], grid_0_05_centroid[,"Y"],  csquare_resolution_005 )

csquares_0_05_split_centroids = st_bind_cols( grid_0_05, 
                                         grid_res = cell_resolution_005, 
                                         csquare = grid_0_05csquares, 
                                         lon = round ( grid_0_05_centroid[,"X"],5 ), 
                                         lat = round ( grid_0_05_centroid[,"Y"],5)  )

st_write(csquares_0_05_split_centroids, paste0(gjPath, "csquares_0_05_split_centroids.geojson"), layer = "csquares_0_05_split_centroids") ## save as geojson 

##spatial grid 0_01

cell_resolution_001 = 0.01 
csquare_resolution_001 = 0.01

grid_0_01  = st_make_grid( bbox_aoi,  cellsize = cell_resolution_001, square = TRUE, offset = c(min(lons),min(lats)))#%>% as(Class="Spatial")

grid_0_01_centroid  = st_sf (grid_0_01) %>%  st_centroid (.) %>% st_coordinates() 
grid_0_01csquares =  CSquare ( grid_0_01_centroid[,"X"], grid_0_01_centroid[,"Y"],  csquare_resolution_001 )

csquares_0_01_split_centroids = st_bind_cols( grid_0_01, 
                                              grid_res = cell_resolution_001, 
                                              csquare = grid_0_01csquares, 
                                              lon = round ( grid_0_01_centroid[,"X"],5 ), 
                                              lat = round ( grid_0_01_centroid[,"Y"],5)  )

st_write(csquares_0_01_split_centroids, paste0(gjPath, "csquares_0_01_split_centroids.geojson"), layer = "csquares_0_01_split_centroids") ## save as geojson 


## Plot the tables 

# read in the world shapefile, or use the ne_countries function from the natural earth package if available
world <- ne_countries(scale = "large", returnclass = "sf")


## Add the spatial grid geometry to the fisheries data products tables: 

table1.1_geom = csquares_0_05_split_centroids %>% inner_join(table1.1, by = c("csquare" = "Csquare"))
table1.2_geom = csquares_0_05_split_centroids %>% inner_join(table1.2, by = c("csquare" = "Csquare"))
table1.3_geom = csquares_0_05_split_centroids %>% inner_join(table1.3, by = c("csquare" = "Csquare"))
table1.4_geom = csquares_0_01_split_centroids %>% inner_join(table1.4, by = c("csquare" = "Csquare_01"))
table1.5_geom = csquares_0_01_split_centroids %>% inner_join(table1.5, by = c("csquare" = "Csquare_01"))
table1.6_geom = csquares_0_01_split_centroids %>% inner_join(table1.6, by = c("csquare" = "Csquare_01"))

# write tables to geojson

st_write(table1.1_geom, paste0(gjPath, "table1_1_geom.geojson"), layer = "table1_1_geom.geojson") ## save as geojson 
st_write(table1.2_geom, paste0(gjPath, "table1_2_geom.geojson"), layer = "table1_2_geom.geojson")
st_write(table1.3_geom, paste0(gjPath, "table1_3_geom.geojson"), layer = "table1_3_geom.geojson")
st_write(table1.4_geom, paste0(gjPath, "table1_4_geom.geojson"), layer = "table1_4_geom.geojson")
st_write(table1.5_geom, paste0(gjPath, "table1_5_geom.geojson"), layer = "table1_5_geom.geojson")
st_write(table1.6_geom, paste0(gjPath, "table1_6_geom.geojson"), layer = "table1_6_geom.geojson")


# create and save the plot images
res1 = plot_function(data_plot = table1.1_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "QUARTER")
ggsave(plot = res1, filename = paste0("table1_1_plot.png"), path = plotsPath, width = 16, height = 9, dpi = 300)

res2 = plot_function(data_plot = table1.2_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "Year")
ggsave(plot = res2, filename = paste0("table1_2_plot.png"), path = plotsPath, width = 16, height = 9, dpi = 300)

res3 = plot_function(data_plot = table1.3_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "Month")
ggsave(plot = res3, filename = paste0("table1_3_plot.png"), path = plotsPath, width = 16, height = 9, dpi = 300)

res4 = plot_function(data_plot = table1.4_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "QUARTER")
ggsave(plot = res4, filename = paste0("table1_4_plot.png"), path = plotsPath, width = 16, height = 9, dpi = 300)

res5 = plot_function(data_plot = table1.5_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "Year")
ggsave(plot = res5, filename = paste0("table1_5_plot.png"), path = plotsPath, width = 16, height = 9, dpi = 300)

res6 = plot_function(data_plot = table1.6_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "Month")
ggsave(plot = res6, filename = paste0("table1_6_plot.png"), path = plotsPath, width = 16, height = 9, dpi = 300)
