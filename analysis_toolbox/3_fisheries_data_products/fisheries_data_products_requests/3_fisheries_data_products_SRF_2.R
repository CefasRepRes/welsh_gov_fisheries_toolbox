# 3.1 Load TABLE 1 (VMS) and TABLE 2 (LOGBOOK) --------------------------------------------

library(tidyverse)
library(lubridate)
library(vmstools)
library(sf)
library(rnaturalearth)
library(devtools)
library(data.table)
library(here)

setwd(here(".\\data"))

# create folders for outputs if they do not exist 

# input folder
inPath = here(".\\analysis_toolbox\\3_fisheries_data_products\\fisheries_data_products_requests")

# data products folder
dir.create(paste0(inPath, "\\data-products"))
outPath = paste0(inPath, "\\data-products\\")

# geojson output folder
dir.create(paste0(outPath, "geojson"))
gjPath = paste0(outPath, "geojson\\")

# plots folder
dir.create(paste0(outPath, "plots"))
plotsPath = paste0(outPath, "plots\\")

#######


csq = 'csq005'


#######################
##### SRF 2 FDP A #####
#######################

year = 2022

analysis_type = 'welsh_fleet'

load(file = paste0(".\\workflow_outputs\\srf-2\\tacsatEflalo_", analysis_type, "_", year, "_SRF_2_FDP.RData"))
load(file = paste0(".\\workflow_outputs\\srf-2\\eflalo_output_", analysis_type, "_", year, "_SRF_2_FDP.RData"))

## ASSGIN C-SQuare label


tacsatEflalo$Csquare_01 = CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.01)

VE_lut <- data.frame(VE_REF = unique(c(tacsatEflalo$VE_REF, eflalo_output$VE_REF)))
fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
VE_lut$VE_ID <- paste0(tacsatEflalo$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!


# join onto data tables
table1 <- left_join(tacsatEflalo, VE_lut)
table1$VE_KW = as.numeric(table1$VE_KW)

table1 <- data.table(table1)

table1m <- melt(table1, 
            measure = patterns("LE_KG" = "LE_KG", "LE_EURO" = "LE_EURO"),
            value.name = c("LE_KG", "LE_EURO"), 
            variable.name = "LE_SPE")

table1m[, LE_SPE := factor(LE_SPE, 
                        labels = unique(gsub("(.*)(_)([A-Z]*)$", "\\3", 
                                             grep("LE_KG|LE_EURO", colnames(table1), value = TRUE))))]

table1m = table1m %>% filter(LE_KG > 0 & LE_EURO > 0)

table1m %>% filter(!INTV == 0) %>% tally()

### Year, Month, 0.01 Csquare, gear, species

table1.a_input <- table1m

table1.a =
  table1.a_input %>%
  group_by(SI_YEAR, Month, Csquare_05, LE_GEAR, LE_SPE) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kw       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    #sum_le_kg_tot    = sum(LE_KG_TOT, na.rm = TRUE),
    #sum_le_euro_tot  = sum(LE_EURO_TOT, na.rm = TRUE),
    sum_le_kg_tot    = sum(LE_KG, na.rm = TRUE),
    sum_le_euro_tot  = sum(LE_EURO, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .after = LE_GEAR) %>%
  #mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()

write.csv(table1.a, paste0(outPath, "\\", year, "_table1_a_", csq, "_SRF2.csv"), row.names = FALSE)




#######################
##### SRF 2 FDP B #####
#######################

year = 2022

analysis_type = 'welsh_waters'

load(file = paste0(".\\workflow_outputs\\srf-2\\tacsatEflalo_", analysis_type, "_", year, "_SRF_2_FDP.RData"))
load(file = paste0(".\\workflow_outputs\\srf-2\\eflalo_output_", analysis_type, "_", year, "_SRF_2_FDP.RData"))

## ASSGIN C-SQuare label
tacsatEflalo$Csquare_01 = CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.01)

VE_lut <- data.frame(VE_REF = unique(c(tacsatEflalo$VE_REF, eflalo_output$VE_REF)))
fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
VE_lut$VE_ID <- paste0(tacsatEflalo$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!

# join onto data tables
table1 <- left_join(tacsatEflalo, VE_lut)
table1$VE_KW = as.numeric(table1$VE_KW)

table1 <- data.table(table1)

table1m <- melt(table1, 
                measure = patterns("LE_KG" = "LE_KG", "LE_EURO" = "LE_EURO"),
                value.name = c("LE_KG", "LE_EURO"), 
                variable.name = "LE_SPE")

table1m[, LE_SPE := factor(LE_SPE, 
                           labels = unique(gsub("(.*)(_)([A-Z]*)$", "\\3", 
                                                grep("LE_KG|LE_EURO", colnames(table1), value = TRUE))))]

table1.b_input <- table1m

### Year, Month, 0.01 Csquare, gear, species
table1.b = 
  table1.b_input %>%
  group_by(SI_YEAR, Month, Csquare_05, LE_GEAR, LE_SPE) %>%
  summarise(
    mean_si_sp       = mean(SI_SP),
    sum_intv         = sum(INTV, na.rm = TRUE),
    mean_intv        = mean(INTV, na.rm=TRUE),
    mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
    mean_ve_kf       = mean(VE_KW, na.rm = TRUE),
    sum_kwHour       = sum(kwHour, na.rm=TRUE),
    sum_le_kg        = sum(LE_KG, na.rm = TRUE),
    sum_le_euro      = sum(LE_EURO, na.rm = TRUE),
    n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
    vessel_ids       = ifelse (
      n_distinct(VE_ID) < 3,
      paste(unique(VE_ID), collapse = ";"),
      'not_required'
    )
  ) %>% relocate(n_vessels, vessel_ids, .before = Csquare_05) %>%
  mutate(AverageGearWidth = NA %>% as.numeric()) %>%
  as.data.frame()

write.csv(table1.b, paste0(outPath, "\\", year, "_table1_b_", csq, "_SRF2.csv"), row.names = FALSE)




#########################
##### SRF 2 FDP C/D #####
#########################

years = 2012:2021
# year = 2021
analysis_type = 'welsh_waters' ## welsh_fleet / welsh_waters


for ( year in years ) {
  
  load(file = paste0(".\\workflow_outputs\\srf-2\\tacsatEflalo_", analysis_type, "_", year, "_SRF_2_FDP.RData"))
  load(file = paste0(".\\workflow_outputs\\srf-2\\eflalo_output_", analysis_type, "_", year, "_SRF_2_FDP.RData"))
  
  ## ASSGIN C-SQuare label
  
  if (year == 2012) {
    tacsatEflalo_all <- tacsatEflalo
    eflalo_all <- eflalo_output
  } else {
    tacsatEflalo_all <- tacsatEflalo_all %>% bind_rows(., tacsatEflalo)
    eflalo_all <- eflalo_all %>% bind_rows(., eflalo_output)
  }
} 

  tacsatEflalo <- tacsatEflalo_all
  eflalo_output <- eflalo_all
  
  tacsatEflalo$Csquare_01 = CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.01)
  
  VE_lut <- data.frame(VE_REF = unique(c(tacsatEflalo$VE_REF, eflalo_output$VE_REF)))
  fmt <- paste0("%0", floor(log10(nrow(VE_lut))) + 1, "d")
  VE_lut$VE_ID <- paste0(tacsatEflalo$VE_COU[1], sprintf(fmt, 1:nrow(VE_lut))) # use relevant country code!
  
  
  # join onto data tables
  table1 <- left_join(tacsatEflalo, VE_lut)
  table1$VE_KW = as.numeric(table1$VE_KW)
  
  table1 <- data.table(table1)
  
  table1m <- melt(table1, 
                  measure = patterns("LE_KG" = "LE_KG", "LE_EURO" = "LE_EURO"),
                  value.name = c("LE_KG", "LE_EURO"), 
                  variable.name = "LE_SPE")
  
  table1m[, LE_SPE := factor(LE_SPE, 
                             labels = unique(gsub("(.*)(_)([A-Z]*)$", "\\3", 
                                                  grep("LE_KG|LE_EURO", colnames(table1), value = TRUE))))]

  table1_input = table1m %>% filter (VE_LEN > 12)
  
  ### Year, Month, 0.01 Csquare, gear, species
  table1.output = 
    table1_input %>%
    group_by(SI_YEAR, Month, Csquare_05, LE_GEAR, LE_SPE) %>%
    summarise(
      mean_si_sp       = mean(SI_SP),
      sum_intv         = sum(INTV, na.rm = TRUE),
      mean_intv        = mean(INTV, na.rm=TRUE),
      mean_ve_len      = mean(VE_LEN, na.rm = TRUE),
      mean_ve_kw       = mean(VE_KW, na.rm = TRUE),
      sum_kwHour       = sum(kwHour, na.rm=TRUE),
      sum_le_kg        = sum(LE_KG, na.rm = TRUE),
      sum_le_euro      = sum(LE_EURO, na.rm = TRUE),
      n_vessels        = n_distinct(VE_ID, na.rm = TRUE),
      vessel_ids       = ifelse (
        n_distinct(VE_ID) < 3,
        paste(unique(VE_ID), collapse = ";"),
        'not_required'
      )
    ) %>% relocate(n_vessels, vessel_ids, .after = LE_GEAR) %>%
    #mutate(AverageGearWidth = NA %>% as.numeric()) %>%
    as.data.frame()
  
  if (analysis_type == 'welsh_fleet') {
    write.csv(table1.output, paste0(outPath, "\\table1_c_", csq, "_SRF2.csv"), row.names = FALSE)
  } else if (analysis_type == 'welsh_waters') {
    write.csv(table1.output, paste0(outPath, "\\table1_d_", csq, "_SRF2.csv"), row.names = FALSE)
  }




  
  


### DATA PRODUCTS VISUALIZATION ###############




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

grid_0_01  = st_make_grid( bbox_aoi,  cellsize = cell_resolution_001, square = TRUE,   offset = c(min(lons),min(lats)))#%>% as(Class="Spatial")

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

#world = read_sf("C:\\Users\\md09\\OneDrive - CEFAS\\data\\europe_coastline_shp", "Europe_coastline") %>% st_transform(., crs = 4326)

world <- ne_countries(scale = "large", returnclass = "sf")


## Add the spatial grid geometry to the fisheries data products tables: 

table1.a_geom = csquares_0_01_split_centroids %>% inner_join(table1.a, by = c("csquare" = "Csquare_05"))
table1.c_geom = csquares_0_01_split_centroids %>% inner_join(table1.c, by = c("csquare" = "Csquare_05"))
table1.b_geom = csquares_0_01_split_centroids %>% inner_join(table1.b, by = c("csquare" = "Csquare_05"))
table1.d_geom = csquares_0_01_split_centroids %>% inner_join(table1.d, by = c("csquare" = "Csquare_05"))

# write tables to geojson

st_write(table1.a_geom, paste0(gjPath, "table1_a_geom_", year, ".geojson"), layer = paste0("table1_a_geom_", year, ".geojson")) ## save as geojson 
st_write(table1.c_geom, paste0(gjPath, "table1_c_geom_", year, ".geojson"), layer = paste0("table1_c_geom_", year, ".geojson"))
st_write(table1.b_geom, paste0(gjPath, "table1_b_geom_", year, ".geojson"), layer = paste0("table1_b_geom_", year, ".geojson"))
st_write(table1.d_geom, paste0(gjPath, "table1_d_geom_", year, ".geojson"), layer = paste0("table1_d_geom_", year, ".geojson"))


# create and save the plot images
resa = plot_function(data_plot = table1.a_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "Month")
ggsave(plot = resa, filename = paste0("table1_a_plot_", year, ".png"), path = plotsPath, width = 16, height = 9, dpi = 300)

resc = plot_function(data_plot = table1.c_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "Month")
ggsave(plot = resc, filename = paste0("table1_c_plot_", year, ".png"), path = plotsPath, width = 16, height = 9, dpi = 300)

resb = plot_function(data_plot = table1.b_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "Month")
ggsave(plot = resb, filename = paste0("table1_b_plot_", year, ".png"), path = plotsPath, width = 16, height = 9, dpi = 300)

resd = plot_function(data_plot = table1.d_geom, col_plot = 'sum_intv', col_facet1 = "LE_GEAR", col_facet2 = "Month")
ggsave(plot = resd, filename = paste0("table1_d_plot_", year, ".png"), path = plotsPath, width = 16, height = 9, dpi = 300)


