 
####### UK FISHING ACTIVITY WITHIN WELSH WATER BOUDNARIES ######
library(DBI)
con_geofish = dbConnect(odbc::odbc(), "geofish_psql_dev_editor")


# 1.1 Load spatial auxiliary data ===========================================

## LIBRARY SF required for spatial analysis 

getwd()
setwd('.\\analysis_toolbox\\1_data_preprocessing') ##set up the new location of current Working Directory 


welsh_marine_area = st_read ( dsn = '.\\spatial_layers\\wales_plan_area.geojson' )

ices_rect = st_read(con_geofish,Id(schema = 'marine_statistical_grids', table = 'ices_rectangles') )



ices_rect_welsh =  ices_rect %>%  filter(  st_intersects( ., st_make_valid(st_union(welsh_marine_area  )) , sparse = FALSE)  ) 

ggplot(ices_rect_welsh , aes( icesname , color = 'red'  ) ) + geom_sf(   ) 
 
 
world = ne_countries(scale = "large", returnclass = "sf")

welsh_marine_area %>% st_bbox()

        ggplot() + geom_sf( data =   world     ) +
        geom_sf (data = ices_rect_welsh ,  fill = "transparent"   ) +
        geom_sf (data = welsh_marine_area ,  fill = "transparent"   ) +
        theme_bw() +
        coord_sf(xlim = c( -8.0,-1.5 ), ylim =  c( 50.5, 55), expand = FALSE)  

        

## save the ices rectangle selected for welsh waters
        
st_write( ices_rect_welsh, dsn = ".\\..\\..\\..\\data\\ices_rectangle_welsh.geojson", layer = "ices_rectangle_welsh.geojson")


paste( shQuote( ices_rect_welsh%>%distinct(icesname)%>%pull(), type="sh"   ) , collapse = ',' )
        