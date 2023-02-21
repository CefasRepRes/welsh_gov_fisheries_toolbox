

### LOOPS 

## While loop

x = 1

while ( x < 1000  ) {
   
    x = x  +  5
    print(x)
}


## For loop

vgear = c('DRB', 'FPO', 'OTB')

lands_df = data.frame(ve_ref = c('v1','v2','v3','v3','v1','v2','v4') , 
           gear = c('FPO','OTB','OTB','TBB','TBB', 'FPO','DRB'),
           kg = c (234, 66, 78, 90, 100, 34, 98 ))


gear_list = unique( lands_df$gear )
total_stats_df = data.frame()

for ( x in gear_list ) {

    gear_kg = lands_df[ lands_df$gear == x ,  "kg" ]
    total_kg = sum(gear_kg)
    
    print( paste( 'The gear ', x , 'captured' , total_kg, 'kg of fish'   ) )
    
    row_val = c(x, total_kg )
    total_stats_df = rbind(total_stats_df, row_val)
   
}



names(total_stats_df) = c('gear', 'total_kg')


### load library 

library(ggplot2)

ggplot( data =  total_stats_df, aes( x = gear , y = total_kg  )) + 
  geom_bar()

