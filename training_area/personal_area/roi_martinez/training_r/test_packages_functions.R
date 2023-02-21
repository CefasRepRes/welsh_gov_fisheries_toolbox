?mean


mean  ( 10)

weight_kg = c(35, 25, 12)
spe = c('whe', 'cod', 'had' )
ve_ref = c( 've1', 've2', 've3')

landings = data.frame (ve_ref, spe, weight_kg   )

landings[ c(2, 3) , c( "ve_ref","spe")  ]

landings[   landings$weight_kg  < 30  ,    ]


landings[ , "weight_kg" ] < 30

colnames(landings) = c("ve_ref", "species", "weight_kg" )
head(landings)
dim(landings)


 ?head
weight_kg[1]

meanval = mean (  t   )
maxval = max ( t )
min(t)

res = meanval /maxval
rm(res)

565
