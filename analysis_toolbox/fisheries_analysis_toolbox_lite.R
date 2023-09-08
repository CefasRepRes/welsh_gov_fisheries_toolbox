########### DATA EXPLORATION AND PREPROCESSING IN ADVANCE OF  QUALITY CONTROL AND CLEANING ##########################

library(dplyr)
library(ggplot2)

setwd('./../../data')

load(".\\workflow_outputs\\eflalo.RData")
load(".\\workflow_outputs\\tacsat.RData")

### 1. Define the fleet segment to be analysed from the Analysis Option chosen in "0_DATA_ACCESS" toolbox section ####

## Fleet segment: 
## - Over 12 m vessels ( source GeoFISH)
## - Under 12 m vessels ( source T3)
## - Combined O12m and U12m fleets

### If you want to analyse a specific section of the data, apply the following if statement


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





## 2. Create the VESSEL LENGTH CAGTEGORY to explore data by vessel length classes ####

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



## 3. Create the field "trip_days" with duration of each trips as   number of days  ####

eflalo_fs  = eflalo_fs %>% mutate ( trip_days = as.numeric(difftime(eflalo_fs$FT_LDATIM, eflalo_fs$FT_DDATIM), units = "days") ) 




### 4. DATA QUALITY CONTROL ( Follow the script in '1_data_preprocessing/1_eflalo_tacsat_quality_control.R' )  ############

    ## Load eflalo and tacsat cleaned data 

        ##   load(file = '.\\workflow_outputs\\eflalo_fs_qc.RData' )
        ##   load(file = '.\\workflow_outputs\\tacsat_fs_qc.RData' )

### 5. ANALYSIS OF EFLAO AND TACSAT COMBINED ( Is assumed the data input is already clean and quality controlled) ####


      # 5.1 Merge the TACSAT and EFLALO data together --------------------------------------------
      
      # Merge eflalo and tacsat =================================
       
      
      ## Convert EFLALO from LARGE format into WIDE format. This complies with the format required by VMSTOOL 'splitamongpings' function
      ## Depending the format selected we have to choose Option 1 or Option 2 in step '2.3 Dispatch landings/catches among VMS pings'
      ## As a general approach , if we want to analyse all species together choose large format if we want to anlyse a number of species choose wide format
      
      eflalo_fs = eflalo_fs%>%
        pivot_wider(id_cols = c( FT_REF, FT_DCOU, FT_DHAR, FT_DDAT, FT_DTIME, FT_DDATIM,
                                 FT_LCOU, FT_LHAR, FT_LDAT, FT_LTIME, FT_LDATIM, VE_REF,
                                 VE_FLT, VE_COU, VE_LEN, VE_KW, VE_TON, FT_YEAR,
                                 LE_ID, LE_CDAT, LE_STIME, LE_ETIME, LE_SLAT, LE_GEAR,
                                 LE_MSZ, LE_RECT, LE_DIV, LE_MET, EFLALO_FT_FT_REF, Year,
                                 Month, VE_LEN_CAT, SOURCE, FLEET_SEG ),
                    names_from = c(LE_SPE), values_from = c(LE_KG, LE_VALUE))
      
       
      
      # Assign gear and length to tacsat =================================
      # Consider that one fishing trip can use more than one geear, 
      # we use log events instead of fishing trip to linkboth dataset
      
      ## 1. Assign LOG EVENT  and FISHING TRIP information to VMS locations 
      
      ## Select the fields required from EFLALO to be transferred into TACSAT
      
      eflalo_sel =  eflalo_fs %>% 
        select(FT_REF, LE_CDAT, LE_GEAR,LE_MSZ, LE_RECT, LE_MET, VE_LEN, VE_KW, VE_COU) %>%
        distinct()
      
      
      
      tacsatp =  tacsat_fs%>%  left_join( eflalo_sel  , by = c("SI_FT" = "FT_REF"       , "SI_DATE" = "LE_CDAT"  )   )
      
      tacsatp%>%filter ( is.na(LE_GEAR)) ## must be 0 , that means all iVMS records have an associated EFLALO records 
      
      
      
      ## Get the first most used gear by fishing trip to fill VMS records with  not associated Log Event .
      ## As a result some VMS records have not gear associated
      
      ft =    eflalo_sel %>%arrange( FT_REF , LE_CDAT) %>% 
        select (FT_REF, LE_GEAR)%>% rename( LE_GEAR2 = LE_GEAR)%>%
        group_by(FT_REF)%>%slice(1)
      
      ## Assign  the main of gear of each trip to the VMS locations with not gear assigned
      
      tacsatp =  tacsatp %>% left_join ( ft , by = c( "SI_FT" = "FT_REF"))%>%
        mutate ( LE_GEAR = ifelse( is.na (LE_GEAR), LE_GEAR2 , LE_GEAR  ))%>%
        select (-LE_GEAR2)
      
      
      ## The result of this query must be 0 records. Meaning all records have an associated gear
      
      tacsatp %>% filter( is.na( LE_GEAR ) )
      
      ## Fill the information for vessel characteristics for those locations with not associated LOG EVENT
      
      ve_c = eflalo_sel%>%distinct(FT_REF, VE_LEN, VE_KW, VE_COU)%>%rename(VE_LEN2 = VE_LEN, VE_KW2 = VE_KW, VE_COU2 = VE_COU )
      
      tacsatp =  tacsatp %>% left_join ( ve_c , by = c( "SI_FT" = "FT_REF"))%>%
        mutate ( VE_LEN = ifelse(is.na (VE_LEN), VE_LEN2 , VE_LEN  ), 
                 VE_KW = ifelse( is.na (VE_KW), VE_KW2 , VE_KW  ) , 
                 VE_COU = ifelse(is.na (VE_COU), VE_COU2 , VE_COU  ) )%>%
        select (-c(VE_LEN2, VE_KW2, VE_COU2))



      
      
      ## To proceed with analysis we change the format to a data.frame
      
      tacsatp = tacsatp %>% as.data.frame()
      
      
      
      
      ## 5.2 Define the fishing activity ( This is based on expert criteria and can be defined using the script in 2_eflalo_tacsat_analysis\ 2_eflalo_tacsat_analysis.R from line 14 to 321)  ######
      
      
      
      
      
      # 5.3 Dispatch landings/catches of merged eflalo at the VMS/iVMS ping scale  -------------------------------------------------
      
      eflalo_format = 'wide' ## defautl format following toolbox workflow 
      species_analysis_type = 'all_species_sum' # options: ( all_species_sum,  selected_species_sum, selected_species_separated )
      
      if ( eflalo_format == 'wide' ) { 
        
        ## For all the species together 
        
        
        ## Option 1: Names in wide format . Each species catch value has its own column 
        
        idxkg  =  grep("KG", colnames(eflalo_fs ) )  
        idxval =  grep("VALUE", colnames(eflalo_fs )  ) 
        
        
        
        ## Option 1.1 : all landings for all species sum together 
        if (species_analysis_type == 'all_species_sum') {
          
          eflalo_fs_tot = eflalo_fs %>% mutate(LE_KG_TOT = select(.,idxkg) %>% rowSums(., na.rm = TRUE) ) %>% select ( -c ( idxkg, idxval) ) %>% mutate(LE_EURO_TOT = NA ) %>% as.data.frame()       
          
        } else if (species_analysis_type == 'selected_species_sum') {
          
          ## Option 1.2 : by selected species totals
          eflalo_fs_tot = eflalo_fs %>%   mutate(LE_KG_TOT = select(.,contains(c('KG_SOL', 'KG_MAC')) ) %>% rowSums(., na.rm = TRUE) ) %>% select ( -c ( idxkg, idxval) ) %>% as.data.frame()       
          
        } else if (species_analysis_type == 'selected_species_separated') {
          
          ## Option 1.3 : Analysis by species separately 
          eflalo_fs_tot = eflalo_fs %>% select(., -idxkg , -idxval, contains(c('SOL', 'MAC') ) ) %>% filter_at ( vars (contains( c('SOL', 'MAC') ) ) , any_vars(!is.na(.)) ) %>%  as.data.frame()
          
        }
        
        dim( eflalo_fs_tot)
        
      }  else if   (eflalo_format == 'long'  ) { 
        
        
        ## Option 2: Species names in long format. All  species in LE_SPE and  catch value in LE_KG columns
        
        
        captures_total = eflalo_fs %>% group_by(FT_REF, LE_ID) %>% summarise(LE_KG_TOT = sum( LE_KG), LE_EURO_TOT = 0 ) # LE_KG_TOT = sum( LE_KG) if VALUE data is available
        
        
        eflalo_fs_tot = eflalo_fs %>% select(-c (LE_SPE, LE_KG, LE_VALUE)) %>% distinct() %>% inner_join(captures_total , by = c("FT_REF", "LE_ID" ))
        
      } 
      
      tacsatp = tacsatp %>% mutate( FT_REF = SI_FT )
      
      eflaloM =  subset(eflalo_fs_tot,FT_REF %in% unique(tacsatp$FT_REF))
      eflaloNM = subset(eflalo_fs_tot,!FT_REF %in% unique(tacsatp$FT_REF))
    
      
      tacsatp$SI_STATE[which(tacsatp$SI_STATE != "f")] = 0
      tacsatp$SI_STATE[which(tacsatp$SI_STATE == "f")] = 1
      
      tacsatEflalo = tacsatp[tacsatp$SI_STATE == 1,] 
      
      tacsatEflalo = tacsatEflalo %>% filter ( FT_REF %in%  ( eflaloM%>%distinct(FT_REF)%>%pull())   )
      
      
      ## if LE_KG or LE_VALUE is NA , replace by 0s'
      
      
      eflaloM = eflaloM %>% mutate ( LE_VALUE_TOT = 0 )
      eflaloM = eflaloM %>% mutate ( LE_EURO_TOT = LE_VALUE_TOT   )
      
      
      
      
      #- Split among ping the landings to iVMS locations
      
      
      
      tacsatEflalo =  splitAmongPings(
                                      tacsat = tacsatEflalo,
                                      eflalo = eflaloM,
                                      variable = "kgs", # "all",
                                      level = "day",
                                      conserve = FALSE
                                    )
      
      tacsatEflalo%>%filter(!is.na( LE_KG_TOT  ) ) 
      eflaloM%>%summarise(TOT = sum(LE_KG_TOT))
      tacsatEflalo%>%summarise(TOT = sum(LE_KG_TOT))
      
      
      
      save(
        tacsatEflalo,
        file = paste0("workflow_outputs\\tacsatEflalo", year, ".RData")
      )

