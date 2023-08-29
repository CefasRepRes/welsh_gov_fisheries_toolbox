
  
  load(file = '.\\..\\data\\eflalo_gbw_qc.RData' )
  load(file = '.\\..\\data\\tacsat_gbw_qc.RData' )
  
  tacsat_gbw = tacsat_gbw_df
  
  ## define the year for the  analysed data 
  
  year = 2022
  
  
  # 2.1 Merge the TACSAT and EFLALO data together --------------------------------------------
  
  # Merge eflalo and tacsat =================================
  
   
  str(tacsat_gbw)
  str(eflalo_gbw)
 
  dim( eflalo_gbw)
  
  ## Convert EFLALO from LARGE format into WIDE format. This complies with the format required by VMSTOOL 'splitamongpings' function
    ## Depending the format selected we have to choose Option 1 or Option 2 in step '2.3 Dispatch landings/catches among VMS pings'
    ## As a general approach , if we want to analyse all species together choose large format if we want to anlyse a number of species choose wide format
  
  eflalo_gbw = eflalo_gbw%>%
    pivot_wider(id_cols = c( FT_REF, FT_DCOU, FT_DHAR, FT_DDAT, FT_DTIME, FT_DDATIM,
                             FT_LCOU, FT_LHAR, FT_LDAT, FT_LTIME, FT_LDATIM, VE_REF,
                             VE_FLT, VE_COU, VE_FA, VE_LEN, VE_KW, VE_TON, FT_YEAR,
                             LE_ID, LE_CDAT, LE_STIME, LE_ETIME, LE_SLAT, LE_GEAR,
                             LE_MSZ, LE_RECT, LE_DIV, LE_MET, EFLALO_FT_FT_REF, Year,
                             Month, VE_LEN_CAT ),
                names_from = c( LE_SPE), values_from = c( LE_KG, LE_VALUE))
 
 
  dim( eflalo_gbw)
  
  # Assign gear and length to tacsat =================================
    # Consider that one fishing trip can use more than one geear, 
    # we use log events instead of fishing trip to linkboth dataset
  
  ## 1. Assign LOG EVENT  and FISHING TRIP information to VMS locations 
  
    ## Select the fields required from EFLALO to be transferred into TACSAT
  
    eflalo_sel =  eflalo_gbw %>% 
                  select(FT_REF, LE_CDAT, LE_GEAR,LE_MSZ, LE_RECT, LE_MET, VE_LEN, VE_KW, VE_COU) %>%
                  distinct()
  
  
  
    tacsatp =  tacsat_gbw%>%  left_join( eflalo_sel  , by = c("SI_FT" = "FT_REF" ) )    # , "SI_DATE" = "LE_CDAT"  )   )
  
    tacsatp%>%filter ( is.na(LE_GEAR)) ## must be 0 , that means all iVMS records have an associated EFLALO records 
  
  
  
  ## Get the first most used gear by fishing trip to fill VMS records with  not associated Log Event .
    ## As a result some VMS records have not gear associated
  
  ft =    eflalo_sel%>% filter(FT_REF == 10343375608)  %>%arrange( FT_REF , LE_CDAT) %>% 
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
  
  
  tacsatp %>% filter(is.na(VE_LEN))
  
  
   
  # Save not merged tacsat data === 
  
  
  
  ## To proceed with analysis we change the format to a data.frame
  
  tacsatp = tacsatp %>% as.data.frame()
  
  
    # In  this analysis all iVMS has an associated eflallo record
    # since previously we filter VMS records only for existing trips in eflalo
  
  
  # 2.2  Define activity  ---------------------------------------------------------------------
  
  
  # 2.2.1 Calculate time interval between points(if this havent been done before) ======================
 
  minInterval = 1 / 60 ## 1 minute converted in hours (0.01666667 hours)
  
  tacsatp = tacsatp%>%ungroup()%>%
    group_by(VE_REF, SI_FT)%>% arrange (VE_REF, SI_DATIM)%>%
    mutate (INTV =  difftime(SI_DATIM , lag(SI_DATIM), units = "hours" )  )%>%  ## Calculate the difference between a iVMS loction time stamp and previous location to calcualte a fishign effort in a given location
    mutate(interval_mean = mean(INTV , na.rm = T))%>%                           ## Calculate the mean to replace the NA's interval when a VMS location is the 1st of a trip and cannot calculate with a prev. iVMS location
    mutate(INTV = ifelse( is.na(INTV), interval_mean, INTV ))%>%                ## Convert the NA's into a effort represented by the mean of that vessel durign given trip
    select(- interval_mean) %>%ungroup()
  

  # Review the  INTV that are too high e.g. 5x the regular interval rate by trip 
  
  tacsatp %>% group_by(SI_FT) %>% mutate  (   mean_intv_trip = mean ( INTV  )) %>%
    filter ( INTV  > 5 * mean_intv_trip)
  
   
  
  # 2.2.2 Define speed thresholds associated with fishing for gears =====================
  
  
  # Investigate speed pattern through visual inspection of histograms # 
  
 
  ggplot(data = tacsatp, aes(SI_SP)) +
    geom_histogram(
      breaks = seq(0, 10, by =0.4), col = 1) +
    facet_wrap( ~ LE_GEAR, ncol = 4, scales = "free_y") +
    labs(x = "Speed (knots)", y = "Frequency") +
    theme(
      axis.text.y = element_text(colour = "black"),
      axis.text.x = element_text(colour = "black"),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA)
    )
  
  
  
   
  # Create speed threshold object # 
  
  speedarr = as.data.frame( cbind( LE_GEAR = sort(unique(tacsatp$LE_GEAR)), min = NA, max = NA), stringsAsFactors = FALSE)
  
  
  ##  Fill the speed array with expert knowledge on fishign threshold speeds by gear/metier
  
  speedarr$min = rep(1, nrow(speedarr)) # Filled with rule of thumb 1 knot as minimum fishing speed
  speedarr$max = rep(6, nrow(speedarr)) # Filled with rule of thumb 6 knot as maximum fishing speed
  
  ## Change only one gear
  
  speedarr = speedarr %>% mutate( min = if_else(LE_GEAR == 'GN', 0, min ), 
                                  max = if_else(LE_GEAR == 'GN', 1.5, max ))
  
  
  # Analyse activity automated for common gears only. Use the speedarr for the other gears =============== 
  
  autoDetectionGears = c('OTB',  'DRB')
   
  
  subTacsat    = subset(tacsatp, LE_GEAR %in% autoDetectionGears)%>%as.data.frame()
  nonsubTacsat = subset(tacsatp, !LE_GEAR %in% autoDetectionGears)%>%as.data.frame()
   
  
  ## Create the storeScheme object where the speed profiles will be stored
  
    
    storeScheme =  expand.grid(  years = 2022, months = 0,  weeks = 0,  analyse.by = unique(subTacsat[, "LE_GEAR"]) )
    
    storeScheme$peaks <- NA
    storeScheme$means <- NA
    storeScheme$fixPeaks <- FALSE
    storeScheme$sigma0 <- 0.911
    
    
    # Fill the storeScheme values based on analyses of the graphs  
    
    dat = subTacsat %>% filter ( LE_GEAR == 'OTB' )   
    
    datmr = dat %>%mutate (SI_SP = -1 * SI_SP )
    datmr = dat %>% bind_rows(datmr)
    
    xrange = pmin(20, range(datmr$SI_SP), na.rm = TRUE)
    datmr$SI_SP[which(abs(datmr$SI_SP) > 20)] =  NA
    hist(datmr$SI_SP, breaks = seq(xrange[1]- 3, xrange[2] + 3 , 0.5),  main = paste('OTB'), xaxt = "n")
    axis(1, at = seq(xrange[1]- 3, xrange[2] + 3, 1))
    
    # Define mean values of the peaks and the number of peaks when they are different from 5 # 
    
    
    storeScheme$means[which(storeScheme$analyse.by == "DRB")] <- c("-9 -4.5 0 4.5 9")
    storeScheme$peaks[which(storeScheme$analyse.by == "DRB")] <- 5
    storeScheme$means[which(storeScheme$analyse.by == "OTB")] <- c("-9 -4.5 0 4.5 9")
    storeScheme$peaks[which(is.na(storeScheme$peaks) == TRUE)] <- 5
    
    
    
    # storeScheme$means[which(storeScheme$analyse.by == "TBB")] <- c("-11.5 -6 0 6 11.5")
    # storeScheme$means[which(storeScheme$analyse.by == "TB")]  <- c("-11.5 -3 0 3 11.5")
    # storeScheme$means[which(storeScheme$analyse.by == "OTT")] <- c("-9 -3 0 3 9")
    # storeScheme$means[which(storeScheme$analyse.by == "SSC")] <- c("-9 0 9")
    # storeScheme$means[which(storeScheme$analyse.by == "PTB")] <- c("-10 -3 0 3 10")
    # storeScheme$means[which(storeScheme$analyse.by == "HMD")] <- c("-9 0 9")
    # storeScheme$peaks[which(storeScheme$analyse.by == "SSC")] <- 3
    # storeScheme$peaks[which(storeScheme$analyse.by == "DRB")] <- 3
    # storeScheme$peaks[which(storeScheme$analyse.by == "HMD")] <- 3
    # storeScheme$peaks[which(is.na(storeScheme$peaks) == TRUE)] <- 5
  
    
    subTacsat%>%filter(LE_GEAR == 'OTB')
    
     
  acTa = activityTacsat( subTacsat ,units = "year",  analyse.by = "LE_GEAR",storeScheme = storeScheme, plot = TRUE, level = "all")
  subTacsat$SI_STATE = acTa
  subTacsat$ID <- 1:nrow(subTacsat)
  
  subTacsat%>%group_by(LE_GEAR, SI_STATE)%>% summarise(f_min_s = min (SI_SP ), f_max_s = max (SI_SP ))
  ggplot ( subTacsat%>%filter ( LE_GEAR== 'OTB') , aes( x= SI_SP, fill = SI_STATE)  ) + geom_bar ( ) +theme_minimal()
  
  # Check results, and if results are not satisfactory, run analyses again but now with fixed peaks # 
  
  for (iGear in autoDetectionGears) {
    
    subDat <- subset(subTacsat,LE_GEAR == iGear)
    minS = min(subDat$SI_SP[which(subDat$SI_STATE == "s")],  na.rm = TRUE) ## minimum steaming speed
    minF = min(subDat$SI_SP[which(subDat$SI_STATE == "f")],  na.rm = TRUE) ## minimum fishing speed
    
    if(minS < minF) {
      storeScheme$fixPeaks[which(storeScheme$analyse.by == iGear)] = TRUE
      subacTa = activityTacsat(
          subDat, units = "year", analyse.by = "LE_GEAR", storeScheme, plot = FALSE,level = "all"
        )
      subTacsat$SI_STATE[subDat$ID] = subacTa
    }
  }
  
  subTacsat =  subTacsat%>%select(-ID)
  
  subTacsat%>%group_by(LE_GEAR, SI_STATE)%>% summarise(f_min_s = min (SI_SP ), f_max_s = max (SI_SP ))
  
  ggplot ( subTacsat%>%filter ( LE_GEAR== 'OTB') , aes( x= SI_SP, fill = SI_STATE)  ) + geom_bar ( ) +theme_minimal()
  ggplot ( subTacsat%>%filter ( LE_GEAR== 'DRB') , aes( x= SI_SP, fill = SI_STATE)  ) + geom_bar (width = 0.1 ) + theme_minimal()
  
  
  
  
  # Assign for visually inspected gears a simple speed rule classification =============== 
  
      ## This will apply the values in speed array data frame created at the begginign of the process
  
  
  
  metiers = unique(nonsubTacsat$LE_GEAR)
  nonsubTacsat$SI_STATE <- NA
  for (mm in metiers) {
    
    sp_gear =  speedarr %>%filter(LE_GEAR == mm) 
    nonsubTacsat = nonsubTacsat %>% 
                    mutate ( SI_STATE = if_else  ( LE_GEAR == mm & SI_SP >= sp_gear$min & SI_SP <= sp_gear$max, 'f','s' ))
      
   
  }
  
  ## In the dataset we are analysing there aren't NA's or MISC gear , so this section doesn't apply in this example
  
  sp_gear =  speedarr %>%filter(LE_GEAR == 'MIS')
  
  nonsubTacsat =  nonsubTacsat %>% 
                  mutate ( SI_STATE = if_else  ( is.na(LE_GEAR)  & SI_SP >= sp_gear$min & SI_SP <= sp_gear$max, 'f','s' ))
  
  
  nonsubTacsat =  nonsubTacsat%>% mutate (SI_STATE = if_else ( is.na(SI_STATE), 's', SI_STATE)  )  
  
  
  # Combine the two dataset together again into TACSATP =============== 
  
  
  tacsatp = subTacsat %>% bind_rows( nonsubTacsat )%>% arrange(VE_REF, SI_DATIM)   
  
  
  # Set fishing sequences with hauling in the middle to "f" ##################
  
  
  idx =   which(
            tacsatp$SI_STATE[2:(nrow(tacsatp) - 1)] == "h" &
              tacsatp$SI_STATE[1:(nrow(tacsatp) - 2)] == "f" &
              tacsatp$SI_STATE[3:(nrow(tacsatp))    ] == "f" &
              tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[1:(nrow(tacsatp) - 2)] &
              tacsatp$VE_REF[2:(nrow(tacsatp) - 1)] == tacsatp$VE_REF[3:(nrow(tacsatp))]
           ) + 1
  
  tacsatp$SI_STATE[idx] <- "f"
  
  
  
  save( tacsatp, file = file.path(outPath, paste0("tacsatActivity", year, ".RData")) )
  
  message("Defining activity completed")
  
  
  
  # 2.3 Dispatch landings/catches of merged eflalo at the VMS/iVMS ping scale  -------------------------------------------------
  
  
  ## For all the species together 
  
  
  ## Option 1: Names in wide format . Each species catch value has its own column 
 
  idxkg  =  grep("KG", colnames(eflalo_gbw ) )  
  idxval =  grep("VALUE", colnames(eflalo_gbw )  ) 
  
  eflalo_gbw_tot = eflalo_gbw %>% mutate(LE_KG_TOT = select(.,idxkg) %>% rowSums(., na.rm = TRUE) ) %>% select ( -c ( idxkg, idxval) ) %>% mutate(LE_EURO_TOT = NA ) %>% as.data.frame()       
  
  
  ## by species totals
  
  eflalo_gbw_tot = eflalo_gbw %>%   mutate(LE_KG_TOT = select(.,contains(c('KG_SOL', 'KG_MAC')) ) %>% rowSums(., na.rm = TRUE) ) %>% select ( -c ( idxkg, idxval) ) %>% as.data.frame()       
  
  
  ## keep just the species wanted to be analysed
  
  eflalo_gbw_tot = eflalo_gbw %>% select(., -idxkg , -idxval, contains(c('SOL', 'MAC') ) ) %>% filter_at ( vars (contains( c('SOL', 'MAC') ) ) , any_vars(!is.na(.)) ) %>%  as.data.frame()       
  
  dim( eflalo_gbw_tot)
  
  
  
  ## Option 2: Species names in large format . All  species in LE_SPE and  catch value in LE_KG columns
  
  
  captures_total = eflalo_gbw %>% group_by(FT_REF, LE_ID) %>% summarise(LE_KG_TOT = sum( LE_KG), LE_EURO_TOT = 0 ) # LE_KG_TOT = sum( LE_KG) if VALUE data is available
 

  eflalo_gbw_tot = eflalo_gbw %>% select(-c (LE_SPE, LE_KG, LE_VALUE)) %>% distinct() %>% inner_join(captures_total , by = c("FT_REF", "LE_ID" ))
  
  
  ## Save the eflalo with total landings before merge with TACSAT 
  
  save( eflalo_gbw_tot, file = file.path(outPath, paste0("eflaloTotals", year, ".RData")) )
  
 
  tacsatp = tacsatp %>% mutate( FT_REF = SI_FT )
  
  eflaloM =  subset(eflalo_gbw_tot,FT_REF %in% unique(tacsatp$FT_REF))
  eflaloNM = subset(eflalo_gbw_tot,!FT_REF %in% unique(tacsatp$FT_REF))
  
  dim(eflaloM)
  dim(eflaloNM)
  
  
  tacsatp$SI_STATE[which(tacsatp$SI_STATE != "f")] = 0
  tacsatp$SI_STATE[which(tacsatp$SI_STATE == "f")] = 1
  
  tacsatEflalo = tacsatp[tacsatp$SI_STATE == 1,] 
  
  tacsatEflalo = tacsatEflalo %>% filter ( FT_REF %in%  ( eflaloM%>%distinct(FT_REF)%>%pull())   )
  
  
  ## if LE_KG or LE_VALUE is NA , replace by 0s'
  
  
  eflaloM = eflaloM %>% mutate ( LE_VALUE_TOT = 0 )
  eflaloM = eflaloM %>% mutate ( LE_EURO_TOT = LE_VALUE_TOT   )
  
  
  
  
  #- Split among ping the landings to iVMS locations
  
  ?splitAmongPings
 
    tacsatEflalo =
      splitAmongPings(
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
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  
  
  load ( file.path(outPath, paste0("tacsatEflalo", year, ".RData") ) ) 
  print("Dispatching landings completed")
  
  tacsatp%>% filter(SI_STATE == 1 & SI_FT == 10343375608)%>% dim()
  eflaloM %>% filter(FT_REF == 10343375608)
  tacsatEflalo%>% filter(FT_REF == 10343375608) %>% summarise(tot = sum ( LE_KG_TOT))
  
  ##Vessel level 
  
     
  
  eflaloM %>% filter(VE_REF == 'A16337')%>% summarise(tot = sum ( LE_KG_TOT))
  tacsatEflalo%>% filter(VE_REF == 'A16337') %>% summarise(tot = sum ( LE_KG_TOT))
  
  # 2.4 Assign c-square, year, month, quarter, area and create table 1 ----------------------------------------
  
  
  tacsatEflalo$Csquare   =  CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  tacsatEflalo$Year      =  year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month     =  month(tacsatEflalo$SI_DATIM)
  tacsatEflalo$kwHour    =  as.numeric(tacsatEflalo$VE_KW) * tacsatEflalo$INTV  
  tacsatEflalo$cpue      =  tacsatEflalo$LE_KG_TOT / tacsatEflalo$INTV   
  tacsatEflalo$INTV      =  tacsatEflalo$INTV 
  
  
  
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo_output_", year, ".RData"))
  )
  
  
  
 
   
  
  
  # 2.5 Assign  year, month, quarter, area and create table 2 ----------------------------------------
  
  
  
  eflalo_output = eflalo_gbw_tot
  
  eflalo_output$Year      = year(eflalo_output$FT_LDATIM)
  eflalo_output$Month     = month(eflalo_output$FT_LDATIM)
  eflalo_output$INTV      = 1 # 1 day
  
  eflalo_output = eflalo_output %>% group_by(VE_REF, LE_CDAT) %>% mutate ( nr = row_number() ) %>% 
                  mutate ( nr = max(nr) ) %>% 
                  mutate ( INTV = INTV / nr)   %>% 
                  arrange ( VE_REF , LE_CDAT)  %>% as.data.frame()
  
  
   
  
  eflalo_output$kwDays =  as.numeric(eflalo_output$VE_KW) * eflalo_output$INTV
  eflalo_output$tripInTacsat = ifelse(eflalo_output$FT_REF %in% tacsatp$FT_REF, "Y", "N") # Y = Yes and N = No
  
   
  
  
  save(
    eflalo_output,
    file = file.path(outPath, paste0("eflalo_output_", year, ".RData"))
  )
  
  
  
 