############################# Load Extracted EFALO and TACSAT data sets into R Environment ########################

## This script follows up from the EFLALO and TACSAT extraction SQL script. The EFLALO and TACSAT must be available in analysts computers with the following names : 

# -   eflalo_ft.csv
# -   eflalo_le.csv
# -   eflalo_spe.csv
# -   tacsat.csv


## Load required libraries

library(lubridate)  ## this package helps with manage date fields 
library(dplyr)      ## R Package to use pipelines (%>%) analysis language 
library(sf)         ## R Package for spatial analysis in R ( Simple Features )
library(ggplot2)    ## R Package for plotitng and graphs



## 1. Load EFLALO data set blocks and merge them into one R data frame. 


## list.files(path = '.\\..\\data') # check the files in your directory 

getwd()
setwd('C:/Users/RM12/OneDrive - CEFAS/Roi/projects/Welsh_Government_Fishing/welsh_gov_fishing_analysis_capacity/welsh_gov_fisheries_toolbox_git')

eflalo_ft = read.csv(file = '.\\..\\data\\eflalo_ft.csv', header = F, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(eflalo_ft)  = c ( "FT_REF", "FT_DCOU", "FT_DHAR", "FT_DDAT", "FT_DTIME", "FT_DDATIM", "FT_LCOU", "FT_LHAR", "FT_LDAT", "FT_LTIME", "FT_LDATIM", "VE_REF", "VE_FLT", "VE_COU","VE_FA", "VE_LEN", "VE_KW", "VE_TON", "FT_YEAR")


  head (eflalo_ft)
  str(eflalo_ft)
  dim(eflalo_ft)
  summary(eflalo_ft)


 

eflalo_le = read.csv(file = '.\\..\\data\\eflalo_le.csv', header = F, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(eflalo_le)  = c ( "LE_ID", "LE_CDAT", "LE_STIME", "LE_ETIME", "LE_SLAT", "LE_SLON", "LE_ELAT", "LE_ELON", "LE_GEAR", "LE_MSZ", "LE_RECT", "LE_DIV", "LE_MET", "EFLALO_FT_FT_REF")


  head (eflalo_le)
  str(eflalo_le)
  dim(eflalo_le)


eflalo_spe = read.csv(file = '.\\..\\data\\eflalo_spe.csv', header = F, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(eflalo_spe)  = c ( "EFLALO_LE_LE_ID", "EFLALO_FT_FT_REF","LE_SPE", "LE_KG", "LE_VALUE"  )


  head (eflalo_spe)
  str(eflalo_spe)
  dim(eflalo_spe)
 
 
## Merge  EFLALO data blocks into one using common fields 

eflalo =    eflalo_ft %>%
            inner_join (eflalo_le , by =  c("FT_REF" = "EFLALO_FT_FT_REF"))%>%
            inner_join(eflalo_spe, by = c("LE_ID" = "EFLALO_LE_LE_ID"   ))

head (eflalo)
str(eflalo)

# Convert the fields in required formats 

eflalo$FT_DDAT =  ymd( eflalo$FT_DDAT   )  ## ymd lubridate function to CAST date into Year Mond Day date format
eflalo$FT_LDAT =   ymd(eflalo$FT_LDAT  ) 
eflalo$LE_CDAT =   ymd_hms(eflalo$LE_CDAT  ) 
eflalo$FT_DDATIM = ymd_hms( eflalo$FT_DDATIM   ) 
eflalo$FT_LDATIM = ymd_hms( eflalo$FT_LDATIM  ) 

eflalo$VE_LEN = as.numeric(eflalo$VE_LEN)

eflalo$Year = year(eflalo$FT_DDATIM )
eflalo$Month = month(eflalo$FT_LDATIM)




## 2. Load TACSAT data    

tacsat = read.csv(file = '.\\..\\data\\tacsat.csv', header = F, sep = ','  , fileEncoding = 'UTF-8-BOM')
names(tacsat)  = c ( "VE_REF", "SI_LATI", "SI_LONG", "SI_DATE", "SI_TIME", "SI_DATIM", "SI_SP", "SI_HE", "SI_HARB", "SI_STATE", "SI_FT", "INTV", "SI_YEAR" )

dim(tacsat)
head (tacsat)
str(tacsat)




tacsat_bk = tacsat  ##create a backup of the data.frame 


tacsat$SI_DATE  =  ymd( tacsat$SI_DATE   )  
tacsat$SI_DATIM  =   ymd_hms(tacsat$SI_DATIM  ) 
tacsat$SI_SP = as.numeric(tacsat$SI_SP)
tacsat$SI_HE = as.numeric(tacsat$SI_HE)

tacsat%>%filter(is.na(SI_SP))%>%dim()

### The EFLALO and TACSAT have been formatted and ready for analysis!!