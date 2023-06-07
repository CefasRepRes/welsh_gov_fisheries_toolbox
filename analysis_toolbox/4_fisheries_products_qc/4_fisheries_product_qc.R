


# 3.5   ICES DATSU VOCABULARY CHECKS BEFORE DATA SUBMISSION  ------------------------------------------



##Get vocabulary for mandatory and fields with associated vocabulary using the DATSU API
# install.packages("icesVocab", repos = "https://ices-tools-prod.r-universe.dev")
library(icesVocab)


# TABLE 1 =============================================================


### 3.5.1 Check if C-Squares are within ICES Ecoregions =====================

csquares_d      <-  table1Save%>%
  select('C-square')%>%
  distinct( )

csquares_dcoord <-  cbind ( csquares_d ,  CSquare2LonLat (csqr = csquares_d$`C-square` ,degrees =  0.05)   )
valid_csquare   <-  csquares_dcoord%>%
  filter(SI_LATI >= 30 & SI_LATI <= 90  )%>%
  select('C-square')%>%
  pull()



table1Save      <-  table1Save%>%filter(`C-square` %in% valid_csquare)


### 3.5.2 Check Vessel Lengths categories are accepted ==================================


vlen_ices       <-  getCodeList("VesselLengthClass")
table ( table1Save$VesselLengthRange%in%vlen_ices$Key )  # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$VesselLengthRange %in%vlen_ices$Key,]%>%group_by(VesselLengthRange)%>%select(VesselLengthRange)%>%tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(VesselLengthRange %in% vlen_ices$Key)

### 3.5.3 Check Metier L4 (Gear) categories are accepted =================================

m4_ices         <-  getCodeList("GearTypeL4")
table (table1Save$MetierL4 %in%m4_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$MetierL4 %in%m4_ices$Key,]%>%group_by(MetierL4)%>%select(MetierL4)%>%tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(MetierL4 %in% m4_ices$Key)


### 3.5.4 Check Metier L5 (Target Assemblage) categories are accepted =====================

m5_ices         <-  getCodeList("TargetAssemblage")

table (table1Save$MetierL5 %in%m5_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$MetierL5 %in%m5_ices$Key,]%>%group_by(MetierL5)%>%select(MetierL5)%>%tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(MetierL5 %in% m5_ices$Key)

### 3.5.5 Check Metier L6 (Fishing Activity) categories are accepted =====================

m6_ices         <-  getCodeList("Metier6_FishingActivity")

table (table1Save$MetierL6 %in%m6_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$MetierL6 %in%m6_ices$Key,]%>%group_by(MetierL6)%>%select(MetierL6)%>%tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(MetierL6 %in% m6_ices$Key)

### 3.5.6 Check country codes =====================

cntrcode <- getCodeList("ISO_3166")
table (table1Save$CountryCode %in%cntrcode$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table1Save [ !table1Save$VMSEnabled %in% cntrcode$Key,]%>% group_by(CountryCode) %>% select(CountryCode) %>% tally()

# Correct them if any not valid and filter only valid ones
table1Save      <-  table1Save%>%filter(CountryCode %in% cntrcode$Key)




# TABLE 2  =============================================================


### 3.5.6 Check ICES rect are valid  =====================

statrect_ices <- getCodeList("StatRec")

table (table2Save$ICESrectangle %in%statrect_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$ICESrectangle %in%statrect_ices$Key,]%>%group_by(ICESrectangle)%>%select(ICESrectangle)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(ICESrectangle %in% statrect_ices$Key)



### 3.5.7 Check Vessel Lengths categories are accepted ==================================


vlen_ices       <-  getCodeList("VesselLengthClass")
table ( table2Save$VesselLengthRange%in%vlen_ices$Key )  # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$VesselLengthRange %in%vlen_ices$Key,]%>%group_by(VesselLengthRange)%>%select(VesselLengthRange)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(VesselLengthRange %in% vlen_ices$Key)


### 3.5.8 Check Metier L4 (Gear) categories are accepted =================================

m4_ices         <-  getCodeList("GearTypeL4")
table (table2Save$MetierL4 %in%m4_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$MetierL4 %in%m4_ices$Key,]%>%group_by(MetierL4)%>%select(MetierL4)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(MetierL4 %in% m4_ices$Key)


### 3.5.9 Check Metier L5 (Target Assemblage) categories are accepted =====================

m5_ices         <-  getCodeList("TargetAssemblage")

table (table2Save$MetierL5 %in%m5_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$MetierL5 %in%m5_ices$Key,]%>%group_by(MetierL5)%>%select(MetierL5)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(MetierL5 %in% m5_ices$Key)

### 3.5.10 Check Metier L6 (Fishing Activity) categories are accepted =====================

m6_ices         <-  getCodeList("Metier6_FishingActivity")

table (table2Save$MetierL6 %in%m6_ices$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$MetierL6 %in%m6_ices$Key,]%>%group_by(MetierL6)%>%select(MetierL6)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(MetierL6 %in% m6_ices$Key)


### 3.5.11 Check VMSEnabled categories are accepted =====================


yn <- getCodeList("YesNoFields")

table (table2Save$VMSEnabled %in%yn$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$VMSEnabled %in%yn$Key,]%>%group_by(VMSEnabled)%>%select(VMSEnabled)%>%tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(VMSEnabled %in% yn$Key)


### 3.5.12 Check country codes =====================

cntrcode <- getCodeList("ISO_3166")
table (table2Save$CountryCode %in%cntrcode$Key )   # TRUE records accepted in DATSU, FALSE aren't

# Get summary  of   DATSU valid/not valid records
table2Save [ !table2Save$VMSEnabled %in% cntrcode$Key,]%>% group_by(CountryCode) %>% select(CountryCode) %>% tally()

# Correct them if any not valid and filter only valid ones
table2Save      <-  table2Save%>%filter(CountryCode %in% cntrcode$Key)



# DATSU Vocabulary check finished




# 3.6 DATA QC REPORT (OPTIONAL)   ------------------------------------------



# Null values are only accepted for NON MANDATORY fields

# TABLE 1 =============================================================

# Create the table to check fields formats and number of NA's

table_nas <- NULL
for ( nn in colnames(table1Save)) {
  table_na <- table(table1Save[, nn]%>%is.na() )
  row <- c(field = nn, is_na =  ifelse(is.na (table_na['TRUE']), 0, table_na['TRUE'] ), total_records =  table1Save[, nn]%>%length(), field_type =class(  table1Save[, nn]  ) )
  table_nas <- rbind(table_nas,  row)
}

# Print a summary table in Viewer

gt(
  table_nas%>%as_tibble(),
  rowname_col = 'field'
) %>%
  tab_header(
    title = md('Summary of **Table 1**  number of NA and records types')
  ) %>%
  cols_label(  `is_na.NA`=  md('Number of  <br> NA\'s') ,
               total_records = md('Total <br> records'),
               field_type = md('Field <br> type')
  ) %>%
  tab_footnote(
    footnote = md('Non mandatory fields can include null values if not available'),
    locations = cells_stub( rows = c( 'TotValue', 'AverageGearWidth')  )
  )


# TABLE 2 =============================================================

# Create the table to check fields formats and number of NA's

table_nas <- NULL
for ( nn in colnames(table2Save)) {
  table_na <- table(table2Save[, nn]%>%is.na() )
  row <- c(field = nn, is_na =  ifelse(is.na (table_na['TRUE']), 0, table_na['TRUE'] ), total_records =  table2Save[, nn]%>%length(), field_type =class(  table2Save[, nn]  ) )
  table_nas <- rbind(table_nas,  row)
}

# Print a summary table in Viewer

gt(
  table_nas%>%as_tibble(),
  rowname_col = 'field'
) %>%
  tab_header(
    title = md('Summary of **Table 2**  number of NA and records types')
  ) %>%
  cols_label(  `is_na.NA`=  md('Number of  <br> NA\'s') ,
               total_records = md('Total <br> records'),
               field_type = md('Field <br> type')
  ) %>%
  tab_footnote(
    footnote = md('Non mandatory fields can include null values if not available'),
    locations = cells_stub( rows = c( 'TotValue')  )
  )


# Check if TABLE 1 fishing hours > 0

table( table1$INTV > 0  )

# Check if TABLE 2 fishing days  > 0

table( table2$INTV > 0  )

# End of QC checks





# 3.7 Save the final TABLE 1 and TABLE 2 for datacall submission --------------------------------------------

## Headers and quotes have been removed to be compatible with required submission and ICES SQL DB format.

write.table(table1Save, file.path(outPath, "table1Save.csv"), na = "",row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)
write.table(table2Save, file.path(outPath, "table2Save.csv"), na = "",row.names=FALSE,col.names=FALSE,sep=",",quote=FALSE)




############### DATACALL SUBMISSION USING ICESVMS R PACKAGE (OPTIONAL)  ##################

# R packages required to be installed:
# install.packages(c("icesVMS", "icesConnect"), repos = "https://ices-tools-prod.r-universe.dev")  

library(icesVMS)

# Replace with your ICES user name and you will be requested with your password
icesConnect::set_username('submitter_ices_user_id') 

# icesConnect::ices_token(refresh = TRUE)
# icesConnect::decode_token()$UserEmail # Check the email associated to your ices user name is the correct one

screen_vms_file(file.path(outPath, "table1Save.csv"))  # Submit for screening Table 1
screen_vms_file(file.path(outPath, "table2Save.csv"))  # Submit for screening Table 2



######################################################################