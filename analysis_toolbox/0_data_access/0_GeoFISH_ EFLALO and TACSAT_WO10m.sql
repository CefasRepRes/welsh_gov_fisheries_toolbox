--- LOGBOOK DATA FROM IFISH ( EFLALO)
-- FLEET SEGMENT: UK Over 10 meters vessels
-- Filter: 
    -- Date: 2022
    -- UK country: GBW ( Welsh vessels)
    
    
 
---------------------------------------------------------
--- EFLALO_FT: EFLALO Fishing Trip information     ------
---------------------------------------------------------
 

 with a as ( 

	 select * 
	 from F_VOYAGE a 
	 where  YEAR(DEPARTURE_DATE_TIME) = 2022 OR YEAR(RETURN_DATE_TIME) = 2022

 ) , iFV as ( 

	 select a.*,b.COUNTRY_CODE,  b.NAME, b.LENGTH, b.TONNAGE, b.ENGINE_POWER, b.ADMIN_PORT_CODE 
	 from a 
	 inner join D_VESSEL b 
	 on COUNTRY_CODE = 'GBW' and 
		a.RSS_NO = b.RSS_NO  and
		DEPARTURE_DATE_TIME between VALID_FROM_DATE and VALID_TO_DATE
) 


SELECT DISTINCT
  -- Fishing Trip info section
  CAST (iFV.VOYAGE_ID AS BIGINT) as FT_REF,
  iDPD.COUNTRY_CODE as FT_DCOU,
  iDPD.NAME as FT_DHAR,
  CAST ( iFV.DEPARTURE_DATE_TIME AS DATE) as FT_DDAT,
  CAST (iFV.DEPARTURE_DATE_TIME AS TIME) as FT_DTIME,
  CAST ( iFV.DEPARTURE_DATE_TIME AS DATETIME)   as FT_DDATIM, 
  iDPL.COUNTRY_CODE as FT_LCOU,
  iDPL.NAME as FT_LHAR,
  CAST( iFV.RETURN_DATE_TIME AS DATE) as FT_LDAT,
  CAST( iFV.RETURN_DATE_TIME AS TIME) as FT_LTIME, 
  CAST( iFV.RETURN_DATE_TIME AS DATETIME)  as FT_LDATIM,

  -- Vessel information 
  iFV.RSS_NO as VE_REF,
  NULL as VE_FLT,
  iFV.COUNTRY_CODE as VE_COU,
  iFV.LENGTH as VE_LEN,
  iFV.ENGINE_POWER as VE_KW,
  iFV.TONNAGE as VE_TON,
  YEAR(CAST( iFV.DEPARTURE_DATE_TIME AS DATE)) as FT_YEAR 

  
FROM    iFV     

	left join dbo.D_PORT iDPD on iFV.DEPARTURE_PORT_CODE = iDPD.PORT_CODE
	left join dbo.D_PORT iDPL on iFV.LANDING_PORT_CODE = iDPL.PORT_CODE
	
	


---------------------------------------------------------
--- EFLALO_LE: EFLALO Log Event   information  ------
---------------------------------------------------------
	
 with a as ( 

	 select * 
	 from F_VOYAGE a 
	 where  YEAR(DEPARTURE_DATE_TIME) = 2022 OR YEAR(RETURN_DATE_TIME) = 2022

 ) , iFV as ( 

	 select a.*,b.COUNTRY_CODE,  b.NAME, b.LENGTH, b.TONNAGE, b.ENGINE_POWER, b.ADMIN_PORT_CODE 
	 from a 
	 inner join D_VESSEL b 
	 on COUNTRY_CODE = 'GBW' and 
		a.RSS_NO = b.RSS_NO  and
		DEPARTURE_DATE_TIME between VALID_FROM_DATE and VALID_TO_DATE
) 


select DISTINCT
-- Logbook Event info section
iFA.ACTIVITY_ID as LE_ID, -- Need to back track this to FT_REF + counter within FT_REF
iFA.ACTIVITY_DATE as LE_CDAT,
null as LE_STIME,
null as LE_ETIME,
--MAX(rr.NominalLatitude) as LE_SLAT,
--MAX(rr.NominalLongitude) as LE_SLON,
--MAX(rr.NominalLatitude) as LE_ELAT,
--MAX(rr.NominalLongitude) as LE_ELON,
null as LE_SLAT,
null as LE_SLON,
null as LE_ELAT,
null as LE_ELON,
iFA.GEAR_CODE as LE_GEAR,
iFA.MESH_SIZE as LE_MSZ,
iFA.RECTANGLE_CODE as LE_RECT,
iFA.FAO_FISHING_AREA_CODE as LE_DIV,
--iDE.EFLALO2_AREA as LE_DIV,
null as LE_MET, 
iFA.VOYAGE_ID as eflalo_ft_ft_ref

FROM F_ACTIVITY iFA    
where VOYAGE_ID IN ( SELECT DISTINCT VOYAGE_ID FROM iFV ) 
 





---------------------------------------------------------
--- EFLALO_SPE: EFLALO Catch Event   information  ------
---------------------------------------------------------




 with a as ( 

	 select * 
	 from F_VOYAGE a 
	 where  YEAR(DEPARTURE_DATE_TIME) = 2022 OR YEAR(RETURN_DATE_TIME) = 2022

 ) , iFV as ( 

	 select a.*,b.COUNTRY_CODE,  b.NAME, b.LENGTH, b.TONNAGE, b.ENGINE_POWER, b.ADMIN_PORT_CODE 
	 from a 
	 inner join D_VESSEL b 
	 on COUNTRY_CODE = 'GBW' and 
		a.RSS_NO = b.RSS_NO  and
		DEPARTURE_DATE_TIME between VALID_FROM_DATE and VALID_TO_DATE
) 






  select DISTINCT 
-- Logbook Event info section
iFA.ACTIVITY_ID as LE_ID, -- Need to back track this to FT_REF + counter within FT_REF 
iFC.species_code  LE_SPE,
Sum(LIVE_WEIGHT) as LE_KG, 
Sum(LANDINGS_VALUE) as LE_EURO , 
-- Fishing Trip info section
CAST (iFA.VOYAGE_ID AS BIGINT) as   eflalo_ft_ft_ref

FROM  iFV		  
	inner join F_ACTIVITY iFA  
	ON iFV.VOYAGE_ID = iFA.VOYAGE_ID	 
  inner join dbo.F_CATCH iFC
  on iFC.ACTIVITY_ID = iFA.ACTIVITY_ID

group by  iFA.ACTIVITY_ID, SPECIES_CODE, iFA.VOYAGE_ID













---------------------------------------------------------
--- TACSAT: VMS fishing locations                  ------
---------------------------------------------------------




 with a as ( 

	 select * 
	 from F_VOYAGE a 
	 where  YEAR(DEPARTURE_DATE_TIME) = 2022 OR YEAR(RETURN_DATE_TIME) = 2022

 ) , iFV as ( 

	 select a.*,b.COUNTRY_CODE,  b.NAME, b.LENGTH, b.TONNAGE, b.ENGINE_POWER, b.ADMIN_PORT_CODE 
	 from a 
	 inner join D_VESSEL b 
	 on COUNTRY_CODE = 'GBW' and 
		a.RSS_NO = b.RSS_NO  and
		DEPARTURE_DATE_TIME between VALID_FROM_DATE and VALID_TO_DATE
) 





select distinct 
Sat.RSSNo as VE_REF,
Sat.Latitude as SI_LATI,
Sat.Longitude as SI_LONG,
CAST (Sat.SightingDate as DATE ) as SI_DATE,
CAST (Sat.SightingDate as TIME ) as SI_TIME,
Sat.SightingDate as SI_DATIM, 
Sat.Speed as SI_SP,
Sat.Course as SI_HE,
null as SI_HARB,
null as SI_STATE,
CAST( isnull(iFV.VOYAGE_ID,0) AS BIGINT ) as SI_FT

FROM  iFV

-- select VMS points from selected fishing voyages ---
inner join  dbo.SatSighting Sat 
on Sat.SightingDate between  iFV.DEPARTURE_DATE_TIME and iFV.RETURN_DATE_TIME 
and Sat.RSSNo = iFV.RSS_NO 

























--- LOGBOOK DATA FROM IFISH ( EFLALO)
-- FLEET SEGMENT: UK Over 10 meters vessels fishing in Welsh waters
-- Filter: 
    -- Date: 2022
    -- UK country:  Not GBW ( Not Welsh vessels)
    
    
 
---------------------------------------------------------
--- EFLALO_FT: EFLALO Fishing Trip information     ------
---------------------------------------------------------


-- Identifty the ICES Rectangles within the Welsh waters



 with LE_WW as (
	select   DISTINCT VOYAGE_ID 
	from F_ACTIVITY
	where YEAR(CAST( ACTIVITY_DATE AS DATE)) = 2022
	and RECTANGLE_CODE IN ( '32E5','35E5','34E6','32E6','35E6','31E7','32E7','35E7','33E4','33E5','30E2','31E2','30E3','31E3','32E3','30E4','31E4','32E4','34E4','35E4','36E4','31E5','34E5','36E5','31E6','36E6' ) 
	) 
	, a as ( 

	 select * 
	 from F_VOYAGE a 
	 where  YEAR(DEPARTURE_DATE_TIME) = 2022 OR YEAR(RETURN_DATE_TIME) = 2022
	 and VOYAGE_ID IN (select * from LE_WW ) 
	 

 ) , iFV as ( 

	 select a.*,b.COUNTRY_CODE,  b.NAME, b.LENGTH, b.TONNAGE, b.ENGINE_POWER, b.ADMIN_PORT_CODE 
	 from a 
	 inner join D_VESSEL b 
	 on COUNTRY_CODE != 'GBW' and 
		a.RSS_NO = b.RSS_NO  and
		DEPARTURE_DATE_TIME between VALID_FROM_DATE and VALID_TO_DATE
) 


SELECT DISTINCT
  -- Fishing Trip info section
  CAST (iFV.VOYAGE_ID AS BIGINT) as FT_REF,
  iDPD.COUNTRY_CODE as FT_DCOU,
  iDPD.NAME as FT_DHAR,
  CAST ( iFV.DEPARTURE_DATE_TIME AS DATE) as FT_DDAT,
  CAST (iFV.DEPARTURE_DATE_TIME AS TIME) as FT_DTIME,
  CAST ( iFV.DEPARTURE_DATE_TIME AS DATETIME)   as FT_DDATIM, 
  iDPL.COUNTRY_CODE as FT_LCOU,
  iDPL.NAME as FT_LHAR,
  CAST( iFV.RETURN_DATE_TIME AS DATE) as FT_LDAT,
  CAST( iFV.RETURN_DATE_TIME AS TIME) as FT_LTIME, 
  CAST( iFV.RETURN_DATE_TIME AS DATETIME)  as FT_LDATIM,

  -- Vessel information 
  iFV.RSS_NO as VE_REF,
  NULL as VE_FLT,
  iFV.COUNTRY_CODE as VE_COU,
  iFV.LENGTH as VE_LEN,
  iFV.ENGINE_POWER as VE_KW,
  iFV.TONNAGE as VE_TON,
  YEAR(CAST( iFV.DEPARTURE_DATE_TIME AS DATE)) as FT_YEAR 

  
FROM    iFV     

	left join dbo.D_PORT iDPD on iFV.DEPARTURE_PORT_CODE = iDPD.PORT_CODE
	left join dbo.D_PORT iDPL on iFV.LANDING_PORT_CODE = iDPL.PORT_CODE







---------------------------------------------------------
--- EFLALO_LE: EFLALO Log Event   information  ------
---------------------------------------------------------





		
 with LE_WW as (
	select   DISTINCT VOYAGE_ID 
	from F_ACTIVITY
	where YEAR(CAST( ACTIVITY_DATE AS DATE)) = 2022
	and RECTANGLE_CODE IN ( '32E5','35E5','34E6','32E6','35E6','31E7','32E7','35E7','33E4','33E5','30E2','31E2','30E3','31E3','32E3','30E4','31E4','32E4','34E4','35E4','36E4','31E5','34E5','36E5','31E6','36E6' ) 
	) ,  a as ( 

	 select * 
	 from F_VOYAGE a 
	 where  YEAR(DEPARTURE_DATE_TIME) = 2022 OR YEAR(RETURN_DATE_TIME) = 2022
	 and VOYAGE_ID IN (select * from LE_WW ) 

 ) , iFV as ( 

	 select a.*,b.COUNTRY_CODE,  b.NAME, b.LENGTH, b.TONNAGE, b.ENGINE_POWER, b.ADMIN_PORT_CODE 
	 from a 
	 inner join D_VESSEL b 
	 on COUNTRY_CODE != 'GBW' and 
		a.RSS_NO = b.RSS_NO  and
		DEPARTURE_DATE_TIME between VALID_FROM_DATE and VALID_TO_DATE
) 


select DISTINCT
-- Logbook Event info section
iFA.ACTIVITY_ID as LE_ID, -- Need to back track this to FT_REF + counter within FT_REF
iFA.ACTIVITY_DATE as LE_CDAT,
null as LE_STIME,
null as LE_ETIME,
--MAX(rr.NominalLatitude) as LE_SLAT,
--MAX(rr.NominalLongitude) as LE_SLON,
--MAX(rr.NominalLatitude) as LE_ELAT,
--MAX(rr.NominalLongitude) as LE_ELON,
null as LE_SLAT,
null as LE_SLON,
null as LE_ELAT,
null as LE_ELON,
iFA.GEAR_CODE as LE_GEAR,
iFA.MESH_SIZE as LE_MSZ,
iFA.RECTANGLE_CODE as LE_RECT,
iFA.FAO_FISHING_AREA_CODE as LE_DIV,
--iDE.EFLALO2_AREA as LE_DIV,
null as LE_MET, 
iFA.VOYAGE_ID as eflalo_ft_ft_ref

FROM F_ACTIVITY iFA    
where VOYAGE_ID IN ( SELECT DISTINCT VOYAGE_ID FROM iFV ) 






---------------------------------------------------------
--- EFLALO_SPE: EFLALO Catch Event   information  ------
---------------------------------------------------------



 with LE_WW as (
	select   DISTINCT VOYAGE_ID 
	from F_ACTIVITY
	where YEAR(CAST( ACTIVITY_DATE AS DATE)) = 2022
	and RECTANGLE_CODE IN ( '32E5','35E5','34E6','32E6','35E6','31E7','32E7','35E7','33E4','33E5','30E2','31E2','30E3','31E3','32E3','30E4','31E4','32E4','34E4','35E4','36E4','31E5','34E5','36E5','31E6','36E6' ) 
	) ,  a as ( 

	 select * 
	 from F_VOYAGE a 
	 where  YEAR(DEPARTURE_DATE_TIME) = 2022 OR YEAR(RETURN_DATE_TIME) = 2022
	  and VOYAGE_ID IN (select * from LE_WW ) 

 ) , iFV as ( 

	 select a.*,b.COUNTRY_CODE,  b.NAME, b.LENGTH, b.TONNAGE, b.ENGINE_POWER, b.ADMIN_PORT_CODE 
	 from a 
	 inner join D_VESSEL b 
	 on COUNTRY_CODE != 'GBW' and 
		a.RSS_NO = b.RSS_NO  and
		DEPARTURE_DATE_TIME between VALID_FROM_DATE and VALID_TO_DATE
) 




  select DISTINCT 
-- Logbook Event info section
iFA.ACTIVITY_ID as LE_ID, -- Need to back track this to FT_REF + counter within FT_REF 
iFC.species_code  LE_SPE,
Sum(LIVE_WEIGHT) as LE_KG, 
Sum(LANDINGS_VALUE) as LE_EURO , 
-- Fishing Trip info section
CAST (iFA.VOYAGE_ID AS BIGINT) as   eflalo_ft_ft_ref

FROM  iFV		  
	inner join F_ACTIVITY iFA  
	ON iFV.VOYAGE_ID = iFA.VOYAGE_ID	 
  inner join dbo.F_CATCH iFC
  on iFC.ACTIVITY_ID = iFA.ACTIVITY_ID

group by  iFA.ACTIVITY_ID, SPECIES_CODE, iFA.VOYAGE_ID










---------------------------------------------------------
--- TACSAT: VMS fishing locations                  ------
---------------------------------------------------------



		
 with LE_WW as (
	select   DISTINCT VOYAGE_ID 
	from F_ACTIVITY
	where YEAR(CAST( ACTIVITY_DATE AS DATE)) = 2022
	and RECTANGLE_CODE IN ( '32E5','35E5','34E6','32E6','35E6','31E7','32E7','35E7','33E4','33E5','30E2','31E2','30E3','31E3','32E3','30E4','31E4','32E4','34E4','35E4','36E4','31E5','34E5','36E5','31E6','36E6' ) 
	) ,  a as ( 

	 select * 
	 from F_VOYAGE a 
	 where  YEAR(DEPARTURE_DATE_TIME) = 2022 OR YEAR(RETURN_DATE_TIME) = 2022
	  and VOYAGE_ID IN (select * from LE_WW ) 

 )  , iFV as ( 

	 select a.*,b.COUNTRY_CODE,  b.NAME, b.LENGTH, b.TONNAGE, b.ENGINE_POWER, b.ADMIN_PORT_CODE 
	 from a 
	 inner join D_VESSEL b 
	 on COUNTRY_CODE != 'GBW' and 
		a.RSS_NO = b.RSS_NO  and
		DEPARTURE_DATE_TIME between VALID_FROM_DATE and VALID_TO_DATE
) 


select distinct 
Sat.RSSNo as VE_REF,
Sat.Latitude as SI_LATI,
Sat.Longitude as SI_LONG,
CAST (Sat.SightingDate as DATE ) as SI_DATE,
CAST (Sat.SightingDate as TIME ) as SI_TIME,
Sat.SightingDate as SI_DATIM, 
Sat.Speed as SI_SP,
Sat.Course as SI_HE,
null as SI_HARB,
null as SI_STATE,
CAST( isnull(iFV.VOYAGE_ID,0) AS BIGINT ) as SI_FT

FROM  iFV

-- select VMS points from selected fishing voyages ---
inner join  dbo.SatSighting Sat 
on Sat.SightingDate between  iFV.DEPARTURE_DATE_TIME and iFV.RETURN_DATE_TIME 
and Sat.RSSNo = iFV.RSS_NO 


