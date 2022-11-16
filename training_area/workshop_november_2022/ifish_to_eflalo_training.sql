/****** Script for SelectTopNRows command from SSMS  ******/

WITH iFV as ( 

	SELECT *
	  FROM [CEDER].[dbo].[F_VOYAGE]
	  where VOYAGE_ID = 610674854
	  -- DEPARTURE_DATE_TIME between '2021-11-11 06:00:00.000' AND '2021-11-18 06:00:00.000'
	   
) , 

iFA as (

	SELECT * 
	FROM dbo.F_ACTIVITY
	WHERE VOYAGE_ID IN ( SELECT DISTINCT VOYAGE_ID FROM iFV ) 
	

) , 

iFC as ( 

	SELECT * 
	FROM dbo.F_CATCH
	WHERE ACTIVITY_ID IN (SELECT DISTINCT ACTIVITY_ID FROM iFA  ) 


) 


select 
iFV.VOYAGE_ID as ft_ref ,
iFV.RSS_NO as ve_ref, 
iFV.DEPARTURE_DATE_TIME as ft_ddatim,
iFV.RETURN_DATE_TIME as ft_ldatim, 
iFV.TARGET_SPECIES as ve_flt,
iFA.ACTIVITY_ID as le_id,
iFA.ACTIVITY_DATE as le_cdat,
iFA.GEAR_CODE as le_gear,
iFA.RECTANGLE_CODE as le_rect, 
iFC.SPECIES_CODE as le_spe,
iFC.LIVE_WEIGHT as le_kg, 
iFC.LANDINGS_VALUE as le_val

from iFA
inner join iFV
ON iFV.VOYAGE_ID = iFA.VOYAGE_ID
inner join iFC
ON iFA.ACTIVITY_ID = iFC.ACTIVITY_ID



   
