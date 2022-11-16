
with a as ( 

select FT_LHAR ,FT_LCOU, LE_SPE,  sum(LE_KG) as total_catch_kg, sum(LE_EURO) as total_catch_val, avg(VE_LEN ) ve_len_mean
from RM12.eflalo_2018_gbw
GROUP BY FT_LHAR,FT_LCOU, LE_SPE


) 

select a.*, b.NAME  
from a
inner join dbo.D_SPECIES b
on a.LE_SPE = b.SPECIES_CODE
 
ORDER BY total_catch_kg DESC 



---  10 QUERY MOST LADED SPECIES IN WALES PORTS



with a as ( 

select  LE_SPE,FT_LCOU, LE_GEAR,  month_y, sum(LE_KG) as total_catch_kg, sum(LE_EURO) as total_catch_val 
from ( select *, MONTH(FT_DDAT ) month_y  from  RM12.eflalo_2018_gbw ) foo 
WHERE FT_LCOU = 'GBW'
GROUP BY   LE_SPE,  FT_LCOU,LE_GEAR, month_y

) 

select a.*, b.NAME  
from a
inner join dbo.D_SPECIES b
on a.LE_SPE = b.SPECIES_CODE
 
ORDER BY month_y,total_catch_kg DESC 
