--- LOGBOOK DATA FROM GEOFISH (EFLALO)
-- FLEET SEGMENT: UK Over 10 meters vessels
-- Filter: 
    -- Date: 2022
    -- UK country: GBW (Welsh vessels)
    
    
--------------------------------------------------
--- EFLALO_FT: EFLALO Fishing Trip information ---
--------------------------------------------------

select *
from eflalo2.eflalo_ft
where ve_cou = 'GBW'
and ft_year = '2022'


-----------------------------------------------
--- EFLALO_LE: EFLALO Log Event information ---
-----------------------------------------------

select *
from eflalo2.eflalo_le
where eflalo_ft_ft_ref in (
	select ft_ref 
	from eflalo2.eflalo_ft
	where ve_cou = 'GBW'
	and ft_year = '2022'
)


--------------------------------------------------
--- EFLALO_SPE: EFLALO Catch Event information ---
--------------------------------------------------

select eflalo_le_le_id, le_spe, sum(le_kg) as le_kg, sum(le_euro) as le_euro, eflalo_ft_ft_ref
from eflalo2.eflalo_spe
where eflalo_ft_ft_ref in (
	select ft_ref 
	from eflalo2.eflalo_ft
	where ve_cou = 'GBW'
	and ft_year = '2022'
)
group by eflalo_le_le_id, le_spe, eflalo_ft_ft_ref


-------------------------------------
--- TACSAT: VMS fishing locations ---
-------------------------------------

select *
from tacsat2.tacsat
where si_ft in (
	select ft_ref 
	from eflalo2.eflalo_ft
	where ve_cou = 'GBW'
	and ft_year = '2022'
)


--------------------------------------------------------------------------------------------------


--- LOGBOOK DATA FROM IFISH (EFLALO)
-- FLEET SEGMENT: UK Over 10 meters vessels fishing in Welsh waters
-- Filter: 
    -- Date: 2022
    -- UK country: Not GBW (Not Welsh vessels)
    

--------------------------------------------------
--- EFLALO_FT: EFLALO Fishing Trip information ---
--------------------------------------------------

select *
from eflalo2.eflalo_ft
where ft_ref in (
	select eflalo_ft_ft_ref from eflalo2.eflalo_le
	where le_rect in ('32E5','35E5','34E6','32E6','35E6','31E7','32E7','35E7','33E4','33E5','30E2','31E2','30E3',
					  '31E3','32E3','30E4','31E4','32E4','34E4','35E4','36E4','31E5','34E5','36E5','31E6','36E6'
					  )
				)
	and ft_year = 2022
	

-----------------------------------------------
--- EFLALO_LE: EFLALO Log Event information ---
-----------------------------------------------

select *
from eflalo2.eflalo_le
where le_rect in ('32E5','35E5','34E6','32E6','35E6','31E7','32E7',
				  '35E7','33E4','33E5','30E2','31E2','30E3','31E3',
				  '32E3','30E4','31E4','32E4','34E4','35E4','36E4',
				  '31E5','34E5','36E5','31E6','36E6'
				  )
and (le_cdat, le_cdat) overlaps ('2022-01-01'::DATE, '2022-12-31'::DATE)

				 
--------------------------------------------------
--- EFLALO_SPE: EFLALO Catch Event information ---
--------------------------------------------------

select *
from eflalo2.eflalo_spe
where eflalo_ft_ft_ref in (
	select eflalo_ft_ft_ref
	from eflalo2.eflalo_le
	where le_rect in ('32E5','35E5','34E6','32E6','35E6','31E7','32E7',
				  	  '35E7','33E4','33E5','30E2','31E2','30E3','31E3',
				  	  '32E3','30E4','31E4','32E4','34E4','35E4','36E4',
				  	  '31E5','34E5','36E5','31E6','36E6'
				  	  )
	and (le_cdat, le_cdat) overlaps ('2022-01-01'::DATE, '2022-12-31'::DATE)
)


-------------------------------------
--- TACSAT: VMS fishing locations ---
-------------------------------------

select *
from tacsat2.tacsat 
where si_ft in (
	select eflalo_ft_ft_ref
	from eflalo2.eflalo_le
	where le_rect in ('32E5','35E5','34E6','32E6','35E6','31E7','32E7',
				  	  '35E7','33E4','33E5','30E2','31E2','30E3','31E3',
				  	  '32E3','30E4','31E4','32E4','34E4','35E4','36E4',
				  	  '31E5','34E5','36E5','31E6','36E6'
				  	  )
	)
and si_year = 2022


