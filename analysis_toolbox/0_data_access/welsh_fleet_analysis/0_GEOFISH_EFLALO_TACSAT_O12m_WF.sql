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


