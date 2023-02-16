create schema welsh_gov_fish_tool; 



create table welsh_gov_fish_tool.eflalo_ft  (

	FT_REF varchar (150),
	FT_DCOU varchar (50),
	FT_DHAR varchar (50),
	FT_DDAT date, 
	FT_DTIME timetz,
	FT_DDATIM timestamptz, 
	FT_LCOU varchar (50),
	FT_LHAR varchar (50),
	FT_LDAT date, 
	FT_LTIME timetz,
	FT_LDATIM timestamptz, 
	VE_REF varchar (50),
	VE_FLT varchar (50),
	VE_COU varchar (50),
	VE_LEN numeric, 
	VE_KW numeric, 
	VE_TON numeric, 
	FT_YEAR numeric 
) 


create table welsh_gov_fish_tool.eflalo_le  (
	
	LE_ID
	LE_CDAT
	LE_STIME
	LE_ETIME
	LE_SLAT 
	LE_SLON 
	LE_ELAT
	LE_ELON
	LE_GEAR
	LE_MSZ
	LE_RECT
	LE_DIV
	LE_MET
	eflalo_ft_FT_REF
) 
	
	
create table welsh_gov_fish_tool.eflalo_spe  (
	
	 LE_ID
	 eflalo_ft_FT_REF
	 LE_SPE
	 LE_KG
	 LE_VALUE
	)  	  
	
	
create table welsh_gov_fish_tool.eflalo_tacsat  (
	
VE_REF
SI_LATI
SI_LONG
SI_DATE
SI_TIME
SI_DATIM
SI_SP
SI_HE
SI_HARB
SI_STATE
SI_FT
INTV
SI_YEAR
) 