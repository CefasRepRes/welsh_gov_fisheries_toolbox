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
	
    "LE_ID" bigint,
    "LE_CDAT" date,
    "LE_STIME" time without time zone,
    "LE_ETIME" time without time zone,
    "LE_SLAT" double precision,
    "LE_SLON" double precision,
    "LE_ELAT" double precision,
    "LE_ELON" double precision,
    "LE_GEAR" varchar (20),
    "LE_MSZ" numeric,
    "LE_RECT" varchar (25),
    "LE_DIV" varchar (40),
    "LE_MET" text,
    eflalo_ft_ft_ref bigint
) 
	
	
create table welsh_gov_fish_tool.eflalo_spe  (
	
	 LE_ID bigint,
	 eflalo_ft_FT_REF bigint,
	 LE_SPE varchar (30),
	 LE_KG numeric,
	 LE_VALUE numeric
	)  	  
	
	
create table welsh_gov_fish_tool.eflalo_tacsat  (
	
VE_REF varchar(50),
SI_LATI double precision,
SI_LONG double precision,
SI_DATE date,
SI_TIME time without time zone,
SI_DATIM timestamptz,
SI_SP double precision,
SI_HE integer,
SI_HARB integer,
SI_STATE integer,
SI_FT bigint,
INTV double precision,
SI_YEAR numeric
) 
