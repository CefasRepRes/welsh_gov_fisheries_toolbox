declare @Year int = 2022

select FishingTripDmk, ft.TripIdentifier, ft.StartDatetime, DATEADD(dd, 1, ft.EndDatetime) as EndDatetime, ft.DeparturePortDmk, ft.ArrivalPortDmk, ft.RegisteredFishingVesselDmk, v.RSSNumber
into #U10TripsToExtract
from DM.DimFishingTrip ft with(nolock)
left join DM.DimVesselRegistration v with(nolock) on ft.RegisteredFishingVesselDmk = v.RegisteredFishingVesselDmk
where YEAR(CAST(StartDatetime AS DATE)) = @Year
and TripIdentifier like 'GBR-TRP-SDS-%'
and VE_FA = 'Wales'


select FishingTripDmk, ft.TripIdentifier, ft.StartDatetime, ft.EndDatetime, ft.DeparturePortDmk, ft.ArrivalPortDmk, ft.RegisteredFishingVesselDmk, v.RSSNumber
into #O10TripsToExtract
from DM.DimFishingTrip ft with(nolock)
left join DM.DimVesselRegistration v with(nolock) on ft.RegisteredFishingVesselDmk = v.RegisteredFishingVesselDmk
where YEAR(CAST(StartDatetime AS DATE)) = @Year
and TripIdentifier not like 'GBR-TRP-SDS-%'
and VE_FA = 'Wales'

select *
into #TripsToExtract
from
(
	select * from #U10TripsToExtract
	union
	select * from #O10TripsToExtract
) r

drop table #U10TripsToExtract
drop table #O10TripsToExtract


-- eflao_ft
select distinct
	  x.FishingTripDmk as FT_REF
	, dep.Country3Code as FT_DCOU
	, dep.SiteCode as FT_DHAR
	, CAST(x.StartDatetime as DATE) as FT_DDAT
	, CAST(x.StartDatetime as TIME) as FT_DTIME
	, x.StartDatetime as FT_DDATIM
	, rtp.Country3Code as FT_LCOU
	, rtp.SiteCode as FT_LHAR
	, CAST(x.EndDatetime as DATE) as FT_LDAT
	, CAST(x.EndDatetime as TIME) as FT_LTIME
	, x.EndDatetime as FT_LDATIM
	, v.RSSNumber as VE_REF
	, NULL as VE_FLT
	, v.FlagStateCountryCode as VE_COU
	, v.FisheriesAuthorityName as VE_FA
	, v.LengthOverall as VE_LEN
	, v.PowerKW as VE_KW
	, v.RSSTons as VE_TON
	, YEAR(CAST(x.StartDatetime AS DATE)) as FT_YEAR
from #TripsToExtract x 
inner join DM.DimFishingOperation fo with(nolock) on x.TripIdentifier = fo.TripIdentifier
left join DM.DimVesselRegistration v with(nolock) on x.RegisteredFishingVesselDmk = v.RegisteredFishingVesselDmk
left join DM.DimSite dep with(nolock) on x.DeparturePortDmk = dep.SiteDmk
left join DM.DimSite rtp with(nolock) on x.ArrivalPortDmk = rtp.SiteDmk
where fo.ActivityType = 'DEPARTURE'
order by x.FishingTripDmk

-- eflalo_le
select distinct
	  fo.FishingOperationDmk as LE_ID
	, fo.ActivityOccurrence as LE_CDAT
	, null as LE_STIME
	, null as LE_ETIME
	, null as LE_SLAT 
	, null as LE_SLON 
	, null as LE_ELAT
	, null as LE_ELON
	, gc.FishingGearType as LE_GEAR
	, fg.GearCharacteristicValue as LE_MSZ
	, rec.ICESRectangleCode as LE_RECT
	, fao.QualifiedAreaCode as LE_DIV
	, null as LE_MET
	, x.FishingTripDmk as eflalo_ft_FT_REF
into #LeWithMesh
from #TripsToExtract x 
inner join DM.DimFishingOperation fo with(nolock) on x.TripIdentifier = fo.TripIdentifier
left join DM.FactFishingTripGearCharacteristic fg with(nolock) on fo.FishingOperationDmk = fg.FishingOperationDmk
left join DM.DimFishingGearTypeCharacteristic gc with(nolock) on fg.FishingGearTypeCharacteristicDmk = gc.FishingGearTypeCharacteristicDmk and gc.GearCharacteristicType = 'ME'
left join DM.DimIcesSubRectangle rec with(nolock) on fo.ICESSubRectangleDmk = rec.IcesSubRectangleDmk
left join DM.DimFAOFishingArea fao with(nolock) on fo.FAOFishingAreaDmk = fao.FAOFishingAreaDmk
where fo.ActivityType = 'FISHING_OPERATION'
and gc.FishingGearType is not null
order by x.FishingTripDmk, fo.FishingOperationDmk

select distinct
	  fo.FishingOperationDmk as LE_ID
	, fo.ActivityOccurrence as LE_CDAT
	, null as LE_STIME
	, null as LE_ETIME
	, null as LE_SLAT 
	, null as LE_SLON 
	, null as LE_ELAT
	, null as LE_ELON
	, gc.FishingGearType as LE_GEAR
	, null as LE_MSZ
	, rec.ICESRectangleCode as LE_RECT
	, fao.QualifiedAreaCode as LE_DIV
	, null as LE_MET
	, x.FishingTripDmk as eflalo_ft_FT_REF
from #TripsToExtract x 
inner join DM.DimFishingOperation fo with(nolock) on x.TripIdentifier = fo.TripIdentifier
left join DM.FactFishingTripGearCharacteristic fg with(nolock) on fo.FishingOperationDmk = fg.FishingOperationDmk
left join DM.DimFishingGearTypeCharacteristic gc with(nolock) on fg.FishingGearTypeCharacteristicDmk = gc.FishingGearTypeCharacteristicDmk and gc.GearCharacteristicType <> 'ME'
left join DM.DimIcesSubRectangle rec with(nolock) on fo.ICESSubRectangleDmk = rec.IcesSubRectangleDmk
left join DM.DimFAOFishingArea fao with(nolock) on fo.FAOFishingAreaDmk = fao.FAOFishingAreaDmk
where fo.ActivityType = 'FISHING_OPERATION'
and gc.FishingGearType is not null
and fo.FishingOperationDmk not in (select LE_ID from #LeWithMesh)

union 

select * 
from #LeWithMesh
order by eflalo_ft_FT_REF, LE_ID

drop table #LeWithMesh

-- eflalo_spe
select 
	  fo.FishingOperationDmk as LE_ID
	, x.FishingTripDmk as eflalo_ft_FT_REF
	, s.FAOSpeciesCode as LE_SPE
	, SUM(CASE WHEN foc.Weight <> 0 THEN foc.Weight ELSE foc.WeightApportioned END) as LE_KG
	, NULL as LE_VALUE
from #TripsToExtract x 
inner join DM.DimFishingOperation fo with(nolock) on x.TripIdentifier = fo.TripIdentifier
inner join DM.FactFishingActivityUnlandedCatch foc with(nolock) on fo.FishingOperationDmk = foc.FishingOperationDmk
left join DM.DimSpecies s with(nolock) on s.SpeciesDmk = foc.SpeciesDmk
where fo.ActivityType = 'FISHING_OPERATION'
group by fo.FishingOperationDmk, x.FishingTripDmk, s.FAOSpeciesCode
order by x.FishingTripDmk, fo.FishingOperationDmk

-- tacsat
select distinct
	  x.RSSNumber as VE_REF
	, p.Latitude as SI_LATI
	, p.Longitude as SI_LONG
	, CAST (p.EventOccurrence as DATE ) as SI_DATE
	, CAST (p.EventOccurrence as TIME ) as SI_TIME
	, p.EventOccurrence as SI_DATIM
	, p.SpeedRecorded as SI_SP
	, p.CourseRecorded as SI_HE
	, null as SI_HARB
	, null as SI_STATE
	, x.FishingTripDmk as SI_FT
	, null as INTV
	, YEAR(CAST(p.EventOccurrence AS DATE)) as SI_YEAR
from #TripsToExtract x 
inner join dm.FactVesselPosition p with(nolock) on x.RegisteredFishingVesselDmk = p.RegisteredFishingVesselDmk and p.EventOccurrence between x.StartDatetime and x.EndDatetime
order by x.FishingTripDmk

drop table #TripsToExtract