Create Database  if not exists ProjectDataMart;
USE seai_ber_schema;
SET GLOBAL event_scheduler = ON;

# Create first table in data mart for BER in 2020 for house holder and window manufacturer attributes with Type of 'Final' or 'Existing'
drop event IF EXISTS SQLProject3;
CREATE EVENT SQLProject3
ON SCHEDULE AT CURRENT_TIMESTAMP + INTERVAL 1 second
ON COMPLETION PRESERVE
DO Create Table ProjectDataMart.RetrofitDataMart  
select DwellingTypeDescr as 'Type',Year_of_Construction as 'YearBuilt', EnergyRating as 'BERCategory', TotalFloorArea, WallArea, FloorArea, WindowArea, DoorArea, RoofArea, 
NoStoreys, UValueWindow, UValueDoor, UValueRoof, UValueFloor, UValueWall, MainSpaceHeatingFuel as 'HeatType', MainWaterHeatingFuel as 'WaterHeatType', 
NoOfChimneys, NoOfOpenFlues, NoOfFansAndVents, NoOfFluelessGasFires, DraftLobby as 'SealedPorch', StructureType, SuspendedWoodenFloor, VentilationMethod,
PercentageDraughtStripped, NoOfSidesSheltered, NoOilBoilerHeatingPumps, CHBoilerThermostatControlled as 'ThermoControledBoiler', OBPumpInsideDwelling 'OilBoilerInside',
NoGasBoilerHeatingPumps, WarmAirHeatingSystem, InsulationType as 'LaggingJacketType', InsulationThickness, DeliveredLightingEnergy, PrimaryEnergyLighting, 
CO2Lighting, LowEnergyLightingPercent, DistributionLosses, WaterStorageVolume, WHMainSystemEff,  WHEffAdjFactor, ThermalMassCategory, 
PrimaryEnergyPumpsFans, PrimaryEnergyMainWater, PrimaryEnergySupplementaryWater, DeliveredEnergyMainSpace, DeliveredEnergySecondarySpace, PrimaryEnergyMainSpace, 
PrimaryEnergySecondarySpace, CO2MainSpace, CO2SecondarySpace, NoCentralHeatingPumps, OBBoilerThermostatControlled as 'OilBoilerThermo'
from seai_ber_schema.assesedber2020_full_schema
where TypeofRating LIKE 'Final%' OR TypeofRating LIKE 'Existing%';
