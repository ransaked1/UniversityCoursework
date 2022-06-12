/* */
INSERT INTO Dates SELECT DISTINCT NULL,dateRep,day,month,year FROM dataset LIMIT -1 OFFSET 1;
/* */
INSERT INTO Countries SELECT DISTINCT NULL,countriesAndTerritories,geoId,countryterritoryCode,popData2020,continentExp FROM dataset LIMIT -1 OFFSET 1;
/* */
INSERT INTO CasesAndDeaths
SELECT
	Dates.id,
	Countries.id,
	dataset.cases,
	dataset.deaths
FROM dataset 
INNER JOIN Dates ON Dates.dateRep = dataset.dateRep 
INNER JOIN Countries ON Countries.countriesAndTerritories = dataset.countriesAndTerritories;