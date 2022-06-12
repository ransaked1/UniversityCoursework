SELECT	
	countriesAndTerritories,
	ROUND(SUM(cases)*1.0/popData2020*100, 2),
	ROUND(SUM(deaths)*1.0/popData2020*100, 2)
FROM CasesAndDeaths
INNER JOIN Countries ON country_id=Countries.id
GROUP BY country_id;