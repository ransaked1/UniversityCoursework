SELECT
	countriesAndTerritories,
	ROUND(SUM(deaths)*1.0/SUM(cases)*1.0*100, 2) AS percent
FROM CasesAndDeaths
INNER JOIN Countries ON country_id=Countries.id
GROUP BY country_id
ORDER BY percent DESC
LIMIT 10;