SELECT dateRep,cases
FROM CasesAndDeaths
INNER JOIN Countries ON country_id=Countries.id
INNER JOIN Dates ON date_id=Dates.id
WHERE countriesAndTerritories="United_Kingdom"
ORDER BY year,month,day;