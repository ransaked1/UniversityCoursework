SELECT dateRep,countriesAndTerritories,cases,deaths
FROM CasesAndDeaths
INNER JOIN Countries ON country_id=Countries.id
INNER JOIN Dates ON date_id=Dates.id
ORDER BY year,month,day;