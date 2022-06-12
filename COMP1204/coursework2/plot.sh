#!/bin/bash

IDS=$(sqlite3 coronavirus.db "SELECT country_id FROM CasesAndDeaths INNER JOIN Countries ON country_id=Countries.id GROUP BY country_id ORDER BY SUM(deaths) DESC LIMIT 10;" | tr '\n' ' ')
NAMES=$(sqlite3 coronavirus.db "SELECT countriesAndTerritories FROM CasesAndDeaths INNER JOIN Countries ON country_id=Countries.id GROUP BY country_id ORDER BY SUM(deaths) DESC LIMIT 10;" | tr '\n' ' ' |  tr '_' '-')

gnuplot -persist <<-EOFMarker
  set key top left autotitle columnheader
  set key reverse Left
  set title 'Cumulative COVID-19 Deaths Top 10'
  set ylabel 'Deaths'
  set xlabel 'Date'
  set grid
  set xdata time
  set datafile separator "|"
  set format x '%d/%m/%Y'
  set timefmt "%d/%m/%Y"
  set xtics mirror rotate by -45
  set rmargin at screen 0.94
  set term png
  set terminal png size 1024,768
  set output "graph.png"
  titles = "$NAMES"
  ids = "$IDS"
  ttl(n) = sprintf("%s", word(titles, n))
  plot for [i=1:10] '< sqlite3 coronavirus.db "SELECT dateRep,SUM(deaths) OVER (ROWS UNBOUNDED PRECEDING) FROM CasesAndDeaths INNER JOIN Countries ON country_id=Countries.id INNER JOIN Dates ON date_id=Dates.id WHERE country_id='.word(ids, i).' ORDER BY year,month,day;"' using 1:2 title ttl(i) w l lw 2
EOFMarker
