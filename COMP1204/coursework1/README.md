These are files provided for COMP1204 CW1.

File info:
- al*.kml 
	These are the kml files containing storm data that you need to process.
	We've given you three files to develop your script against.
	You should generate a storm map for each one (using create_map_plot.sh).
	These map plots should then be added to your report.
- create_map_plot.sh
	This script generates plots from csv files. 
	Your create_csv script should make csv files based on data extracted from the kml files.
	You can then run
		./create_map_plot.sh storm_data.csv plot.png 
	to create a map of the storm data.
	NOTE: For this script to work correctly, it must be in the same location as plot-locations-on-map.gpi and world-50m.txt.
- plot-locations-on-map.gpi
	This is the gnuplot code for creating the storm plots. 
	It is run by the create_map_plot.sh script, so you shouldn't need to run it yourself or modify it in any way.
- report.tex
	This is the Latex report template that you should use for your report.
- world-50m.txt
	This contains information used to draw the world map background in your storm plots. 
	It is used by the plot-locations-on-map.gpi script.
