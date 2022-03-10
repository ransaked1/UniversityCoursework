#!/bin/bash

csv_input_path=$1
image_output_path=$2
tmp_csv_output=tmp_plot.csv

# Create temporary file containing only location data for plotting
cat $csv_input_path | cut -d, -f2,3 | sed '1d' | sed -e 's/\sN//g' -e 's/\sW//g' -e 's/,/ /g' > $tmp_csv_output

# Run gnuplot script
gnuplot -e "line_csv_path='$tmp_csv_output'; image_output_path='$image_output_path'" plot-locations-on-map.gpi

# Remove temporary file
rm $tmp_csv_output
