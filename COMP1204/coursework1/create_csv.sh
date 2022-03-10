#!/bin/bash

# Getting arguments for input and output file
# Variable for regex patterns for later use delimited by '\|'
INPUT="$1"
OUTPUT="$2"
REGEX=".*UTC.*\|.*\<N\>\|.*\<S\>\|,\s.*\<W\>\|,\s.*\<E\>\|.*knots\|.*mb"

# Conversion information and starting progress bar
echo "Converting ${INPUT} -> ${OUTPUT}..."
echo -ne '[                         ] (0%)\r'
sleep 0.3

# Cleaning up the input
# cat - prints contents of file
# sed - removes the html style tags
# awk - assignment in awk removes leading and trailing spaces/tabs
# grep - prints the lines that match the regex expressions
# sed - removes the leading ', ' for West/East elements
echo -ne '[#####                    ] (33%)\r'
CLEAN=$(sed -e 's/<[^>]*>//g' ${INPUT} | awk '{$1=$1};{print}' | grep -o ${REGEX} | sed -e 's/, //g')
sleep 0.3

# Formatting the output
# echo - get the contents of the cleaned input
# sed - remove the 2 duplicate dates from every 7 line block
# tr - replaces new lines with commas
# sed - puts the new line back to create rows of 5 values of interest
echo -ne '[#############            ] (66%)\r'
RESULT=$( echo "${CLEAN}" | sed -n '3~7p;4~7p;5~7p;6~7p;7~7p' | tr "\n" "," | sed 's/,/\n/5;P;D')
sleep 0.3

# Append labels and output the result to csv
echo -ne '[#########################] (100%)\r'
APPEND=$'Timestamp,Latitude,Longitude,MinSeaLevelPressure,MaxIntensity\n'
echo "${APPEND}${RESULT}" > "${OUTPUT}"

# Stop the progress bar
echo -ne '\n'



