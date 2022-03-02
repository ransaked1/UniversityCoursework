#!/bin/bash

input_file=$1
echo Using $input_file input file

if [ -z "$2" ]
  then
    student_list=$(cat $input_file)
  else
    year_filter=$2
    echo Filtering to 20$year_filter students only
    student_list=$(grep "$year_filter"$ $input_file)
fi
n_students=$(echo -e "$student_list" | wc -l)
echo Found $n_students students

echo Parsing website data...
out=""
i=1
for student in $student_list
do
  site_out=$(curl -sf http://personal.soton.ac.uk/~$student/index.html)
  test $? -eq 0 && out+="$student $(echo $site_out | wc -c) \n"
  echo -ne " Progress: $i/$n_students\r"
  i=$((i+1))
done

echo -e "\n\nStudent NumChars"
echo -e "$out" | sort -rnk2
echo Done!