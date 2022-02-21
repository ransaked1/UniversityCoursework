#!/bin/bash

INPUT="$1"
FILTER="$2"

INPUT=$(cat "${INPUT}" | grep "${FILTER: -2}")

i=0
len=$(echo "${INPUT}" | wc -l)

 while read -r name; do
   OUT=$(curl -sf "http://personal.soton.ac.uk/~$name/index.html");
   if test $?; then
     if [ "${#OUT}" -ne "0" ]; then
       REZ="$REZ$name ${#OUT}\n";
     fi
   fi
   ((i+=1))
   echo -ne "Processed ${i}/${len} students\r"
 done <<< "$INPUT"

echo -e "\n"
echo -e "${REZ}" | sort -nr -k2