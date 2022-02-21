#!/bin/bash

INPUT="$1"

 while read -r name; do
   OUT=$(curl -sf "http://personal.soton.ac.uk/~$name/index.html");
   if test $?; then
     if [ "${#OUT}" -ne "0" ]; then
       REZ="$REZ$name ${#OUT}\n";
     fi
   fi
 done < "${INPUT}"

echo -e "${REZ}" | sort -nr -k2
