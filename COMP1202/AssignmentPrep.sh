#!/bin/sh

<<com
Author: Daniel Braghis
ddb1u20@soton.ac.uk
github.com/ransaked1
com

#Usage text variable
usage="Description:
    This script generates a new lab folder cleaned, zipped and organized with the structure required
    Name of the new folder - \"[lab name]_generated\"
    If output is satisfactory delete the original and replace with generated result.
Usage:
    $(basename "$0") [-h] [-n name] [-c labParts] [-z] [-o organize ]
Where:
    -h  Show this help text
    -n  Required parameter for the name of the lab folder you want to prepare
    -c  Required parameter for the number of parts the lab has
    -o  Optional parameter that will organize your Java files into folders
        Example for 2 parts with 1 and 3 files: -o \"1 File1 2 File2 File3 File4\"
        If numbers missing will default to 1
    -z  Optional parameter to zip the lab folder. Organizing must be enabled!
    -r  Optional parameter to remove generated folder after zipping. Zipping must be enabled!
Example usage (lab1):
    Fully prepare lab1: ./AssignmentPrep.sh -c 3 -n lab1 -o \"1 Hello 2 GuessingGame 3 FizzBuzz\" -zr
    No organizing or zipping: ./AssignmentPrep.sh -c 3 -n lab1"

#Initilizing flags for parameters
n_flag=0
c_flag=0
o_flag=0
z_flag=0
r_flag=0

echo "------------------------"

#Parse parameters and arguments
while getopts "n:c:o:zhr" opt;
do
   # shellcheck disable=SC2220
   case "$opt" in
      h ) echo "$usage"
        exit 0 ;;
      n ) echo "Lab name given: $OPTARG"
        n_flag=1
        labName="$OPTARG" ;;
      c ) echo "Lab parts given: $OPTARG"
        c_flag=1
        labPartCount="$OPTARG";;
      o ) echo "File organizing enabled"
        o_flag=1
        orgOptions=$OPTARG ;;
      z ) echo "Zipping enabled"
        z_flag=1 ;;
      r ) echo "Cleanup enabled"
        r_flag=1 ;;
   esac
done

echo "------------------------"
echo ""

#Error checking invalid folder name, required parameters, zipping without organizing, negative lab count
if [ ! -d "$labName" ];
then
  echo "ERROR: Couldn't find folder with name \"$labName\""
exit 2
fi

if [ $n_flag -eq 0 ]
then
  echo "ERROR: Required -n parameter missing"
exit 2
fi

if [ $c_flag -eq 0 ]
then
  echo "ERROR: Required -c parameter missing"
exit 2
fi

if [ $o_flag -eq 0 ] && [ $z_flag -eq 1 ]
then
  echo "ERROR: Can't have zipping enabled without organizing files: -o"
exit 2
fi

if [ $z_flag -eq 0 ] && [ $r_flag -eq 1 ]
then
  echo "ERROR: Can't have cleanup enabled without zipping folder: -z"
exit 2
fi

if [ "$labPartCount" -lt 0 ]
then
  echo "ERROR: Lab cannot have less than 1 part"
exit 2
fi

generatedFolderName="${labName}_generated"
#Create folder for lab name and go into it
#Reset contents if there is anything
mkdir -p "${generatedFolderName}"
rm -r ${generatedFolderName:?}/*
cd "${generatedFolderName}" || exit


#Create subfolders for lab parts
for i in $(seq 1 "$labPartCount");
do
  mkdir -p "${labName}part${i}";
done

#Copy files from original and keep only non-test java files
cp -u ../"${labName}"/* .
# shellcheck disable=SC2035
rm *.class *.jar *Test.java Toolbox.java

#Check if organizing requested
if [ $o_flag -eq 1 ];
then
  #Initializing part index
  folderIndex=1

  # shellcheck disable=SC2068
  # shellcheck disable=SC2039
  for option in ${orgOptions[@]};
  do
    if [ -n "$option" ] && [ "$option" -eq "$option" ] 2>/dev/null; #Check if we got a number
    then
      folderIndex=$option
      if [ $((folderIndex + 0)) -gt $((labPartCount + 0)) ]; #Check that we have folders left
      then
        echo "ERROR: Organizer reached the last existing folder"
      exit 2
      fi

    else #If not a number try to move file to folder
      mv "${option}.java" "${labName}part${folderIndex}"
    fi
  done
fi

#Print resulting file tree
echo "Result file tree:"
cd ..
find "${generatedFolderName}" | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/"

#If option selected zip the generated folder
if [ $z_flag -eq 1 ];
then
  echo ""
  echo "Zipping:"
  zip -r "${labName}.zip" "${generatedFolderName}"
fi

#If option selected remove generated folder
if [ $r_flag -eq 1 ];
then
  echo ""
  echo "Cleaning up"
  rm -rf "${generatedFolderName}"
fi