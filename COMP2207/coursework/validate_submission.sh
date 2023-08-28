#!/bin/bash

files=(Controller.java Dstore.java)
cport=12345
r=3
dport1=12346
dport2=12347
dport3=12348
timeout=100
rebalance_period=10000
default_wait_time=5

script_error() {
	echo "Error, script terminated"
	exit 1
}

check_status() {
	if [ ! $? -eq 0 ]
	then
		script_error
	fi
}

silently_kill() {
	{ kill -9 $1 && wait $1; } 2>/dev/null
}

if [ -z $1 ]; then
    echo "submission zip file expected"
	script_error
fi

if [ -z $2 ]
then
	wait_time=${default_wait_time}
else
	wait_time=$2
fi

echo
echo "This script will validate your COMP2207 2021/22 coursework submission by checking whether:"
echo -e "\t - the zip file can be unzipped (this requires the unzip command to be installed)"
echo -e "\t - all required files are there"
echo -e "\t - all the included Java source files can be compiled"
echo -e "\t - Controller and Dstore processes can be started"
echo
echo "If the validation succeeds, you will see some warnings about the Security Manager. This is fine, further explanation is provided in the module wiki page."
echo
if [ ${wait_time} -gt 0 ]; then
	echo "Controller and Dstore processes will be terminated after ${wait_time} seconds. To let these processes run after the validation is completed, you can use a negative integer value as second parameter for this script."
else
	echo "Controller and Dstore processes will not be terminated, you can test them using your client. To terminate these processes once the validation completes, you can use the second parameter for this script to specify how many seconds to wait before terminating them."
fi
echo
echo "Please make sure that the the validation of your zip is successful before submitting it."
echo
echo "This script will unzip files in the ./tmp/ directory, and compile and execute your software in the ./run/ directory. Both folders will be created in the current working directory. If those directories already exist, they will first be deleted, together with all their content."
echo
read -p "Are you sure you want to continue (y/n)" -n 1 -r
echo
echo

if [[ $REPLY =~ ^[Yy]$ ]]
then
	echo "*** Validation started"
	echo ""
else
	echo "Script not executed"
	exit 0
fi

if [ -d tmp/ ]; then
	echo -n "tmp directory already exists, deleting it..."
	rm -fdr tmp/
	check_status
	echo "ok"
fi

echo -n "Creating tmp directory..."
mkdir tmp
check_status
echo "ok"

echo -n "Unzipping submission file..."
unzip $1 -d tmp/ > /dev/null
check_status
echo "ok"

echo -n "Checking all required files exist..."
cd tmp/
for f in ${files[@]}; do
	if [ ! -f "$f" ]; then
		echo "$f does not exist - maybe this file is included in a directory inside the zip? Please make sure the zip does not include any folder."
		# exit 1
		script_error
	fi
done
echo "ok"

cd ..

if [ -d run/ ]; then
	echo -n "run directory already exists, deleting it..."
	rm -fdr run/
	check_status
	echo "ok"
fi

echo -n "Creating run directory..."
mkdir run
check_status
echo "ok"

echo -n "Copying Java policy file to run folder..."
if [ -f ./my_policy.policy ]
then
	cp ./my_policy.policy ./run/
	check_status
	echo "ok"
else
	echo "couldn't find my_policy.policy Please make sure this file is in the current directory. This file was included in the local validation zip you downloaded from the module wiki page."
	script_error
fi

cd run/

echo "Compiling Java sources..."
javac ../tmp/*.java -d .
check_status
echo "Ok, compilation succeeded."

echo "Starting Controller on port ${cport} with replication factor ${r}, timeout ${timeout}ms and rebalance period ${rebalance_period} seconds ..."
java -Djava.security.manager -Djava.security.policy=my_policy.policy Controller $cport $r $timeout $rebalance_period &
controllerId=$!
check_status
sleep 1s
echo "Ok, Controller started"

echo "Starting 3 Dstore processes on ports ${dport1}, ${dport2} and ${dport3} with timeout ${timeout}ms..."
mkdir file_folder_$dport1
java -Djava.security.manager -Djava.security.policy=my_policy.policy Dstore $dport1 $cport $timeout file_folder_$dport1 &
d1Id=$!
check_status
mkdir file_folder_$dport2
java -Djava.security.manager -Djava.security.policy=my_policy.policy Dstore $dport2 $cport $timeout file_folder_$dport2 &
d2Id=$!
check_status
mkdir file_folder_$dport3
java -Djava.security.manager -Djava.security.policy=my_policy.policy Dstore $dport3 $cport $timeout file_folder_$dport3 &
d3Id=$!
check_status
echo "Ok, Dstores started"

echo ""
echo "*** Validation successfully completed!"
echo "Note that this just means that your zip likely complies with the Submission Requirements reported in section 5 of the coursework description. Please also check above for java.security.AccessControlException error messages, as they would indicate your code does not comply with the Security Manager requirements (see the the module wiki page for further information)."
echo ""

if [ $wait_time -gt 0 ]
then
	echo "Waiting ${wait_time} seconds before terminating Controller and Dstore processes..."
	sleep ${wait_time}s
	check_status

	echo -n "Killing Controller and Dstore processes..."
	silently_kill $d1Id 
	silently_kill $d2Id
	silently_kill $d3Id
	silently_kill $controllerId
	echo "ok"
else
	echo "Controller and Dstore processes are still running, you can test them using your client. Note that you should terminate these processes manually."
fi
