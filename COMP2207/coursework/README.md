# Distributed File System

## Prerequisites

Java 1.8 is the most common version of java on consumer machines but you will need *Java 17.0.2* from [here](https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html) to compile the code.

### Note
I included a small script to add the environment variables needed. Change the paths to match the location of your jdk installation.<br> Run it with this command:
```
. j17.sh
```

<br></br>

## Usage
### Prerequisites
The controller runs by default on port 12355. If this port is not available you can change it in the Makefile (change all occurances).<br>
You will need a new terminal instance for each component of the file system you run.

### Compile and run a Controller
```
make controller R=[replication factor]
```
The replication factor is an integer representing the minimum number of dstores the controller needs to operate and the number of replicas for each file in the file system.

### Compile and run a DStore (run repeatedly)
```
make dstore P=[port] FOLDER=[path]
```
The port is the port to run the DStore on. Each DStore should have a unique port assigned to it. FOLDER is the path to a folder for the DStore to use to store it's files.

### Compile and run a Client
```
make client
```
You will want to change the code of the ClientMain.java to change the operations the client sends to the controller.

<br></br>

## Other Makefile commands
Removes all the compiled .class files:
```
make clean
```
Removes all object files and executables generated:
```
make fclean
```
