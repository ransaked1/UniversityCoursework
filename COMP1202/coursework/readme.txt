
!!! This submission includes a Makefile for easier setup and testing, read below !!!

// BASIC PART \\

Project structure:
    bugs - contains all the bug classes
    building - contains the building class
    students - contains the student and team classes
    configs - contains game setup examples
    tests - contains the files for unit testings
    root - all other java files, a Makefile and a pdf of the coursework specification

Compile and run:
    make all
    java EcsBuildingDefence [topFloor] [constructionPoints] configs/bugs.txt [knowledgePoints]
    make clean

Commands available:
    make demo - cleans, compiles and runs a demo with the "... 4 20 configs/bugs.txt 210" arguments
    	        Replace with "java EcsBuildingDefence 4 20 configs/easy.txt 10000" inside the Makefile to run
    	        the other game config file.
    make test1 - runs the part1 unit tests provided in the tests folder
    make test2 - runs the part2 unit tests provided in the tests folder
    make test3 - runs the part3 unit tests provided in the tests folder