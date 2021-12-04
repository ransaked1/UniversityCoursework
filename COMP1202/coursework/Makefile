# This is the Makefile for the EcsBuildingDefence coursework.                                    \
It very conveniently compiles and runs code automatically to then clean everything up            \
The options available are:                                                                       \
	make demo - cleans, compiles and runs a demo with the "4 20 configs/bugs.txt 210" arguments  \
	            Replace with java EcsBuildingDefence 4 20 configs/easy.txt 10000 for another test\
	make test1 - runs the part1 unit tests provided in the tests folder                          \
	make test2 - runs the part2 unit tests provided in the tests folder                          \
	make test3 - runs the part3 unit tests provided in the tests folder                          \
	make clean - removes all the class files and the utility files used for testing              \
	make all - utilitarian option for recompiling in the other options

CC = javac

BUGSPATH = bugs/
BUILDINGPATH = building/
STUDENTSPATH = students/

BUGSSRC = bugs/*.java
BUILDINGSRC = building/*.java
STUDENTSRC = students/*.java

JUNIT = junit-platform-console-standalone-1.8.1.jar

FLAGS = -d
RM = rm -rf

SRC = *.java
CLASS = *.class

demo: clean
	@$(CC) $(FLAGS) $(BUILDINGPATH) $(BUGSSRC)
	@$(CC) $(FLAGS) $(STUDENTSPATH) $(BUGSSRC)
	@$(CC) $(BUGSSRC)
	@$(CC) $(FLAGS) $(STUDENTSPATH) $(BUILDINGSRC)
	@$(CC) $(BUILDINGSRC)
	@$(CC) $(STUDENTSRC)
	@$(CC) EcsBuildingDefence.java
	@echo "MAKE DEMO IS DONE"
	@java EcsBuildingDefence 4 20 configs/bugs.txt 210
	@make clean

all:
	@$(CC) $(FLAGS) $(BUILDINGPATH) $(BUGSSRC)
	@$(CC) $(FLAGS) $(STUDENTSPATH) $(BUGSSRC)
	@$(CC) $(BUGSSRC)
	@$(CC) $(FLAGS) $(STUDENTSPATH) $(BUILDINGSRC)
	@$(CC) $(BUILDINGSRC)
	@$(CC) $(STUDENTSRC)
	@$(CC) EcsBuildingDefence.java
	@echo "MAKE ALL IS DONE"

clean:
	@$(RM) bugs/$(CLASS)
	@$(RM) students/$(CLASS)
	@$(RM) building/$(CLASS)
	@$(RM) building/bugs
	@$(RM) students/bugs
	@$(RM) students/building
	@$(RM) *.class
	@echo "MAKE CLEAN IS DONE"

test1: all
	@cp tests/$(JUNIT) .
	@cp tests/TestPart1.java .
	@cp tests/AbstractTestClass.java .
	@cp tests/TestCoursework.java .
	@cp bugs/*.class .
	@javac -cp .:$(JUNIT) TestPart1.java
	@java -jar $(JUNIT) -cp . -c TestPart1

	@$(RM) $(JUNIT)
	@$(RM) TestPart1.java
	@$(RM) AbstractTestClass.java
	@$(RM) TestCoursework.java
	@make clean

test2: all
	@cp tests/$(JUNIT) .
	@cp tests/TestPart2.java .
	@cp tests/AbstractTestClass.java .
	@cp tests/TestCoursework.java .
	@cp bugs/*.class .
	@cp building/*.class .
	@javac -cp .:$(JUNIT) TestPart2.java
	@java -jar $(JUNIT) -cp . -c TestPart2

	@$(RM) $(JUNIT)
	@$(RM) TestPart2.java
	@$(RM) AbstractTestClass.java
	@$(RM) TestCoursework.java
	@make clean

test3: all
	@cp tests/$(JUNIT) .
	@cp tests/TestPart3.java .
	@cp tests/AbstractTestClass.java .
	@cp tests/TestCoursework.java .
	@cp bugs/*.class .
	@cp building/*.class .
	@javac -cp .:$(JUNIT) TestPart3.java
	@java -jar $(JUNIT) -cp . -c TestPart3

	@$(RM) $(JUNIT)
	@$(RM) TestPart3.java
	@$(RM) AbstractTestClass.java
	@$(RM) TestCoursework.java
	@make clean

.PHONY: clean demo