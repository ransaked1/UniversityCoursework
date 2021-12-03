
CC = javac

BUGSPATH = bugs/
BUILDINGPATH = building/
STUDENTSPATH = students/

BUGSSRC = bugs/*.java
BUILDINGSRC = building/*.java
STUDENTSRC = students/*.java

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
	@java EcsBuildingDefence 4 20 bugs.txt 210
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
	@cp tests/junit-platform-console-standalone-1.8.1.jar .
	@cp tests/TestPart1.java .
	@cp tests/AbstractTestClass.java .
	@cp tests/TestCoursework.java .
	@cp bugs/*.class .
	@javac -cp .:junit-platform-console-standalone-1.8.1.jar TestPart1.java
	@java -jar junit-platform-console-standalone-1.8.1.jar -cp . -c TestPart1

	@$(RM) junit-platform-console-standalone-1.8.1.jar
	@$(RM) TestPart1.java
	@$(RM) AbstractTestClass.java
	@$(RM) TestCoursework.java
	@make clean

test2: all
	@cp tests/junit-platform-console-standalone-1.8.1.jar .
	@cp tests/TestPart2.java .
	@cp tests/AbstractTestClass.java .
	@cp tests/TestCoursework.java .
	@cp bugs/*.class .
	@cp building/*.class .
	@javac -cp .:junit-platform-console-standalone-1.8.1.jar TestPart2.java
	@java -jar junit-platform-console-standalone-1.8.1.jar -cp . -c TestPart2

	@$(RM) junit-platform-console-standalone-1.8.1.jar
	@$(RM) TestPart2.java
	@$(RM) AbstractTestClass.java
	@$(RM) TestCoursework.java
	@make clean

test3: all
	@cp tests/junit-platform-console-standalone-1.8.1.jar .
	@cp tests/TestPart3.java .
	@cp tests/AbstractTestClass.java .
	@cp tests/TestCoursework.java .
	@cp bugs/*.class .
	@cp building/*.class .
	@javac -cp .:junit-platform-console-standalone-1.8.1.jar TestPart3.java
	@java -jar junit-platform-console-standalone-1.8.1.jar -cp . -c TestPart3

	@$(RM) junit-platform-console-standalone-1.8.1.jar
	@$(RM) TestPart3.java
	@$(RM) AbstractTestClass.java
	@$(RM) TestCoursework.java
	@make clean

.PHONY: clean demo