
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
    java EcsBuildingDefence [topFloor] [constructionPoints] configs/[config name] [knowledgePoints]
    make clean

Commands available:
    make demo - cleans, compiles and runs a demo with the "... 4 20 configs/bugs.txt 210" arguments
    	        Replace with "java EcsBuildingDefence 4 20 configs/easy.txt 10000" inside the Makefile to run
    	        the other game config file.
    make test1 - runs the part1 unit tests provided in the tests folder
    make test2 - runs the part2 unit tests provided in the tests folder
    make test3 - runs the part3 unit tests provided in the tests folder

Additional comments:
This is a high level overview. Please consult the Javadocs for more specific explanation on what each
non-specified feature does and how it works.

## Part 1 ##
Implemented classes Bug, ConcurrentModificationBug, NoneTerminationBug and NullPointerBug following
the coursework specification.

Added methods getName, getCurrentHp and compareTo are added to Bug class to get the bug's name/HP
to be later printed and for sorting the ArrayList of bugs respectively.


## Part 2 ##
Implemented class Building following the coursework specification.

Added member variables CMB_DAMAGE, NTB_DAMAGE, NPB_DAMAGE to Building class as constants for a
cleaner code style.

Added public methods compareBugsEqual, getAllBugsReal to Building class to specify the comparison between
two bugs for the sorting of bugs and to get all the in the game, not only the ones inside the building as
the in the specification.

Added private methods applyBuildingDamage, removeMultipleBugs to Building class to calculate the damage
done by a bug and to remove multiple bugs from the building when killed.


## Part 3 ##
Implemented classes Student, AiStudent, CsStudent, CyberStudent, SeStudent following the coursework
specification.

Added getDamage, getDelay, getDelayCounter, levelUpDamage, upgrade signatures to the Student interface.

Added new abstract class AbstractStudent to define the constructor and standard methods getLevel and upgrade
from the specification plus additional getDelay, getDelayCounter, upgradeCost, getDamage, damageBug to
get the special attack delay/counter for it, getting the upgrade cost of the student, the damage the student
can deal and applying that damage to the bug.

Each student class (AiStudent, CsStudent, CyberStudent and SeStudent) implements its own version of
the public methods levelUpDamage, defence (from specification) that calculates the student's damage if
it's leveled up and defends the building as specified. Also, a private method in each class that implements
the special attack for each student.


## Part 4 ##
Implemented class Team following the coursework specification.


## Part 5 ##
Implemented class Battle following the coursework specification.

Added member variable roundNumber to keep track of the step count.

Added public methods addBugs, printGameState to the Battle class that adds bugs to the building and print
the game state to the console.

Added private methods makeDecision, printRoundNumber, printBugsState, printStudentsState, printGeneralInfo
for better code readability in manageTeam when picking the optimal decision and when printing different
parts of the game state to the console.

My manageTeam method optimizes for the highest damage for the lowest price. For this I created a Decision
class that either takes the team or student as constructor and calculates the damage increase in the next
round when recruiting a new student or upgrading the one provided, taking the special attack of the student
into account (if it is triggered in the next step (round)).The decision weight is calculated using the
formula "damage / knowledgePointsCost * 100". manageTeam creates a list of decisions in this manner using
the team object or student as input and puts it in a list of decision objects. The decision list is sorted
and traversed from the highest to lowest executing the first one that can be afforded. This tactic results
in prioritizing recruiting students rather than waiting to upgrade one which mathematically makes sense as
upgrading is pricey for just a 130%, 270%, 320%... increase by levels for a quadratic increase in price
compared to a 100%, 200%, 300% increase for recruiting which increases in price linearly only by 10pts.


## Part 6 ##
Implemented class EcsBuildingDefence following the coursework specification.

Added helper class ConfigReader that takes a file name when initialized and generates a bidimensional
ArrayList of bugs containing all the bug waves to be added to the game or throws an exception.


// EXTENSION \\

Implemented the following extension:
Extend to include students from other programmes in ECS (from ELEC sides, there are 6 different programmes,
see https://www.ecs.soton.ac.uk/undergraduate/find_a_course), including their abilities. Make sure that you
extend your strategy to include these new students.

Added the following classes and their stats/special abilities:
    - EtronicStudent (Electronic Engineering) baseAtk - 6, delay - 15, makes the building immune to bug damage
      every 16 - level rounds for one round.
    - EtricalStudent (Electrical Engineering) baseAtk - 6, delay - 10, repairs the building 2 + level points.
    - EeeStudent (Electronic & Electrical Engineering) baseAtk - 5, delay - 4, attacks the top bug with a
      random multiplier between 1 and 8.
    - AeeStudent (Aerospace Electronic Engineering) baseAtk - 5, delay - 8, knock the top bug out of the building.
    - BeStudent (Biomedical Engineering) baseAtk - 4, delay - 10, has "5 + level" percent chance of upgrading
    itself with no cost. Otherwise, deals twice the damage.
    - MechaStudent (Mechatronic Engineering) baseAtk - 10, delay - 8, attacks the top bug with 2x the damage.
    If it kills the bug proceeds to the next one and so on until failing to kill.\


To implement these changes besides creating new classes for each student and implementing the special power
method had to do the following:

Added public method pushOut to the Bug class to set the currentFloor to -1 and the steps to 0.

Added member variable immune to the Building class and a setter method for it to implement the immunity from
damage for the building.

Modified public method recruitNewStudent to include the new students. Now there is 10% chance of recruiting
each type of student.



