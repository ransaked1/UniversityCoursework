# 2122-COMP1202-CourseworkTest

Tests Harness for COMP1202 Coursework (AY2021-22)

# Disclaimer #
We prepare some test harness to help with the early parts of the
coursework. Depending on your design, when you come to the later part
of the coursework, the test harness for the early part might no longer
work. For example, your program in Part 4 might not pass the Part 1
Test Harness. The test harness means to guide you with the design and
implementation of the early part of coursework only. **It does not
mean to be exhaustive or complete** for coursework.

# Instructions for Using IntelliJ #
1. Download JUnit platform console standalone 1.8.1 from the Maven
central repository

      https://repo1.maven.org/maven2/org/junit/platform/junit-platform-console-standalone/1.8.1/junit-platform-console-standalone-1.8.1.jar

    and place the downloaded jar file at the top directory of your
    source folder, i.e. *src*

2. Add the JUnit platform console standalone as an external library
   * Right-click on your project and choose *Open Module Settings*

   * Choose *Project Settings -> Modules* and navigate to the
   *Dependencies* tab on the page on the right of the dialog.

   * Select the *+" symbol and choose *JARs or Directories ...* and
     browse to the downloaded jar file.

   * Click *Apply* then *OK* to close the project setting.

3. Add the test class to the project
   * Create a folder name *test* in your project
   * Copy all the Java files (*.java) from this repository to the *test*
   folder
   * Mark the *test* folder as *Test Sources Root* (see Lab 3)

4. Run the test by right-clicking on the *test* folder and choose *Run
   'All Tests'* or individual test files accordingly, e.g. *Run TestPart1*

# Instructions for Using Command Line #
1. Download JUnit platform console standalone 1.8.1 from the Maven
central repository

      https://repo1.maven.org/maven2/org/junit/platform/junit-platform-console-standalone/1.8.1/junit-platform-console-standalone-1.8.1.jar

    and place the downloaded jar file at the top directory of your source folder

2. Download all Java files (*.java) from this repository and place them
at the top directory of your source folder.

3. Make sure that your code in the packages *bugs*, *building*, and
*students* complied by running *javac* at the top directory of your
source folder (depending on which parts you have finished).
    javac bugs/*.java building/*.java students/*.java

4. Compile the test classes. Note that the test classes are written
    using Java reflection so it will not automatically compile your
    source code. For example, to compile ``TestPart1.java``, use the
    following command (depending on your OS).

   * On Windows:

         javac -cp .;junit-platform-console-standalone-1.8.1.jar TestPart1.java


   * On Linux/Mac OSX:

         javac -cp .:junit-platform-console-standalone-1.8.1.jar TestPart1.java


5. Run the test classes (depending on which part that you have
     finished). For example, to run TestPart1, use the following command.

        java -jar junit-platform-console-standalone-1.8.1.jar -cp . -c TestPart1
