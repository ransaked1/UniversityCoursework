# Task
To create a SQlite database to represent current Coronavirus data from an Open Data Source, in order to be able to answer questions and perform simple analysis.

report.pdf containts the answers to all the questions and briefly explains the code used for each exercise.

# Dataset
The dataset to be used for this exercise is COVID-19 Coronavirus data from the [EU Open Data Portal](https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/csv/data.csv).

A data dictionary to explain the fields used can be found on the [EU Portal](https://www.ecdc.europa.eu/en/publications-data/data-daily-new-cases-covid-19-eueea-country).

*The dataset may differ from the one used in the project as it is constatnly updated.

# Exercises
## The Relational Model
For this first part, you should answer the questions by looking directly at the data in the dataset and answering them in your LaTeX report.

### EX1: Express the relation directly represented in the dataset file. Assign relevant SQLite data types to each column.

We would suggest representing this in written Relation form and then using a LaTeX table to represent the columns and types.
Look at the data that is in the dataset and express it as a Relation, as seen earlier in the course.
Types: To ease the modelling process, we will be using SQLite data types (INTEGER, TEXT, BLOB, REAL and NUMERIC)
Note: You must use the attribute names exactly as they appear in the dataset

### EX2: List the minimal set of Functional Dependencies (FDs)

Every FD must have only one attribute on the RHS (right hand side)
Where A -> B,C,D this is represented as A -> B, A -> C, A -> D
Every FD must be minimal on it’s LHS (left hand side)
There can be more than one attribute on the LHS, but there should be no attributes on the LHS that add no further information (e.g. If A -> C, then A,B -> C would not be minimal as the B is not adding anything further)
There should be no trivial FDs (a -> a adds nothing of value)
There must be no redundant FDs (those which are already implied by other FDs)

### EX3: From your minimal set of functional dependencies, list the potential candidate keys

### EX4: Identify a suitable primary key, and justify your decision

## Normalisation
For this part, you should be able to answer all these questions by looking at the dataset directly and writing the answers in the report.

Keys: Where possible, you should only introduce new Surrogate Keys where they are necessary. If and where they are necessary, to avoid anomalies, you should explain this in your report with a justification.
Attributes: While you are able to introduce new attributes if you wish, you must not rename or remove or change the values of any of the attributes in the original relation (such as the dateRepresentation, even though it is otherwise broken up). All must appear as originally named in your broken down relations.
NULL values: NULL values are not values in themselves, but represent unknown values in the dataset (you cannot treat all NULL values as the same 'null' value). NULL values can be present throughout the normalisation process, you do not need to remove them. However, you may find you need to introduce surrogate keys in the case where a NULL could or is present in something you would want to be a key or split into a relation, for example.

### EX5: List any partial-key dependencies in the relation as it stands and any resulting additional relations you should create as part of the decomposition.

### EX6: Use decomposition and your answer to the above to achieve 2nd Normal Form, introducing appropriate new relations. List the new relations and their fields, types and keys. Explain the process you took.
Note: When decomposing, you may have options as to what attributes to use - you should use the appropriate attributes identified in your primary key in EX4.

### EX7: List transitive dependencies, if any, in your new relations.

### EX8: Convert your relations into 3rd Normal Form using your answers to the above. List the new relations and their fields, types and keys. Explain the process you took.
Note: Depending on assumptions you have made and earlier processes, you may already be in 3NF - in which case, demonstrate why your table is in 3NF.

### EX9: Finally, convert your relations into Boyce-Codd Normal Form. Justify and explain how your relations are in BCNF.

Note: Originally, this was just explaining whether the dataset was in BCNF or not and why - if you have explained this rather than converting into BCNF, that will be acceptable too. However, to make things easier, we would encourage forming your relation into BCNF where possible if it is not already in BCNF.

## Modelling
Where possible, you should only introduce new Surrogate Keys where they are necessary. If and where they are necessary, to avoid anomalies, you should explain this in your report with a justification.

Attributes: While you may add new attributes if you wish, all original attribute names and their values must remain as they were in the original dataset, you may not rename them or change their values - this ensures consistency with the original dataset and easy importing of new data.
Each SQL statement should be written in the report, as well as saved as ex<number>.sql (e.g. ex11.sql for the first). You should explain the steps for EX10 and provide the steps and SQL for EX11-13. Briefly write down the process you went through to go with it - enough that a person with just your report could reproduce what you have done.

### EX10: Using the CSV import function (.import), import the raw dataset into SQLite into a single table called 'dataset' in an SQLite database called coronavirus.db. Dump (using .dump) this table as dataset.sql (which will include the SQL CREATE and INSERT statements), such that running it will import the full dataset into a fresh SQLite database

Note: You may not change the CSV file - it must be the original provided dataset file. The attribute names must be as in the original file.

### EX11: Write the SQL to create the full normalised representation, including all additional tables (with correct types) with no data and excluding the dataset table. Write the set of SQL statements to ex11.sql, run them on your database and as above, dump the full database at this point to dataset2.sql

You should not modify/create/include the dataset table. The SQL should contain CREATE statements to create any new tables. You should include indexes and foreign keys where appropriate, and list and justify these in your answer.
If you have introduced any surrogate keys, please list and justify them as part of this answer.
The SQL statements to create the tables should be saved as ex11.sql
The entire database at this point should be dumped as dataset2.sql

### EX12: Write INSERT statements using SELECT to populate the new tables from the 'dataset' table. Save these to ex12.sql, run them on your database and as above, dump the database as it stands now to dataset3.sql

The SQL statements to populate the tables from the dataset table should be saved as ex12.sql
The entire database at this point should be dumped as dataset3.sql
Tip: Consider using INSERT OR IGNORE INTO (which won't error on duplicates on your keys) or SELECT DISTINCT to avoid selecting duplicate rows when you pull from the dataset
Warning: If your INSERT statements are containing data values directly, you're doing it wrong! (You should be populating FROM the dataset table)
EX13: Test and ensure that on a clean SQLite database, you can execute dataset.sql followed by ex11.sql followed by ex12.sql to successfully populate your database.

### Commands to setup the database
```
sqlite3 coronavirus.db < dataset.sql
sqlite3 coronavirus.db < ex11.sql
sqlite3 coronavirus.db < ex12.sql
```

## Querying. Write an SQL statement for each of the following:

As well as including the SQL, you should also briefly describe your approach for each in the report.
                                 
### EX14: The worldwide total number of cases and deaths (with total cases and total deaths as columns)

### EX15: The number of cases by date, in increasing date order, for the United Kingdom (with the date representation and number of cases as columns)

### EX16: The number of cases and deaths by date, in increasing date order, for each country (with country, date, number of cases and number of deaths as columns)

### EX17: The total number of cases and deaths as a percentage of the population, for each country (with country, % cases of population, % deaths of population as columns)

### EX18: A descending list of the the top 10 countries, by percentage total deaths out of total cases in that country (with country name and % deaths of country cases as columns)

### EX19: The date against a cumulative running total of the number of deaths by day and cases by day for the united kingdom (with date, cumulative UK deaths and cumulative UK cases as columns)
                                 
### Commands to query the database
```
sqlite3 coronavirus.db < ex14.sql
sqlite3 coronavirus.db < ex15.sql
sqlite3 coronavirus.db < ex16.sql
sqlite3 coronavirus.db < ex17.sql
sqlite3 coronavirus.db < ex18.sql
sqlite3 coronavirus.db < ex19.sql
```

## Extension
### EX20: Using GnuPlot, write a small script (plot.sh) which will, using the data in the SQLite database (called coronavirus.db in the same folder as the script), produce a graph named graph.png in the current working directory with the date on the horizontal axis and the cumulative number of deaths by country on the vertical axis. You should represent the top 10 countries in terms of overall cumulative deaths only.

The graph.png can be written to the current working directory (which will be redirected). Any other files you create or need must be created using mktemp to ensure you can write to them, or they will fail (you will not be able to write other files to the working directory).
For any other temporary files or folders you need as part of your script, you must use mktemp to create these (you can create an entire temporary folder with mktemp and then write inside it if you wish). You cannot guarantee file or folder paths on the marking system.
Cumulative deaths refers to the addition of all the deaths to a specific date. For example: Deaths: 1, 6, 10, 20, Cumulative Deaths would be: 1, 7, 17, 37.
Note: The calls to SQLite must be directly to just sqlite3 (do not specify a folder or path - if you have installed it in such a way, add it to your PATH)
Include an explanation of your script in the report. The full script and resulting graph should be included as an appendix in the report (not counting towards the page limit) and in the archive itself.
                                 
### Running the script
```
sh plot.sh
```
