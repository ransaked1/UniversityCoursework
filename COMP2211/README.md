# Ad Auction Dashboard
## Prerequisites

Java 1.8 is the most common version of Java on consumer machines but you will need the latest Java 17 from [here](https://download.oracle.com/java/17/archive/jdk-17.0.3_windows-x64_bin.exe) to run the game.

Your system must have the Impression Log Source, Click Log Source, and Server Log.  Source for each campaign as .csv files. It is recommended to keep these files in the same folder for each campaign.

## Uploading a new campaign

1. Open the software and click the hamburger logo/three horizontal lines ≡ in the top left corner to open a side menu.
2. Click the blue "Add Campaign" box to open a new window.
3. Type in a campaign name in the input box. It is recommended that the name is clear and descriptive.
4. Click each "Select File" button and choose the impression log, click log, and server log file in that order.
5. Select "Create Campaign" and wait for the files to be processed. Progress bars will indicate the progress. The window will
    automatically close when finished.

## Filtering the Campaign

1. Once the statistics have loaded at the top of the screen, apply the desired filters.
2. Choose filters such as gender, age group, income, context, and bounce definition (time or single page).
    a. Bounce Definition - Time - Bounces will be measured by page visits for less than 30 seconds.
    b. Bounce Definition - Single Page - Bounces will be measured by single page visits.
3. Enter a name for the snapshot in the text box above the graph.
4. Click "Recompute Data" at the bottom of the menu. Wait for the data to load.


### Reading the data - statistics

1. After uploading a campaign and applying filters, the statistics will load at the top of the screen.
2. The "Ad Total Cost" displays the total cost for the campaign.
3. The right-hand side shows different metrics and their values.

### Reading the data - graph

1. In the bottom right corner of the main menu, an interactive graph will represent the data.
2. Above the graph, use the drop-down menu to select the metric represented by the graph. Most graphs are line graphs,
    except for the click distribution graph, which is a histogram.
3. Above the center of the graph, select the granularity of the graph, such as hour, day, or week.
4. Along the bottom of the graph, the time of the data is displayed.
5. Along the side of the graph, the value of the data is displayed based on the selected metric.
6. Hover the mouse over key points to get their exact values.

### Changing the date selection

1. In the top right of the window, select the dates for the campaign.
2. The first date sets the start of the campaign data that is red. Select the start date by clicking on the calendar icon.
3. The second date sets where to end reading data. Select the date by clicking on the calendar icon.

### Comparing graphs with different filters

1. Set and apply the filters to how you want initially. Make sure to set the name of the snapshot in the textbox above the
    graph.
2. Click “Take Snapshot” above the top left of the graph.
3. Change the filters to display the second set of data and name the snapshot.
4. Repeat steps 2-4 to compare more graphs.
5. Click "Reset Graph" to reset the graphs.


### Switching between campaigns

1. Upload at least two campaigns by following the "Adding a new campaign" instruction.
2. Click the drop-down menu at the top of the main window that displays the name of the current campaign.
3. Select the campaign to view.

## Frequently Asked Questions

Q. How many different graphs can you compare at once?<br>
A. You can compare as many as you want.

Q. How many campaigns can I have loaded at once?<br>
A. You can load as many as you want.

Q. How to delete a campaign?<br>
A. Click the three horizontal line icon ≡ in the top left to open the side menu. Next to the campaign you wish to delete, click the red
bin icon.

Q. If I close the software, will I lose my campaign data?<br>
A. No - your campaign data will be stored locally on your device and will be there when you reopen the software.

Q. What do the metrics stand for/mean?<br>
A. The metrics and their definitions include:<br>
● Impressions - The number of times an ad is shown to a user.<br>
● Clicks - The number of times a user clicks on an ad that is shown to them.<br>
● CTR - Click-Through-Rate - The average number of clicks per impression.<br>
● Bounces - The number of times a user clicks on an ad but then fails to interact with the website. You can define
this yourself.<br>
● CPC - Cost-Per-Click - The average amount of money spent on an advertising campaign for each click.<br>
● Uniques - The number of unique users that click on an ad during a campaign.<br>
● CPM - Cost-Per-Thousand Impressions - The average amount of money spent on an advertising campaign for
every thousand impressions.<br>
● Bounce Rate - The average number of bounces per click.<br>
● CPA - Cost-Per-Acquisition - The average amount of money spent on an advertising campaign for each time a
user clicks and then acts on an ad.

Q. How do I see specific graph points?<br>
A. Hover your mouse over the point you wish to read on the graph.

Q. How to change the time granularity?<br>
A. Select Hour, Day, or Week above the graph.


