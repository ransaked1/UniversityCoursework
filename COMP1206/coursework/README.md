# Task

Creating a fully functional online game in Java. The online functionality requires being connected to UoS network as the server is on university machines but you can test the game in offline mode. Read the full list of features below.

## Game features to implement

* Play a piece in a valid location
* Reject a piece in an invalid location
* A new random piece generated each time
* Lines are cleared
* Score, Level, Lives shown in the UI
* Scoring works	
Multiplier works
Level works
Background music
Menu screen present and functioning
Menu screen uses animations and visual effects
Menu screen works: launches other scenes
Instructions screen
Instructions screen shows dynamically generated game blocks
Challenge screen shows current piece
Current piece updates on play
Escape navigates back
Challenge screen shows next piece
Piece can be rotated
Piece can be swapped
Sound effects added
Keyboard support for move and drop
Blocks have updated graphics and are not just a single colour square
Placement indicator (e.g. circle) shown on current piece
Blocks are placed on their centre
Hover effect on blocks
Graphics	Fade out effect when blocks cleared
Countdown shown in UI
Countdown animates
Countdown changes when time nearly out
Life lost and new piece when timer expires
Game over when no more lives left
Game goes to Scores screen on Game Over
Scores read from file
Scores saved to file
New score prompts for name
New score inserted into score list
High Score to beat shown in Challenge screen
Online scores shown in Score screen
Lobby updates games as new ones created
Can create a game
Can join a game
Can send a chat message
Can receive a chat message
Can start a game (if host)
Game starts when started by another player (when not host)
Error states handled
Can play a multiplayer game with a friend/second client
Scores update during multiplayer game
Eliminated players shown during multiplayer game
Scores shows multiplayer game scores
Full javadoc must be generated with mvn javadoc:javadoc.

## VIDEO DEMO

[![VIDEO DEMO](https://img.youtube.com/vi/CcnZl6FBO6I/0.jpg)](https://www.youtube.com/watch?v=CcnZl6FBO6I)

# Quick start

## Prerequisites

Java 1.8 is the most common version of java on consumer machines but you will need the latest Java 17 from [here](https://download.oracle.com/java/17/archive/jdk-17.0.3_windows-x64_bin.exe) to run the game.

## Usage
### Compile
```
mvn compile
```

### Run
```
mvn javafx:run
```

### Generate JavaDoc
```
mvn javadoc:javadoc
```

## Documentation

## Built With
* [Pytest 6.2.2](https://docs.pytest.org/en/stable) - testing library used
* [Coverage 5.4](https://coverage.readthedocs.io) - code coverage library used
