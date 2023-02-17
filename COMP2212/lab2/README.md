## To test a part
1. Uncomment block for the part in main.hs
2. Generate haskell code: ```alex partX.x -o partX.hs```
3. Compile the result with main: ```ghc -o part partX.hs main.hs```
4. Run the program: ```./part [input file name]```