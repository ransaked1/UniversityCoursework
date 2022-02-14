#!/bin/bash

for file in ./*
do
	numLines=$(wc -l < $file)
	echo The file $file contains $numLines lines of text
done