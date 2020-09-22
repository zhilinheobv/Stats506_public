#!/bin/env bash
# Stats 506, Fall 2020
# <1> Update the header with your information. 
# This script serves as a template for Part 1 of 
# the shell scripting activity for week 1. 
#
# Author(s): Group 5
# Updated: September 8, 2020
# 79: -------------------------------------------------------------------------

# preliminary, so you know you've run the script
message="Running week1_part1.sh ... "
echo $message

# a - download data if not present
file="recs2015_public_v4.csv" 
url="https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv"

## if the file doesn't exist
if [ ! -f "$file" ]; then
  wget $url 
fi

# b - extract header row and output to a file with one name per line
new_file="recs_names.txt"

## delete new_file if it is already present
if [ -f "$file" ]; then
  rm "$new_file"
fi

# <4> Write your one liner below.  Consider testing in multiple steps. 
< $file head -n1 | tr "," "\n" > $new_file
# c - get column numbers for DOEID and the BRR weights
# as a comma separated string to pass to `cut`
# <5> write your one liner below
< $new_file grep -n -E "DOEID | BRR"| cut -d : -f1 | tr "\n" ","
# <6> uncomment the next three lines and copy the one liner above
cols=$(
 < $new_file grep -n -E "DOEID | BRR"| cut -d : -f1 | tr "\n" ",")

# Uncomment the line below for testing and development:
 echo $cols

# d - cut out the appropriate columns and save as recs_brrweights.csv
# <7> write your one-liner below
< $new_file cut -f$cols > recs_brrweights.csv

# 79: -------------------------------------------------------------------------