#!/bin/bash

DIR=$1
INPUT=$DIR"labels"
OUTPUT=$DIR../data/"attributes"

# Init empty file.
> $OUTPUT

# Wrap attributes with WEKA string.
echo Wrapping text...

cat $INPUT | while read line
do
    echo "@ATTRIBUTE '"$(printf %q $line)"' INTEGER" >> $OUTPUT
done
