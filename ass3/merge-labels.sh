#!/bin/bash

DIR=$1

# Init empty file.
echo "" > $DIR"labels"

# For each 'part-r-..' file, echo its contents.
for f in $DIR"part-r-*"
do
    cat $f >> $DIR"labels"
done
