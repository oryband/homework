#!/bin/bash
set -x

DIR=$1
OUTPUT=$DIR"labels"

# Init empty file.
> $OUTPUT

# For each 'part-r-..' file, echo its contents.
for f in $DIR"part-r-*"
do
    cat $f >> $OUTPUT
done
