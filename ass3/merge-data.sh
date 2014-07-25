#!/bin/bash
set -x

DIR=$1
ATTRIBUTES=$DIR/attributes
DATA=$DIR/data
OUTPUT=$DIR/data.arff

# Echo title.
echo "@RELATION deptrees" > $OUTPUT

# Echo first attribute (class).
echo "@ATTRIBUTE related {true,false}" >> $OUTPUT

# Echo all attributes.
cat $ATTRIBUTES >> $OUTPUT

# Echo data title.
echo "@DATA" >> $OUTPUT

# Echo all data.
cat $DATA >> $OUTPUT
