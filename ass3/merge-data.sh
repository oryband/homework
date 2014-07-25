#!/bin/bash
set -x

DIR=$1
ATTRIBUTES=$DIR/attributes
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
for f in $DIR"part-r-*"
do
    cat $f >> $OUTPUT
done
