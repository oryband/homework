#!/bin/bash

DIR=$1

# Echo title.
echo "@RELATION deptrees" > $DIR/data.arff

# Echo first attribute (class).
echo "@ATTRIBUTE related {true,false}" >> $DIR/data.arff

# Echo all attributes.
cat $DIR/attributes >> $DIR/data.arff

# Echo data title.
echo "@DATA" >> $DIR/data.arff

# Echo all data.
cat $DIR/data >> $DIR/data.arff
