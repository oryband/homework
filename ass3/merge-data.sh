#!/bin/bash

DIR=$1

echo "@RELATION deptrees" > $DIR/data.arff
echo "@ATTRIBUTE related {true,false}" >> $DIR/data.arff
cat $DIR/headers >> $DIR/data.arff
echo "@DATA" >> $DIR/data.arff
cat $DIR/vectors >> $DIR/data.arff
