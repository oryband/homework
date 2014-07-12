#!/bin/bash

DIR=$1

echo "@RELATION deptrees" >> $DIR/data
cat $DIR/headers >> $DIR/data
echo "@DATA" >> $DIR/data
cat $DIR/vectors >> $DIR/data
