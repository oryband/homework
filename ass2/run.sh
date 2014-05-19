#!/bin/sh

ARGS=$@
mvn exec:java -Dexec.mainClass="com.dsp.ass2.WordCount" -Dexec.args="$ARGS"
