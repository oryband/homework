#!/bin/sh

ARGS=$@
mvn exec:java -Dexec.mainClass="com.dsp.ass1.Worker" \
              -Dexec.args="$ARGS"
