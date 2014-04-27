#!/bin/sh

ARGS=$@
mvn exec:java -Dexec.mainClass="com.dsp.ass1.LocalApplication" \
              -Dexec.args="$ARGS"
