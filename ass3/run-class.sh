#!/bin/bash

CLASS_NAME=$1
shift
ARGS=$@

mvn exec:java -Dexec.mainClass="com.dsp.ass3.$CLASS_NAME" \
              -Dexec.args="$ARGS"
