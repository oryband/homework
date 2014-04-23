#!/bin/sh

mvn exec:java -Dexec.mainClass="com.dsp.ass1.Manager" \
              -Dexec.args="${*:2}"
