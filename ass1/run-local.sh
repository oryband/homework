#!/bin/sh

mvn exec:java -Dexec.mainClass="com.dsp.ass1.LocalApplication" \
              -Dexec.args="${*:2}"
