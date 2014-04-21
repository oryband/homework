#!/bin/sh

mvn package && \
mvn exec:java -Dexec.mainClass="com.dsp.ass1.Worker"
