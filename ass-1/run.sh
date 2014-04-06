#!/bin/sh

rm -rf out && mvn package && hadoop jar target\/ass1-1\.0-SNAPSHOT.jar com.dsp.ass1.PDF intext out
