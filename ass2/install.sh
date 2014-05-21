#!/bin/sh

aws s3 cp ./target/ass2-1.0-SNAPSHOT-jar-with-dependencies.jar s3://ory-dsp-ass2/jars/$1
