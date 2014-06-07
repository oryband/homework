#!/bin/bash

cd /home/hadoop
wget https://s3.amazonaws.com/ory-dsp-ass2/lucene/lucene-4.8.1.tgz
tar -xzf lucene-4.8.1.tgz
rm -f lib/lucene-*.jar
find lucene-4.8.1 | grep lucene- | grep jar$ | xargs -I {} cp {} lib/
