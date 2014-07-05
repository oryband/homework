#!/bin/bash

clear && mvn compile --quiet && rm -rf ./out && clear && ./run-class.sh $@
