#!/bin/bash

clear && mvn clean compile --quiet && rm -rf ./out && clear && ./run-class.sh $@
