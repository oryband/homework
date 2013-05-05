#!/usr/bin/env bash

# got to ./bin and execute `../test.sh`
clear && cd .. && make clean && make && cd bin/ && chmod 777 ./task0 && ls -lsh && ./flame2 -a task0 -p task0 && ls -lsh && ./task0 arg1 ; cd ~/Documents/splab/lab-4/bin
