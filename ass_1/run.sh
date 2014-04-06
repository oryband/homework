#!/bin/sh

rm -rf out && javac -cp hadoop-core-0.20.2.jar:pdfbox-1.8.4.jar:commons-io-2.4.jar *.java && jar cvf PDF.jar *.class && /usr/local/bin/hadoop jar PDF.jar PDF inimage out
