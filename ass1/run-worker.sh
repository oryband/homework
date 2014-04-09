#!/bin/sh

rm -f ./*.txt ./*.html ./*.png && \
mvn package && \
mvn exec:java -Dexec.mainClass="com.dsp.ass1.Worker" \
              -Dexec.args="ToText http://www.fbci.org/ministries/isra_docs/Passover.pdf"
