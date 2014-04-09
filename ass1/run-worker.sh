#!/bin/sh

rm -f ./*.txt ./*.html ./*.png && mvn package && java -cp target\/ass1-1\.0-SNAPSHOT.jar com.dsp.ass1.Worker ToText http://www.fbci.org/ministries/isra_docs/Passover.pdf
