#!/bin/sh

aws s3 rm --recursive s3://ory-dsp-ass2/steps/Count/output;
aws s3 rm --recursive s3://ory-dsp-ass2/steps/Join/output;
aws s3 rm --recursive s3://ory-dsp-ass2/steps/Calculate/output;
