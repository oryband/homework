#!/bin/sh

# Dump documents from mongo to json file.

mongoexport -d bitsplease -c users > ./fixtures/users.json
mongoexport -d bitsplease -c mails > ./fixtures/mails.json
