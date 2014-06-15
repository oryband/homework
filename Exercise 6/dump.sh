#!/bin/sh

# Dump documents from mongo to json file.

mongoexport -d bitsplease -c users > schemas/fixtures/users.json
mongoexport -d bitsplease -c mails > schemas/fixtures/mails.json
