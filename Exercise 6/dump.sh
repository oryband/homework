#!/bin/sh

# Dump documents from mongo to json file.

mongoexport -d db -c contacts > schemas/fixtures/contacts.json
mongoexport -d db -c users > schemas/fixtures/users.json
mongoexport -d db -c mails > schemas/fixtures/mails.json
