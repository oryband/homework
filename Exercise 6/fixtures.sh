#!/bin/sh

# Import fixtures from josn files to mongo.

mongoimport -d db -c users ./fixtures/users.json
mongoimport -d db -c mails ./fixtures/mails.json
