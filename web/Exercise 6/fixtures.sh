#!/bin/sh

# Import fixtures from josn files to mongo.

mongoimport -d bitsplease -c users ./fixtures/users.json
mongoimport -d bitsplease -c mails ./fixtures/mails.json
