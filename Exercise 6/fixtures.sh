#!/bin/sh

# Import fixtures from josn files to mongo.

mongoimport -d db -c contacts contacts.json
mongoimport -d db -c users users.json
mongoimport -d db -c mails mails.json
