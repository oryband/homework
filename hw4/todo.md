Eldar:
* working on KICK - done!
* working on LIST - done!
* working on NAMES! - done!
* working on PART - done!
* working on QUIT! - done!
* working on NICK,JOIN - done!
* if user left alone in a room, he become admin - done!


Ory: work on client in c++!


# IMPORATANT: This file is NOT fullt updated, please contact me!
1. The server is working fine ( you can log in as client)
2. After you log in with a Client
3. You can user the numeric replyies: NICK,USER,JOIN.


# TODO:

1. in QUIT section - need to think how to implements "if for some reason
a client connection is closed without the client issing a QUIT comman the
server required to inform upon the nature of the enent"
2. When a client enter NICK and then JOIN and THEN try to write shit
   he need to gey nothing  - fixed!
3. Need to Syncorinized more methods!

--**Do if any time left**--
* if user leave/join room notify all memebers!


# Command
todo:
- need to fix the parameter with #!!!! - done (DO WE NEED AT PART?)


# Client

Mostly just copy code from practical sessions:

1. Client class
2. Sockets

# Server

todo: need to write the while loop as seen in class.

Fileds:
functions:


##Thread Per Client Server

Copied from example - need to be rearrange.

#IrcEncoder (Interface)

todo: FINISHED!

Fileds:
1. Charset

Methods:
1.toBytes(String).
2.fromBytes(byte[]).
3.getCharSet().

#MessageTokenizer (Interface) 

todo: FINISHED!

Fileds:
1. delimiter.
2.InputStreamReader.
3.boolean.

Methods:
1.isAlive().
2.nextToken().

#Protocol

todo: 
* write numericError printer.
* If in Channel there is only 1 person he will become admin!!!!

Fileds:
1. boolean shouldClose.
2. ThreadPerClientServer server.


# Oper - (Handels all data base)

Fileds:
1. Clients.
2. Channels.
3. Commans.

Methods:
1. addClient.
2. isCommandExist.
3. isNickNameExist.
