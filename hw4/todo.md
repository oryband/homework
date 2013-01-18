Eldar: working on sendall function !
Ory: work on client in c++!

**IMPORATANT: This file is NOT fullt updated, please contact me!

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
