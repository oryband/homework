Eldar: next: ThreadPerClientServer working well - now need to wrirte the protocol
            by the irc protocol.
Ory: Studying hard!

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
