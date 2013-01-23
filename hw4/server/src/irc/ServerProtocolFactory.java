package irc;

public interface ServerProtocolFactory<T> {
   AsyncServerProtocol<T> create();
}
