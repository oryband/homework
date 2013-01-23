package irc;

public interface TokenizerFactory<T> {
   MessageTokenizer<T> create();
}
