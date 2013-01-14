//package irc;


import java.lang.String;
import java.io.IOException;


public interface TokenizerInterface {

    public String nextToken() throws IOException;
    public boolean isAlive();
}

