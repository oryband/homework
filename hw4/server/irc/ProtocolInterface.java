//package irc;


import java.lang.String;


public interface ProtocolInterface {

    public void processInput(String msg,Client client);
    public boolean isEnd(String msg);
    public boolean shouldClose();
    public void connectionTerminated();
}
