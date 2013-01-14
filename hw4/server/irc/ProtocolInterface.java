package irc;


import java.lang.String;


public interface ProtocolInterface {

    public String processInput(String msg);
    public boolean isEnd(String msg);
    public boolean shouldClose();
    public void connectionTerminated();
}
