//package irc;

import java.util.ArrayList;
import java.lang.String;


public interface ProtocolInterface {

    public void processInput(String msg,Client client);
    public boolean shouldClose();
    public void connectionTerminated();
    public ArrayList<String> split(String msg);
    public String buildString(ArrayList<String> words);
}
