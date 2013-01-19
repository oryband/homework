//package irc;

import java.util.ArrayList;
import java.lang.String;


public interface ProtocolInterface {

    public void processInput(String msg,Client client);
    public void  setShouldClose(boolean stat);
    public boolean getShouldClose();
    public void connectionTerminated();
    public ArrayList<String> split(String msg);
    public String buildString(ArrayList<String> words);
}
