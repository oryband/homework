//package irc;

import java.util.ArrayList;
import java.util.Scanner;
import java.lang.String;


public class Protocol implements ProtocolInterface {

    private boolean shouldClose;
    private Oper oper;

    public Protocol(Oper oper) {
        this.oper = oper;
    }

    public boolean shouldClose() {
        return this.shouldClose;
    }

    public void connectionTerminated() {
        this.shouldClose = true;
    }

    // TODO need to repalce by QUIT command!!!
    public boolean isEnd(String msg) {
        String trimed = msg.substring(0,msg.length()-1);
        return trimed.equals("bye");
    }

    public void processInput(String msg, Client client) {

        // Check if argument is 
        boolean haveNick = false;
        ArrayList<String> words = splitWords(msg);

        if (!haveNick) {
            
            if (words[0].equals("NICK")) {
                if (!this.oper.isNickNameExist()) {
                client.setNickName(words[1]);
                haveNick = true;
                } else {
                    sendNumericError("NICKEXSIT"); // TODO implemet
                }
            } else {
                sendNumericError("NICK");
            }
        }
        
        


        }
    }
    
    /**
     * Divides lines by a delimeter given as argument.
     *
     * @param lines lines to divide.
     * @param delimeter delimeter to split by.
     */
    public ArrayList<String> split(String msg) {

        ArrayList<String> outputLines = new ArrayList<String>();

        Scanner s = new Scanner(msg);
        s.useDelimiter("\\s");

        boolean flag = true;
        StringBuilder str = new StringBuilder();
        while (s.hasNext()) {

            if (flag) {
                outputLines.add(s.next());
                flag = false;
            } else {
                str.append(s.next());
                str.append(" ");
            }
        }

        String words = str.toString();
        words = words.substring(0, words.length()-1);
        outputLines.add(words);

        s.close();

        return outputLines;
    }
}
