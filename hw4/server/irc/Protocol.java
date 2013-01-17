//package irc;

import java.util.ArrayList;
import java.util.Scanner;
import java.util.Iterator;

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

    public void processInput(String msg, Client client) {

        // Check if argument is 
        ArrayList<String> words = split(msg);

        // COMMAND Message!
        if (this.oper.getCommands().containsKey(words.get(0))) {

            // Executing task upon message and command
            this.oper.getCommands().get(words.get(0)).run(client, words);
        }
        // DATA Message!
        else { 

            // Sending o all users in the channel the message
            String line = buildString(words);
            client.getChannel().sendAll(client.getNickName(), line); 
        }

        /*if (client.getNickName() == null) {

            if (words.get(0).equals("NICK") == true) {
                if (!this.oper.isNickNameExist(words.get(1))) {
                    client.setNickName(words.get(1));
                    haveNick = true;

                    client.sendMessage("Cool you have nick name");
                } else {
                    //sendNumericError("NICKEXSIT"); // TODO implemet
                }
            } else {
                //sendNumericError("NICK");
            }
        }*/

    }

    /**
     * Divides lines by a delimeter given as argument.
     *
     * @param lines lines to divide.
     * @param delimeter delimeter to split by.
     */
    public ArrayList<String> split(String msg) {

        ArrayList<String> outputLines = new ArrayList<String>();

        // We check if the user entered a single command with no parameters
        String message = msg.substring(0,msg.length()-1);
        System.out.println("This is msg"+ msg);
        System.out.println("Size of:"+ msg.length());


        System.out.println("This is message"+ message);
        System.out.println("Size of:"+ message.length());

        if (this.oper.getCommands().containsKey(message)) {
            
            outputLines.add(message);

            return outputLines;
        }

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
        System.out.println("here is each word" + str.toString());
        }

        System.out.println("here is the message" + str.toString());
        String words = str.toString();
        words = words.substring(0, words.length()-1);
        outputLines.add(words);

        s.close();

        return outputLines;
    }

    public String buildString(ArrayList<String> words) {

        Iterator it = words.iterator();
        StringBuilder str = new StringBuilder();

        while (it.hasNext()) {
            str.append(it.next());
            str.append(" ");
        }

        String line = str.toString();
        line = line.substring(0, line.length()-1);

        return line;
    }
}
