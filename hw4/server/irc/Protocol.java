//package irc;


import java.lang.String;


public class Protocol implements ProtocolInterface {

    private boolean shouldClose;


    public boolean shouldClose() {
        return this.shouldClose;
    }

    public void connectionTerminated() {
        this.shouldClose = true;
    }

    public boolean isEnd(String msg) {
        String trimed = msg.substring(0,msg.length()-1);
        System.out.println(trimed);
        System.out.println(trimed.length());
        return trimed.equals("bye");
    }

    public String processInput(String msg) {

        if (isEnd(msg)) {
            this.shouldClose = true;
            return null;
        } else {

            System.out.println(msg);
            return msg;
        }
    }
}
