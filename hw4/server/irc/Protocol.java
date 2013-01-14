package irc;


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
        return msg.equals("bye");
    }

    public String processInput(String msg) {

        if (isEnd(msg)) {
            this.shouldClose = true;
        } else {
            System.out.println(msg);
            return msg;
        }
        return null;
    }
}
