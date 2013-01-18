//package irc;

import java.io.IOException;
import java.net.Socket;


public class Client implements Runnable {

    private Socket socket;
    private EncoderInterface encoder;
    private MessageTokenizer tokenizer;
    private ProtocolInterface protocol;
    private Oper oper;

    private String nickName;
    private String user;
    private Channel channel;
    private boolean inChannel;


    public Client(
            MessageTokenizer tokenizer,
            EncoderInterface encoder,
            ProtocolInterface protocol,
            Socket socket,
            Oper oper ){

        this.socket = socket;
        this.encoder = encoder;
        this.tokenizer = tokenizer;
        this.protocol = protocol;
        this.oper = oper;
        this.nickName = new String();
        this.user = new String();
        this.channel = null;
        this.inChannel = false;

        String msg = "**Welcome To miniIRC server** ; Your Host"
            + socket.getInetAddress() + ":" + socket.getPort();

        this.sendMessage(msg);
            }

    public void run() {

        while (!socket.isClosed() && !protocol.shouldClose()) {

            if (!this.tokenizer.isAlive()) {
                this.protocol.connectionTerminated();
            } else {

                try {
                    String msg = this.tokenizer.nextToken();
                    // Analazing message upon protocol!
                    this.protocol.processInput(msg,this); 

                    System.out.println("11111111111");

                } catch (IOException e) {
                    System.out.println("ERROR: can't Analize message");
                }
            }
        }
        try {
            System.out.println("Connection lost dude ... bye bye!");
            this.socket.close();
        } 
        catch (IOException e) {
            System.out.println("Error in closing");
        }
                    System.out.println("2222222");
    }
    
    // Setters

    public void setIsInChannel(boolean status) {
        this.inChannel = status;
    }
    public void addChannel(Channel channel) {
        this.channel = channel;
    } 

    // Getters
    public Socket getSocket() {
        return this.socket;
    }
    public boolean isInChannel() {

        if (this.inChannel) {
            return true;
        } else {
            return false;
        }
    }
    public String getNickName() {
        return this.nickName;
    }
    public Channel getChannel() {
        return this.channel;
    }
    public Oper getOper() {
        return this.oper;
    }
    // Setters
    public void setNickName(String nick) {
        this.nickName = nick;
    }

    public void setUser(String user) {
        this.user = user;
    }
    public boolean isUserNameExist() {

        if (this.user.length() == 0) {
            return false;
        } else {
            return true;
        }
    }

    public void removeFromChannel() {

        this.channel.removeUser(this);

        if (this.channel.isEmpty()) {
            // Channel is empty, need to delete it.
            this.oper.removeChannel(this.channel);
        } else {
            this.inChannel = false;
            this.channel = null;
        }
    }


    public void sendMessage(String msg) {

        String NEW_LINE = System.getProperty("line.separator");
        String newmsg = msg + NEW_LINE; 
        byte[] buf = this.encoder.toBytes(newmsg);
        try {
            this.socket.getOutputStream().write(buf,0,buf.length);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
