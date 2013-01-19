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
    private boolean newUser;
    private boolean isAdmin;


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
        this.newUser = true;
        this.isAdmin = false;

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
    }

    // Setters
    public void setIsInChannel(boolean status) {
        this.inChannel = status;
    }
    public void addChannel(Channel channel) {
        this.channel = channel;
        this.inChannel = true;
        channel.addUser(this);
    } 
    public void setNickName(String nick) {
        this.nickName = nick;
        this.checkNewUser();
    }
    public void setUser(String user) {
        this.user = user;
        this.checkNewUser();
    }
    public void setAsAdmin() {
        this.isAdmin = true;
        this.nickName = "@"+this.nickName;
    }
    public void setAsNotAdmin() {
        this.isAdmin = false;
        this.nickName = this.nickName.substring(1, this.nickName.length());
    }


    // Getters
    public boolean isUserNameExist() {

        if (this.user.length() == 0) {
            return false;
        } else {
            return true;
        }
    }
    private void checkNewUser() {

        if (this.hasNickname() &&
                this.hasUser()) {
            this.newUser = false;
        }
    }

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
    public boolean canRegister() {
        if (this.nickName.length() != 0 &&
                this.user.length() != 0) {
        System.out.println("Size of the guys" + this.nickName.length() + 
                this.user.length());
            return true;
        } else {
            return true;
        }
    }
    public boolean hasNickname() {
        if (this.nickName.length() != 0) {
            return true;
        } else {
            return false;
        }
    }
    public boolean hasUser() {
        if (this.user.length() != 0) {
            return true;
        } else {
            return false;
        }
    }
    public boolean newUser() {
        return this.newUser;
    }
    public void removeFromChannel() {

        this.channel.removeUser(this);

        if (this.channel.isEmpty()) {
            // Channel is empty, need to delete it.
            this.oper.removeChannel(this.channel);
        }
            this.channel = null;
            this.inChannel = false;
    }
    public boolean isAdmin() {
        if (this.isAdmin == true) {
            return true;
        } else {
            return false;
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
