//package irc;

import java.io.IOException;
import java.net.Socket;


public class Client implements Runnable {

    private Socket socket;
    private EncoderInterface encoder;
    private MessageTokenizer tokenizer;
    private ProtocolInterface protocol;
    private ThreadPerClientServer server;

    private String nickName;
    private String user;


    public Client(
            MessageTokenizer tokenizer,
            EncoderInterface encoder,
            ProtocolInterface protocol,
            Socket socket,
            ThreadPerClientServer server) {

        this.socket = socket;
        this.encoder = encoder;
        this.tokenizer = tokenizer;
        this.protocol = protocol;
        this.server = server;
        this.nickName = new String();
        this.user = new String();

        String NEW_LINE = System.getProperty("line.separator");
        String msg = "**Welcome To miniIRC server** ; Your Host"
            + socket.getInetAddress() + ":" + socket.getPort()+'\n';
        byte[] buf = this.encoder.toBytes(msg);
        try {
            this.socket.getOutputStream().write(buf,0,buf.length);
        } catch (IOException e) {
            e.printStackTrace();
        }
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
    public void setNickName(String nick) {
        this.nickName = nick;
    }

    public void setUser(String user) {
        this.user = user;
    }
}
