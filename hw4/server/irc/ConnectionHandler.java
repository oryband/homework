//package irc;

import java.io.IOException;
import java.net.Socket;


public class ConnectionHandler implements Runnable {

    private Socket socket;
    private EncoderInterface encoder;
    private MessageTokenizer tokenizer;
    private ProtocolInterface protocol;


    public ConnectionHandler(
            MessageTokenizer tokenizer,
            EncoderInterface encoder,
            ProtocolInterface protocol,
            Socket socket) {

        this.socket = socket;
        this.encoder = encoder;
        this.tokenizer = tokenizer;
        this.protocol = protocol;

        System.out.println("Accepted connection from client!");
        System.out.println("The client is from: " + socket.getInetAddress() + ":" + socket.getPort());
            }

    public void run() {

        while (!socket.isClosed() && !protocol.shouldClose()) {

            if (!this.tokenizer.isAlive()) {
                this.protocol.connectionTerminated();
            } else {

                try {
                    String msg = this.tokenizer.nextToken();
                    String ans = this.protocol.processInput(msg); // Prints to screen

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

}
