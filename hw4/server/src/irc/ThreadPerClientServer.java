package irc;

import java.net.Socket;
import java.net.ServerSocket;
import java.io.InputStreamReader;
import java.io.IOException;


public class ThreadPerClientServer implements Runnable {

    private ServerSocket serverSocket;
    private int port;
    private String charset;
    private Oper oper;


    public ThreadPerClientServer(int port, String charset) {
        this.serverSocket = null;
        this.port = port;
        this.charset = charset;
        this.oper = new Oper();
    }


    /**
     * Repeatedly listens for, and accepts connections,
     * and sets up a new client object for each one, each run in its own thread.
     */
    public void run() {
        try {  // Init socket.
            this.serverSocket = new ServerSocket(this.port);
        } catch (IOException e) {
            System.out.println(
                    "Error (Cannot listen on 0.0.0.0:" + this.port + ").\nExiting.");
        }

        EncoderInterface encoder = new IrcEncoder(this.charset);

        System.out.println(
                "Server started. Listening on 0.0.0.0:" + this.port + " ...");

        while (true) {
            // Wait for a client to connect.
            Socket socket;
            try {
                socket = this.serverSocket.accept();
            } catch (IOException e) {
                System.out.println("Failed to accept connection from client.");
                continue;
            }

            InputStreamReader stream;
            try {
                stream = new InputStreamReader(
                        socket.getInputStream(), encoder.getCharSet());
            } catch (IOException e) {
                System.out.println("Failed to init stream reader upon client connection.");
                continue;
            }

            // Set end-of-message char to be the newline '\n'.
            MessageTokenizer tokenizer = new MessageTokenizer(stream ,'\n');

            ProtocolInterface protocol = new IrcProtocol(this.oper);

            Client client = new Client(
                    tokenizer, encoder, protocol, socket, oper);

            this.oper.addClient(client);

            new Thread(client).start();
        }

        //this.serverSocket.close();  // TODO Is this OK?
    }
}
