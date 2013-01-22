package irc;

import java.net.ServerSocket;
import java.io.InputStreamReader;
import java.io.IOException;


public class ThreadPerClientServer implements Runnable {

    private ServerSocket socket;
    private int port;
    private String charset;
    private Oper oper;


    public ThreadPerClientServer(int port, String charset) {
        this.socket = null;
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
            this.socket = new ServerSocket(this.port);
        } catch (IOException e) {
            System.out.println(
                    "Error (Cannot listen on 0.0.0.0:" + this.port + "). \nExiting.");
        }

        EncoderInterface encoder = new IrcEncoder(this.charset);

        System.out.println(
                "Server started. Listening on 0.0.0.0:" + this.port + " ...");

        while (true) {
            Socket socket = socket.accept();  // Wait for a client to connect.

            InputStreamReader stream = new InputStreamReader(
                    socket.getInputStream(), encoder.getCharSet());

            // Set end-of-message char to be the newline '\n'.
            MessageTokenizer tokenizer = new MessageTokenizer(stream ,'\n');

            ProtocolInterface protocol = new IrcProtocol(this.oper);

            Client client = new Client(
                    tokenizer, encoder, protocol, socket, oper);

            this.oper.addClient(client);

            new Thread(client).start();
        }

        socket.close();  // TODO move this to Client.java ?
    }


    public static void main(String[] args) {
        ThreadPerClientServer server = new ThreadPerClientServer(6667, "US-ASCII");
        Thread serverThread = new Thread(server);
        serverThread.start();

        try {
            serverThread.join();
        } catch (InterruptedException e) {
            System.out.println("Server stopped, exiting.");
        }
    }
}
