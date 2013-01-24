/** @author Eldar Damari, Ory Band */

package irc;

import java.nio.channels.SocketChannel;
import java.nio.channels.ServerSocketChannel;
import java.nio.charset.Charset;
import java.net.InetSocketAddress;
import java.io.InputStreamReader;
import java.io.IOException;


public class ThreadPerClientServer implements Runnable {
    private ServerSocketChannel serverSocketChannel;
    private int port;
    private String charset;


    public ThreadPerClientServer(int port, String charset) {
        this.serverSocketChannel = null;
        this.port = port;
        this.charset = charset;
    }


    /**
     * Repeatedly listens for, and accepts connections,
     * and sets up a new client object for each one, each run in its own thread.
     */
    public void run() {
        try {  // Init socket.
            this.serverSocketChannel = ServerSocketChannel.open();
            this.serverSocketChannel.configureBlocking(true);
            this.serverSocketChannel.socket().bind(new InetSocketAddress(port));
        } catch (IOException e) {
            System.out.println(
                    "Error (Cannot listen on 0.0.0.0:" + this.port + ").\nExiting.");
        }

        System.out.println(
                "Server started. Listening on 0.0.0.0:" + this.port + " ...");

        while (true) {
            SocketChannel socketChannel = null;

            // Wait (block) for a client to connect.
            try {
                socketChannel = this.serverSocketChannel.accept();
                socketChannel.configureBlocking(true);
            } catch (IOException e) {
                System.out.println("Failed to accept connection from client.");
                continue;
            }

            FixedSeparatorMessageTokenizer tokenizer =
                new FixedSeparatorMessageTokenizer(
                        "\n", Charset.forName(this.charset));

            IrcProtocol protocol = new IrcProtocol();

            TpcConnectionHandler<String> connectionHandler =
                TpcConnectionHandler.create(socketChannel, protocol, tokenizer);

            protocol.setConnectionHandler(connectionHandler);

            new Thread(connectionHandler).start()  ;
        }

        //this.serverSocket.close();  // TODO Is this OK?
    }


    public static void main(String[] args) {
        ThreadPerClientServer server = new ThreadPerClientServer(6667, "UTF-8");
        Thread serverThread = new Thread(server);
        serverThread.start();

        try {
            serverThread.join();
        } catch (InterruptedException e) {
            System.out.println("Server stopped.\nExiting.");
        }
    }
}
