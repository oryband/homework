/** @author Eldar Damari, Ory Band */

package irc;

import java.nio.channels.SocketChannel;
import java.nio.channels.ServerSocketChannel;
import java.nio.charset.Charset;
import java.net.InetSocketAddress;
import java.io.IOException;


public class ThreadPerClient {
    /**
     * Repeatedly listens for, and accepts connections,
     * and sets up a new client object for each one, each run in its own thread.
     */
    public static void main(String[] args) {
        int port = 6667;
        String charset = "UTF-8";
        ServerSocketChannel serverSocketChannel = null;

        try {  // Init socket.
            serverSocketChannel = ServerSocketChannel.open();
            serverSocketChannel.configureBlocking(true);
            serverSocketChannel.socket().bind(new InetSocketAddress(port));
        } catch (IOException e) {
            System.out.println(
                    "Error (Cannot listen on 0.0.0.0:" + port + ").\nExiting.");
        }

        System.out.println(
                "Thread-Per-Client server started. " +
                "Listening on 0.0.0.0:" + port + " ...");

        while (true) {
            SocketChannel socketChannel = null;

            // Wait (block) for a client to connect.
            try {
                socketChannel = serverSocketChannel.accept();
                socketChannel.configureBlocking(true);
            } catch (IOException e) {
                System.out.println("Failed to accept connection from client.");
                continue;
            }

            FixedSeparatorMessageTokenizer tokenizer =
                new FixedSeparatorMessageTokenizer(
                        "\n", Charset.forName(charset));

            IrcProtocol protocol = new IrcProtocol();

            TpcConnectionHandler<String> connectionHandler =
                TpcConnectionHandler.create(socketChannel, protocol, tokenizer);

            protocol.setConnectionHandler(connectionHandler);

            new Thread(connectionHandler).start()  ;
        }
    }
}
