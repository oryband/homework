//package irc;

import java.util.*;
import java.io.IOException;

public class Server {

    public static void main(String[] args) throws IOException {

        // Get port
        int port = Integer.decode(args[0]).intValue();

        ThreadPerClientServer server = new ThreadPerClientServer(port);
        Thread serverThread = new Thread(server);
        serverThread.start();

        try {
            serverThread.join();
        }
        catch (InterruptedException e)
        {
            System.out.println("Server stopped");
        }
    }
}
