//package irc;

import java.util.*;
import java.io.*;
import java.net.*;
import java.lang.System;

public class ThreadPerClientServer implements Runnable{

    private ServerSocket serverSocket;
    private int listenPort;

    public ThreadPerClientServer(
            int port) {
        serverSocket = null;
        listenPort = port;
            }

    public void run() {

        try {
            EncoderInterface encoder = new IrcEncoder("UTF-8");
            this.serverSocket = new ServerSocket(listenPort);
            System.out.println("Listening...");

            while (true)
            {
                try {

                    Socket socket = serverSocket.accept();
                    InputStreamReader isr = new InputStreamReader(
                            socket.getInputStream(),
                            encoder.getCharSet());
                    MessageTokenizer tokenizer = 
                        new MessageTokenizer(isr ,'\n');

                    ProtocolInterface protocol = new Protocol();
                    ConnectionHandler handler = new ConnectionHandler(
                            tokenizer,
                            encoder,
                            protocol,
                            socket);
                    new Thread(handler).start();
                }
                catch (IOException e)
                {
                    System.out.println("Failed to accept on port " + listenPort);
                }
            }

        } catch (IOException e) {
            System.out.println("Cannot listen on port " + listenPort);
        }
        try {
            this.close();
        } catch (IOException e) {
            System.out.println("can't close");
        }
    }

    // Closes the connection
    public void close() throws IOException
    {
        serverSocket.close();
    }
}	
