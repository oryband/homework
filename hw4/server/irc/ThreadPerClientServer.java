//package irc;

import java.util.ArrayList;
import java.util.Iterator;
import java.io.*;
import java.net.*;
import java.lang.System;

public class ThreadPerClientServer implements Runnable{

    private ServerSocket serverSocket;
    private int listenPort;
    private Oper oper;

    public ThreadPerClientServer(
            int port) {
        this.serverSocket = null;
        this.listenPort = port;
        this.oper = new Oper();
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

                    ProtocolInterface protocol = new Protocol(this);
                    Client client = new Client(
                            tokenizer,
                            encoder,
                            protocol,
                            socket,
                            this);
                    // Add client to list
                    this.oper.addClient(client);
                    // Run thread...Run!!
                    new Thread(client).start();
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

    /*public boolean isNickNameExist(String nick) {

        Iterator<Client> it = this.clients.iterator();

        while (it.hasNext()) {

            if (it.next().getNick().equals(nick)) {
                return true;
            } else {
                return false;
            }
        }
    }*/
}	
