package irc;

import java.util.*;
import java.io.*;
import java.net.*;

public class ThreadPerClientServer implements Runnable{

    private ServerSocket serverSocket;
    private int listenPort;
    private ServerProtocolFactory factory;


    public void  ThreadPerClientServer(
            int port,
            ServerProtocolFactory p)
    {
        serverSocket = null;
        listenPort = port;
        factory = p;
    }

    public void run()
    {
        try {
            serverSocket = new ServerSocket(listenPort);
            System.out.println("Listening...");
        }
        catch (IOException e) {
            System.out.println("Cannot listen on port " + listenPort);
		}
		
		while (true)
		{
			try {
				ConnectionHandler newConnection = new ConnectionHandler(serverSocket.accept(), factory.create());
            new Thread(newConnection).start();
			}
			catch (IOException e)
			{
				System.out.println("Failed to accept on port " + listenPort);
			}
		}
	}
	
	// Closes the connection
	public void close() throws IOException
	{
		serverSocket.close();
	}
}	
