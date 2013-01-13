package irc;

import java.util.*;

class Server {
	
    public static void main(String[] args) throws IOException {
	
        // Get port
		int port = Integer.decode(args[0]).intValue();
		
		ThreadPerClientServer server = new MultipleClientProtocolServer(port, new EchoProtocolFactory());
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
