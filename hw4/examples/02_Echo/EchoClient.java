import java.io.*;
import java.net.*;

public class EchoClient {
	
	public static void main(String[] args) throws IOException
	{
		Socket clientSocket = null; // the connection socket
		PrintWriter out = null;
		BufferedReader in = null;

		// Get host and port
		String host = args[0];
		int port = Integer.decode(args[1]).intValue();
		
		System.out.println("Connecting to " + host + ":" + port);
		
		// Trying to connect to a socket and initialize an output stream
		try {
			clientSocket = new Socket(host, port); // host and port
      		out = new PrintWriter(clientSocket.getOutputStream(), true);
    	} catch (UnknownHostException e) {
      		System.out.println("Unknown host: " + host);
      		System.exit(1);
	    } catch (IOException e) {
	    	System.out.println("Couldn't get output to " + host + " connection");
			System.exit(1);
    	}
    	
    	// Initialize an input stream
    	try {
    		in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream(),"UTF-8"));
    	} catch (IOException e) {
	    	System.out.println("Couldn't get input to " + host + " connection");
			System.exit(1);
    	}
    	
    	System.out.println("Connected to server!");

		String msg;
    	BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in,"UTF-8"));
    	
		while ((msg = userIn.readLine())!= null)
		{
			out.println(msg);
			System.out.println(in.readLine());
			if(msg.equals("bye"))
				break;
		}
    	
    	System.out.println("Exiting...");
    	
    	// Close all I/O
    	out.close();
    	in.close();
    	userIn.close();
    	clientSocket.close();
	}
}
