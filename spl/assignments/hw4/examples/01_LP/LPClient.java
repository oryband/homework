import java.io.*;
import java.net.*;

public class LPClient {
	
	public static void main(String[] args) throws IOException
	{
		Socket lpSocket = null; // the connection socket
		PrintWriter out = null;

		// Get host and port
		String host = args[0];
		int port = Integer.decode(args[1]).intValue();
		
		System.out.println("Connecting to " + host + ":" + port);
		
		// Trying to connect to a socket and initialize an output stream
		try {
			lpSocket = new Socket(host, port); // host and port
      		out = new PrintWriter(lpSocket.getOutputStream(), true);
    	} catch (UnknownHostException e) {
      		System.out.println("Unknown host: " + host);
      		System.exit(1);
	    } catch (IOException e) {
	    	System.out.println("Couldn't get I/O to " + host + " connection");
			System.exit(1);
    	}
    	
    	System.out.println("Connected to server!");

		String msg;
    	BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));
	try{    	
		while ((msg = userIn.readLine())!= null)
		{
			out.println(msg);
		}
    	 } catch (IOException e) { }
	
    	System.out.println("Exiting...");
    	
    	// Close all I/O
    	out.close();
    	userIn.close();
    	lpSocket.close();
	}
}
