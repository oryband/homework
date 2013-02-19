import java.io.*;
import java.net.*;

class LPServer {
	
	public static void main(String[] args) throws IOException
	{
		ServerSocket lpServerSocket = null;

		// Get port
		int port = Integer.decode(args[0]).intValue();
		
		// Listen on port
		try {
			lpServerSocket = new ServerSocket(port);
		} catch (IOException e) {
			System.out.println("Couldn't listen on port " + port);
			System.exit(1);
		}
		
		System.out.println("Listening...");
		
		// Waiting for a client connection
		Socket lpClientSocket = null;
		try {
			lpClientSocket = lpServerSocket.accept();
		} catch (IOException e) {
			System.out.println("Failed to accept...");
			System.exit(1);
		}
		
		System.out.println("Accepted connection from client!");
		System.out.println("The client is from: " + lpClientSocket.getInetAddress() + ":" + lpClientSocket.getPort());
		
		// Read messages from client
		BufferedReader in = new BufferedReader(new InputStreamReader(lpClientSocket.getInputStream()));
		String msg;

		while ((msg = in.readLine()) != null)
		{
			System.out.println("Received from client: " + msg);
			if (msg.equals("bye"))
			{
				System.out.println("Client sent a terminating message");
				break;
			}
		}
		
		System.out.println("Client disconnected - bye bye...");
		lpClientSocket.close();
		lpServerSocket.close();
		in.close();
		System.out.println("closed all sockets");
		
	}
}
