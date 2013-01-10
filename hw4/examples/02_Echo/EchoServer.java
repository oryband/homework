import java.io.*;
import java.net.*;

class EchoServer {
	
	private BufferedReader in;
	private PrintWriter out;
	ServerSocket echoServerSocket;
	Socket clientSocket;
	int listenPort;
	
	public EchoServer(int port)
	{
		in = null;
		out = null;
		echoServerSocket = null;
		clientSocket = null;
		listenPort = port;
	}
	
	// Starts listening
	public void initialize() throws IOException
	{
		// Listen
		echoServerSocket = new ServerSocket(listenPort);
		
		System.out.println("Listening...");
		
		// Accept connection
		clientSocket = echoServerSocket.accept();
		
		System.out.println("Accepted connection from client!");
		System.out.println("The client is from: " + clientSocket.getInetAddress() + ":" + clientSocket.getPort());
		
		// Initialize I/O
		in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream(),"UTF-8"));
		out = new PrintWriter(new OutputStreamWriter(clientSocket.getOutputStream(),"UTF-8"), true);
		
		System.out.println("I/O initialized");
	}
	
	public void process() throws IOException
	{
		String msg;
		
		while ((msg = in.readLine()) != null)
		{
			System.out.println("Got " + msg);
			/*out.print(msg +"\n");
			out.flush();*/
			out.println(msg);
			if (msg.equals("bye"))
			{
				break;
			}
		}
	}
	
	// Closes the connection
	public void close() throws IOException
	{
		in.close();
		out.close();
		clientSocket.close();
		echoServerSocket.close();
	}
	
	public static void main(String[] args) throws IOException
	{
		// Get port
		int port = Integer.decode(args[0]).intValue();

		EchoServer echoServer = new EchoServer(port);
		
		// Listen on port
		try {
			echoServer.initialize();
		} catch (IOException e) {
			System.out.println("Failed to initialize on port " + port);
			System.exit(1);
		}
		
		// Process messages from client
		try {
			echoServer.process();
		} catch (IOException e) {
			System.out.println("Exception in processing");
			echoServer.close();
			System.exit(1);
		}
		
		System.out.println("Client disconnected - bye bye...");
		
		echoServer.close();
	}
}
