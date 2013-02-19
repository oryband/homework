import java.io.*;
import java.net.*;

interface ServerProtocol {

	String processMessage(String msg);

	boolean isEnd(String msg);

}

class EchoProtocol implements ServerProtocol {

	public EchoProtocol() {	}

	public String processMessage(String msg)
	{
		return msg;
	}

	public boolean isEnd(String msg)
	{
		return msg.equals("bye");
	}
}

class ProtocolServer {

	private BufferedReader in;
	private PrintWriter out;
	ServerSocket echoServerSocket;
	Socket clientSocket;
	int listenPort;
	ServerProtocol protocol;

	public ProtocolServer(int port, ServerProtocol p)
	{
		in = null;
		out = null;
		echoServerSocket = null;
		clientSocket = null;
		listenPort = port;
		protocol = p;
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
			System.out.println("Received \"" + msg + "\" from client");

			String response = protocol.processMessage(msg);
			if (response != null)
			{
				try{
					out.println(response);
				}catch(Exception e){
					
				}
			}

			if (protocol.isEnd(msg))
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

		ProtocolServer server = new ProtocolServer(port, new EchoProtocol());

		// Listen on port
		try {
			server.initialize();
		} catch (IOException e) {
			System.out.println("Failed to initialize on port " + port);
			System.exit(1);
		}

		// Process messages from client
		try {
			server.process();
		} catch (IOException e) {
			System.out.println("Exception in processing");
			server.close();
			System.exit(1);
		}

		System.out.println("Client disconnected - bye bye...");

		server.close();
	}
}