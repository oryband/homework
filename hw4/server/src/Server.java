import irc.ThreadPerClientServer;


public class Server {
    public static void main(String[] args) {
        ThreadPerClientServer server = new ThreadPerClientServer(6667, "UTF-8");
        Thread serverThread = new Thread(server);
        serverThread.start();

        try {
            serverThread.join();
        } catch (InterruptedException e) {
            System.out.println("Server stopped.\nExiting.");
        }
    }
}
