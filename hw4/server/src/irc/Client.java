/**
 * Represents a client session.
 *
 * @author Eldar Damari, Ory Band.
 */

package irc;

import java.io.IOException;
import java.net.Socket;


public class Client implements Runnable {

    private Socket           socket;
    private EncoderInterface encoder;
    private IrcTokenizer     tokenizer;
    private IrcProtocol      protocol;
    private IrcOperations    ircOperations;

    private String nickname;
    private String username;

    private Channel channel;
    private boolean inChannel;
    private boolean isChanop;

    private boolean isNewClient;


    public Client(
            IrcTokenizer     tokenizer,
            EncoderInterface encoder,
            Socket           socket,
            IrcOperations    ircOperations) {


        this.socket        = socket;
        this.encoder       = encoder;
        this.tokenizer     = tokenizer;
        this.ircOperations = ircOperations;

        this.protocol = new IrcProtocol(this.ircOperations, this);

        this.nickname = new String();
        this.username = new String();

        this.channel   = null;
        this.inChannel = false;
        this.isChanop   = false;

        this.isNewClient = true;

        // Welcome message.
        this.sendMessage(
                "Connected to miniIRC server on "
                + socket.getInetAddress() + ":" + socket.getPort());
    }


    public void run() {
        while ( ! socket.isClosed() && ! protocol.shouldClose()) {
            if ( ! this.tokenizer.isAlive() ) {
                this.protocol.close();
            } else {
                try {
                    String msg   = this.tokenizer.nextToken();
                    String reply = this.protocol.processMessage(msg);
                    sendMessage(reply);
                } catch (IOException e) {
                    System.out.println(
                            "Error analyzing message from client '" +
                            this.username + "/" + this.nickname + "'.");

                    continue;
                }
            }
        }

        // Connection has closed.
        this.ircOperations.removeClient(this);

        System.out.println(
                "Client " + this.username + "/" + this.nickname +
                " has disconnected.");

        System.out.println(
                "Currently connected users: " + this.ircOperations.clients.toString());

        try {
            this.socket.close();
        } catch (IOException e) {
            System.out.println(
                    "Error closing socket for client " +
                    this.username + "/" + this.nickname + "'.");
        }
    }


    /**
     * @param channel channel to add client into.
     */
    public void addToChannel(Channel channel) {
        channel.addUser(this);
        this.channel = channel;
        this.inChannel = true;
    } 


    /**
     * @param nick nickname to set client with.
     */
    public void setNickname(String nickname) {
        this.nickname = nickname;
        checkNewUser();
    }

    /**
     * @param username username to set client with.
     */
    public void setUsername(String username) {
        checkNewUser();
        this.username = username;
    }

    /**
     * Sets client as chanop
     */
    public void setChanop() {
        this.isChanop = true;
        this.nickname = "@" + this.nickname;
    }


    /**
     * Remove chanop status from client.
     */
    public void removeChanop() {
        this.isChanop = false;
        this.nickname = this.nickname.substring(1, this.nickname.length());
    }


    /**
     * @return client's socket.
     */
    public Socket getSocket() {
        return this.socket;
    }

    /**
     * @return client's nickname.
     */
    public String getNickname() {
        return this.nickname;
    }

    /**
     * @return client's channel, or null if client isn't in channel.
     */
    public Channel getChannel() {
        return this.channel;
    }

    /**
     * @return client's associated IrcOperations object.
     */
    public IrcOperations getIrcOperations() {
        return this.ircOperations;
    }


    /**
     * @return whether client has username already set.
     */
    public boolean isUsernameExist() {
        return this.username.length() != 0;
    }

    /**
     * @return whether client has joined a channel.
     */
    public boolean isInChannel() {
        return this.inChannel;
    }

    /**
     * @return whether client is chanop in the channel he is in.
     */
    public boolean isChanop() {
        return this.isChanop;
    }

    /**
     * @return whether client has nickname already set.
     */
    public boolean hasNickname() {
        return this.nickname.length() != 0;
    }

    /**
     * @return whether client has username already set.
     */
    public boolean hasUsername() {
        return this.username.length() != 0;
    }

    /**
     * @return whether client is a new client.
     */
    public boolean isNewClient() {
        return this.isNewClient;
    }


    /** Sets user as 'not new' if it has already set up a NICK and USER. */
    private void checkNewUser() {
        if (this.hasNickname() && this.hasUsername()) {
            this.isNewClient = false;
        }
    }


    /**
     * @return whether client has set nick and user, and can be registered.
     */
    public boolean canRegister() {
        return this.nickname.length() != 0 && this.username.length() != 0;
    }


    /** removes client from his channel. */
    public void removeFromChannel() {
        if (this.channel != null) {
            this.channel.removeUser(this);

            // Delete channel if empty.
            if (this.channel.isEmpty()) {
                this.ircOperations.removeChannel(this.channel);
            }

            this.channel   = null;
            this.inChannel = false;
        }
    }


    /**
     * @param msg message to send to client.
     */
    public void sendMessage(String msg) {
        String NEW_LINE = System.getProperty("line.separator");
        String newmsg = msg + NEW_LINE; 
        byte[] buf = this.encoder.toBytes(newmsg);

        try {
            this.socket.getOutputStream().write(buf, 0, buf.length);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
