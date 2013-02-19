/**
 * Represents a client session.
 *
 * @author Eldar Damari, Ory Band.
 */

package irc;

import java.nio.channels.SocketChannel;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.io.IOException;


public class Client {

    // Static members and methods.

    /** Static singleton instance of all connected clients. */
    private static ArrayList<Client> clients  = new ArrayList<Client>();

    /**
     * @param client client to add to client list.
     */
    public static Client createClient() {
        Client client = new Client();
        clients.add(client);
        return client;
    }

    /**
     * @param client to delete from client list.
     */
    public static void removeClient(Client client) {
        clients.remove(clients.indexOf(client));
    }

    /**
     * @param nickname client nickname to search for.
     *
     * @return client matched by nickname, or null if not found.
     */
    public static Client getClient(String nickname) {
        for (Client client : clients) {
            if (client.getNickname().equals(nickname) ||
                    client.getNickname().equals("@" + nickname)) {

                return client;
            }
        }

        return null;
    }


    /**
     * @return string list representations of client nicknames.
     */
    public static String getClientsString() {
        return clients.toString();
    }


    /**
     * @param nick nickname to search for.
     *
     * @return true if client matching given nickname is found, false otherwise.
     */
    public static boolean isNicknameExist(String nick) {
        for (Client client : clients) {
            String nickname = client.getNickname();

            // Compare nickname or @nickname.
            if (nickname.length() > 0 &&
                    (nickname.equals(nick) ||
                     nickname.substring(1, nickname.length()).equals(nick))) {

                return true;
            }
        }

        return false;
    }


    // Non-static members and methods.
    public ConnectionHandler<String> connectionHandler;

    private String nickname;
    private String username;

    private Channel channel;
    private boolean inChannel;
    private boolean isChanop;

    private boolean isNewClient;


    public Client() {
        this.connectionHandler = null;

        this.nickname = new String();
        this.username = new String();

        this.channel   = null;
        this.inChannel = false;
        this.isChanop  = false;

        this.isNewClient = true;
    }


    /**
     * @param h Connection handler to set client onto.
     */
    public void setConnectionHandler(ConnectionHandler<String> h) {
        this.connectionHandler = h;
    }


    /**
     * @return client's connection handler.
     */
    public ConnectionHandler<String> getConnectionHandler() {
        return this.connectionHandler;
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
        this.username = username;
        checkNewUser();
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
        if (this.isChanop) {
            this.isChanop = false;
            this.nickname = this.nickname.substring(1, this.nickname.length());
        }
    }


    /**
     * @return client's nickname.
     */
    public String getNickname() {
        return this.nickname;
    }

    /**
     * @return client's username.
     */
    public String getUsername() {
        return this.username;
    }

    /**
     * @return client's channel, or null if client isn't in channel.
     */
    public Channel getChannel() {
        return this.channel;
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
                Channel.removeChannel(this.channel);
            }

            this.channel   = null;
            this.inChannel = false;
        }
    }
}
