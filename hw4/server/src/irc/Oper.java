/**
 * Singleton that handles all channel/client operations.
 *
 * @author Eldar Damari, Ory Band.
 * */

package irc;

import java.util.ArrayList;


public enum IrcOperations {
    INSTANCE;  // Singleton pattern.
    
    private ArrayList<Client>  clients  = new ArrayList<Client>();
    private ArrayList<Channel> channels = new ArrayList<Channel>();


    /**
     * @param client client to add.
     */
    public void addClient(Client client) {
        this.clients.add(client);
    }

    /**
     * @param client to delete from server.
     */
    public void removeClient(Client client) {
        this.clients.remove(this.clients.indexOf(client));
    }

    /**
     * @param name channel name to create.
     * @param client client make chanop.
     */
    public void addChannel(String name, Client admin) {
        this.channels.add(new Channel(name, admin));
    }

    /**
     * @param channel channel to remove.
     */
    public void removeChannel(Channel channel) {
        this.channels.remove(this.channels.indexOf(channel));
    }

    /**
     * @param channelName channel to check if exists.
     */
    public Channel getChannel(String channelName) {
        for (Channel channel : this.channels) {
            if (channel.getName().equals(channelName)) {
                return channel;
            }
        }

        return null;
    }

    /**
     * @return channel list.
     */
    public ArrayList<Channel> getChannels() {
        return this.channels;

    }


    /**
     * @return LIST reply - channel list.
     */
    public String getListReply() {
        StringBuilder channelList = new StringBuilder();

        channelList.append(IrcProtocol.STATUS.LISTSTART+ " \n");

        for (Channel channel : this.channels) {
            channelList.append(
                    IrcProtocol.STATUS.LIST + " #" + channel.getName() + '\n');
        }

        channelList.append(
                IrcProtocol.STATUS.LISTEND.getNumber() + " " +
                IrcProtocol.STATUS.LISTEND.getText() + '\n');

        return channelList.toString();
    }


    /**
     * @param nickname client nickname to search for.
     *
     * @return client matched by nickname, or null if not found.
     */
    public Client getClient(String nickname) {
        for (Client client : this.clients) {
            if (client.getNickname().equals(nickname) ||
                    client.getNickname().equals("@" + nickname)) {

                return client;
            }
        }

        return null;
    }


    /**
     * @param nick nickname to search for.
     *
     * @return true if client matching given nickname is found, false otherwise.
     */
    public boolean isNicknameExist(String nick) {
        for (Client client : this.clients) {
            String nickname = client.getNickname();

            // Compare nickname or @nickname.
            if (nickname.equals(nick) ||
                    nickname.substring(1, nickname.length()).equals(nick)) {

                return true;
                    }
        }

        return false;
    }


    /**
     * @param channelName channel to add client into.
     * @param client client to add to given channel.
     */
    public void addToChannel(String channelName, Client client) {
        Channel channel = getChannel(channelName);

        if (channel != null) {
            channel.addUser(client);
            // Create channel if non-existent, and set client as chanop.
        } else {
            this.channels.add(new Channel(channelName, client));
        }
    }
}
