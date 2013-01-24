/** @author Eldar Damari, Ory Band. */

package irc;

import java.util.ArrayList;


public class Channel {
    // Static members and methods.

    private static ArrayList<Channel> channels = new ArrayList<Channel>();


    /**
     * @param name channel name to create.
     * @param client client make chanop.
     */
    public static void createChannel(String name, Client chanop) {
        channels.add(new Channel(name, chanop));
    }

    /**
     * @param channel channel to remove.
     */
    public static void removeChannel(Channel channel) {
        channels.remove(channels.indexOf(channel));
    }

    /**
     * @param channelName channel to check if exists.
     */
    public static Channel getChannel(String channelName) {
        for (Channel channel : channels) {
            if (channel.getName().equals(channelName)) {
                return channel;
            }
        }

        return null;
    }

    /**
     * @return channel list.
     */
    public static ArrayList<Channel> getChannels() {
        return channels;

    }


    /**
     * @return LIST reply - channel list.
     */
    public static String getListReply() {
        StringBuilder channelList = new StringBuilder();

        channelList.append(IrcProtocol.STATUS.LISTSTART+ " \n");

        for (Channel channel : channels) {
            channelList.append(
                    IrcProtocol.STATUS.LIST + " #" + channel.getName() + '\n');
        }

        channelList.append(
                IrcProtocol.STATUS.LISTEND.getNumber() + " " +
                IrcProtocol.STATUS.LISTEND.getText() + '\n');

        return channelList.toString();
    }


    /**
     * @param channelName channel to add client into.
     * @param client client to add to given channel.
     */
    public static void addToChannel(String channelName, Client client) {
        Channel channel = getChannel(channelName);

        if (channel != null) {
            channel.addUser(client);
            // Create channel if non-existent, and set client as chanop.
        } else {
            channels.add(new Channel(channelName, client));
        }
    }


    // Non-static members and methods.

    private String name;
    private Client chanop;
    private ArrayList<Client> clients;


    public Channel(String name, Client chanop) {
        this.clients = new ArrayList<Client>();
        this.name    = name;
        this.chanop  = chanop;
        this.chanop.setChanop();
    }


    // Getters
    public String getName() {
        return this.name;
    }

    public Client getchanop() {
        return this.chanop;
    }

    public boolean isEmpty() {
        if (this.clients.size() == 0) {
            System.out.println(this.clients.size());
            return true;
        } else {
            return false;
        }
    }

    /**
     * @param finalLine states whether to add end-of-list message at the end.
     *
     * @return name reply string, ready to be sent to users in a certain channel.
     */
    public String getNameReply(boolean finalLine) {
        StringBuilder str = new StringBuilder();
        String N = System.getProperty("line.separator");

        // Append channel.
        str.append(
                IrcProtocol.STATUS.NAMEREPLY.getNumber() +
                " #" + this.getName() + " ");
        
        // Append users in channel.
        for (Client client : this.clients) {
            str.append(client.getNickname() + " ");
        }

        // Append new-line.
        str.append(N);

        // Add "end-of-list" message at the end.
        if (finalLine) {
            str.append(
                    IrcProtocol.STATUS.ENDOFNAMES.getNumber() +
                    " #" + this.getName() + " :End of /NAMES list");
        }

        return str.toString();
    }


    public ArrayList<Client> getClients() {
        return this.clients;
    }


    public synchronized void addUser(Client client) {
        this.clients.add(client);
    }


    public synchronized void removeUser(Client client) {
        if (this.clients.size() == 2 && client.isChanop()) { 
            this.clients.remove(this.clients.indexOf(client));
            client.removeChanop();

            // Setting the only user left in the channel as chanop 
            this.clients.get(0).setChanop();
            this.chanop = this.clients.get(0);
        } else {
            if (this.clients.size() == 1) {
                client.removeChanop();
             } 

            this.clients.remove(this.clients.indexOf(client));
        }
    }
}
