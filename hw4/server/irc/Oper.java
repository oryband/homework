// package irc


import java.util.ArrayList;
import java.util.Iterator;
import java.util.HashMap;

public class Oper {

    public ArrayList<Client> clients;
    private ArrayList<Channel> channels;
    private final HashMap<String, Command> commands;

        public Oper() {
            this.clients = new ArrayList<Client>();
            this.channels = new ArrayList<Channel>();

            // Initializing all commands data!
            this.commands = new HashMap<String, Command>();
            ArrayList<Integer> numbers = new ArrayList<Integer>();

            numbers.add(431);
            numbers.add(433);
            numbers.add(401);
            this.commands.put("NICK",new Command("NICK",numbers));
            numbers.clear();

            numbers.add(461);
            numbers.add(462);
            numbers.add(402);
            this.commands.put("USER", new Command("USER", numbers));
            numbers.clear();

            this.commands.put("QUIT", new Command("QUIT", numbers));

            numbers.add(461);
            numbers.add(353);
            numbers.add(366);
            this.commands.put("JOIN", new Command("JOIN", numbers));
            numbers.clear();

            numbers.add(461);
            numbers.add(403);
            numbers.add(405);
            this.commands.put("PART", new Command("PART" , numbers));
            numbers.clear();

            numbers.add(353);
            numbers.add(366);
            numbers.add(403);
            this.commands.put("NAMES", new Command("NAMES", numbers));
            numbers.clear();
            
            numbers.add(321);
            numbers.add(323);
            numbers.add(322);
            this.commands.put("LIST", new Command("LIST", numbers));
            numbers.clear();

            numbers.add(461);
            numbers.add(404);
            numbers.add(483);
            this.commands.put("KICK", new Command("KICK", numbers));
    }

    // Setters
    public void addClient(Client client) {
        this.clients.add(client);
    }
    public void addChannel(String name, Client admin) {
        this.channels.add(new Channel(name, admin));
    }
    public void removeChannel(Channel channel) {
        this.channels.remove(this.channels.indexOf(channel));
    }
    public void removeClient(Client client) {
        this.clients.remove(this.clients.indexOf(client));
    }

    // Getters
    public boolean isCommandExist(String command) {

        return this.commands.containsKey(command);
    }
    public HashMap<String, Command> getCommands() {
        return this.commands;
    }

    // Checks if channel exist!
    public Channel isChannelExist(String channelName) {

        Iterator<Channel> it = this.channels.iterator();

        while (it.hasNext()) {
            Channel channel = it.next();
            if (channel.getName().equals(channelName)) {
                return channel ;
            }
        }
        return null;
    }
    public ArrayList<Channel> getChannels() {
        return this.channels;

    }

    // Checks if nick name exist!
    public boolean isNickNameExist(String nick) {

        Iterator<Client> it = this.clients.iterator();

        while (it.hasNext()) {

            String nickname = it.next().getNickName();
            int namelength = nickname.length();

            if (nickname.equals(nick) == true ) {
                return true;
            } else {
                // Checking if client name with @  - indicates for admin
                if (namelength >= 2) {

                    if (nickname.substring(1, nickname.length()).
                            equals(nick)) {
                                    return true;
                                }
                }
            }
        }
        return false;
    }

    public void addToChannel(String channelName,Client client) {

        //Check if channel exist!
        Channel channel = this.isChannelExist(channelName);

        if (channel != null) {
            channel.addUser(client);
        } else {
            this.createChannel(channelName, client);
        }
    }

    // Creating a new channel with a new admin!
    public void createChannel(String channelName, Client admin) {

        Channel newCH = new Channel(channelName, admin);
        this.channels.add(newCH);
    }
}

