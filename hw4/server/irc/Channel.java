//package irc;


import java.util.ArrayList;
import java.util.Iterator;


public class Channel {

    private String name;
    private Client admin;
    private ArrayList<Client> users;

    public Channel(String name, Client admin) {

        this.name = name;
        this.admin = admin;
    }

    // Getters
    public String getName() {
        return this.name;
    }

    public Client getAdmin() {
        return this.admin;
    }

    public ArrayList<Client> getUsers() {
        return this.users;
    }

    public void addUser(Client client) {
        this.users.add(client);
    }

    // Sending a message to all users in this channgel
    public void sendAll(String nickname, String msg) {

        Iterator<Client> it = this.users.iterator();

        while (it.hasNext()) {
            it.next().sendMessage(nickname+": "+msg);
        }
    }
}
