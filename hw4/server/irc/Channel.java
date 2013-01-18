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
        this.users = new ArrayList<Client>();
        this.users.add(admin);
        this.admin.setIsInChannel(true);
    }

    // Getters
    public String getName() {
        return this.name;
    }

    public Client getAdmin() {
        return this.admin;
    }

    public boolean isEmpty() {

        if (this.users.size() == 0) {
            
            System.out.println(this.users.size());
            
            return true;
        } else {
            return false;
        }
    }

    public String getNameReply() {

        Iterator<Client> it = this.users.iterator();

        StringBuilder str = new StringBuilder();
        String N = System.getProperty("line.separator");

        str.append("<" + this.getName() + ">");
        while (it.hasNext()) {

            str.append("<" + it.next().getNickName() + ">");
        }
        // Add new line
        str.append(N);
        str.append("<" + this.getName() + "> :End of /NAMES list");
        return str.toString();
    }

    public ArrayList<Client> getUsers() {
        return this.users;
    }

    public synchronized void addUser(Client client) {
        this.users.add(client);
        client.setIsInChannel(true);
        client.addChannel(this);
    }

    public void removeUser(Client client) {
        this.users.remove(this.users.indexOf(client));
    }

    // Sending a message to all users in this channgel
    public void sendAll(String nickname, String msg) {

        Iterator<Client> it = this.users.iterator();

        while (it.hasNext()) {
            it.next().sendMessage(nickname+": "+msg);
        }
    }
}
