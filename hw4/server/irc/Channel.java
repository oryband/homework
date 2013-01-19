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
        this.admin.setAsAdmin();
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
    }

    public synchronized void removeUser(Client client) {

        if (this.users.size() == 2 &&
                client.isAdmin()) { 
            this.users.remove(this.users.indexOf(client));
            client.setAsNotAdmin();
            // Setting the only user left in the channel as admin 
            this.users.get(0).setAsAdmin();
            this.admin = this.users.get(0);
        } else {
            if (this.users.size() == 1) {
                client.setAsNotAdmin();
             } 
                this.users.remove(this.users.indexOf(client));
        }
    }

    // Sending a message to all users in this channgel
    public void sendAll(String nickname, String msg) {

        Iterator<Client> it = this.users.iterator();

        while (it.hasNext()) {
            it.next().sendMessage(nickname+": "+msg);
        }
    }
    public void sendAllSystemMessage(String msg) {
        
        Iterator<Client> it = this.users.iterator();

        while (it.hasNext()) {
            it.next().sendMessage(msg);
        }

    }
}
