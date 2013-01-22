/** @author Eldar Damari, Ory Band. */

package irc;

import java.util.ArrayList;


public class Channel {
    private String name;
    private Client admin;
    private ArrayList<Client> users;

    public Channel(String name, Client admin) {
        this.name  = name;
        this.users = new ArrayList<Client>();
        this.admin = admin;
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
        for (Client client : this.users) {
            str.append(client.getNickName() + " ");
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


    public ArrayList<Client> getUsers() {
        return this.users;
    }


    public synchronized void addUser(Client client) {
        this.users.add(client);
    }

    public synchronized void removeUser(Client client) {
        if (this.users.size() == 2 && client.isAdmin()) { 
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


    public synchronized void sendAll(String nickname, String msg) {
        for (Client client : this.users) {
            client.sendMessage(nickname + ": " + msg);
        }
    }


    public synchronized void sendAllSystemMessage(String msg) {
        for (Client client : this.users) {
            client.sendMessage(msg);
        }
    }
}
