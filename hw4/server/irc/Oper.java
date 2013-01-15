

import java.util.ArrayList;
import java.util.Iterator;

public class Oper {

    private ArrayList<Client> clients;
    private ArrayList<Channel> channels;
    private final ArrayList<String> commands;

    public Oper() {
        this.clients = new ArrayList<Client>();
        this.channels = new ArrayList<Channel>();
        this.commands = new ArrayList<String>();
            this.commands.add("NICK");
            this.commands.add("USER");
            this.commands.add("QUIT");
            this.commands.add("JOIN");
            this.commands.add("PART");
            this.commands.add("NAMES");
            this.commands.add("LIST");
            this.commands.add("KICK");
    }

    // Setters
    public void addClient(Client client) {
        this.clients.add(client);
    }

    // Getters
    public boolean isCommandExist(String command) {

        Iterator<String> it = this.commands.iterator();
        boolean flag = false;
        while (it.hasNext()) {

            if (it.next().equals(command)) {
                flag = true;
            } else {
                flag = false;
            }
        }
        return flag;
    }

    public boolean isNickNameExist(String nick) {

        Iterator<Client> it = this.clients.iterator();

        while (it.hasNext()) {

            if (it.next().getNickName().equals(nick)) {
                return true;
            }
        }
        return false;
    }

}
