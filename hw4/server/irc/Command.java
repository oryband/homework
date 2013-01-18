//package irc;

import java.util.ArrayList;

import java.lang.String;


public class Command{

    private String name;
    private int id;
    private ArrayList<Integer> numerics;

    public Command(String name, ArrayList<Integer> numerics) {

        this.name = name;
        this.numerics = numerics;

        if (this.name.equals("NICK")) { id = 1 ;}
        if (this.name.equals("USER")) { id = 2 ;}
        if (this.name.equals("QUIT")) { id = 3 ;}
        if (this.name.equals("JOIN")) { id = 4 ;}
        if (this.name.equals("PART")) { id = 5 ;}
        if (this.name.equals("NAMES")) { id = 6 ;}
        if (this.name.equals("LIST")) { id = 7 ;}
        if (this.name.equals("KICK")) { id = 7 ;}
    }

    public void run(Client client, ArrayList<String> words) {

        switch(this.id) {

            // NICK
            case 1: 
                this.runNick(client,words);
                break;

                // USER
            case 2:
                this.runUser(client,words);
                break;

                // QUIT 
            case 3:
                break; //TODO

                // JOIN
            case 4: 
                this.runJoin(client, words);
                break;
        }


    }

    private void runNick(Client client, ArrayList<String> words) {

        if (words.size() == 1) {
            client.sendMessage("431 No nickname given"); 
        } else { 
            if (words.size() == 2) {

                if (client.getOper().isNickNameExist(words.get(1))) {
                    client.sendMessage("433 <"+words.get(1)+
                            "> :Nickname is already in use"); 
                } else {
                    client.setNickName(words.get(1));
                    client.sendMessage("401");
                }
            }
        }
    }

    private void runUser(Client client, ArrayList<String> words) {
        if (words.size() == 1) {

            client.sendMessage("461 USER :Not enough parameters"); 
        } else { 
            if (words.size() == 2) {

                if (!client.isUserNameExist()) {

                    client.setUser(words.get(1));
                    client.sendMessage("402");
                } else {

                    client.sendMessage("462 :You may not reregister");
                }
            }
        }
    }

    private void runJoin(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            client.sendMessage("461 USER :Not enough parameters"); 
        } else { 

            if (words.size() == 2) {

                // Client already in a channel 
                if (client.isInChannel() == true) {

                    // Trying to get inside the same cahnnel!
                    if (client.getChannel().getName().
                            equals(words.get(1)) == true) {

                    } else {

                        // Removing the client from the channel he is in!
                        client.removeFromChannel();

                        Channel channel = client.getOper()
                            .isChannelExist(words.get(1));

                        // Is the channel exist?
                        if (channel == null) {

                            // Creating new channel with client as admin
                            client.getOper().addChannel(words.get(1),client);
                            client.addChannel(client.getOper()
                                    .isChannelExist(words.get(1)));

                            client.sendMessage(client.getChannel().getNameReply());

                        } else { // Channel exist!

                            client.addChannel(channel);
                            client.sendMessage(channel.getNameReply());
                        }
                    } 
                } else { // new user need new channel

                    Channel channel = client.getOper()
                        .isChannelExist(words.get(1));

                    // lets see if channel already exist
                    if (channel == null) {

                        // Creating new channel with client as admin
                        client.getOper().addChannel(words.get(1),client);
                        client.addChannel(client.getOper()
                                .isChannelExist(words.get(1)));

                        client.sendMessage(client.getChannel().getNameReply());

                    } else { // Channel exist!

                        client.addChannel(channel);
                        client.sendMessage(channel.getNameReply());

                    }
                }
            }
            System.out.println("All channels: " + client.getOper().getChannels().toString());
        }
    }
}
