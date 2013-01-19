//package irc;

import java.util.ArrayList;

import java.lang.String;
import java.util.Iterator;


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
                this.runQuit(client, words);
                break; 

                // JOIN
            case 4: 
                this.runJoin(client, words);
                break;

            case 5: 
                this.runPart(client, words);
                break;

            case 6: 
                this.runNames(client, words);
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

                // checking if there is #before and id channel exist
                String channelname = words.get(1);
                if (channelname.charAt(0) != '#') {
                    return;
                }
                channelname = channelname.substring(1, channelname.length());

                // Client already in a channel 
                if (client.isInChannel() == true) {

                    // Trying to get inside the same cahnnel!
                    if (client.getChannel().getName().
                            equals(channelname) == true) {

                    } else {

                        // Removing the client from the channel he is in!
                        client.removeFromChannel();

                        Channel channel = client.getOper()
                            .isChannelExist(channelname);

                        // Is the channel exist?
                        if (channel == null) {

                            // Creating new channel with client as admin
                            client.getOper().addChannel(channelname,client);
                            client.addChannel(client.getOper()
                                    .isChannelExist(channelname));

                            client.sendMessage(client.getChannel().getNameReply(true));

                        } else { // Channel exist!

                            client.addChannel(channel);
                            client.sendMessage(channel.getNameReply(true));
                        }
                    } 
                } else { // new user need new channel

                    Channel channel = client.getOper()
                        .isChannelExist(channelname);

                    // lets see if channel already exist
                    if (channel == null) {

                        // Creating new channel with client as admin
                        client.getOper().addChannel(channelname,client);
                        client.addChannel(client.getOper()
                                .isChannelExist(channelname));

                        client.sendMessage(client.getChannel().getNameReply(true));

                    } else { // Channel exist!

                        client.addChannel(channel);
                        client.sendMessage(channel.getNameReply(true));

                    }
                }
            }
        }
    }

    private void runQuit(Client client, ArrayList<String> words) {
        
        if (words.size() == 1) {

            // SYSTEM means it a system message and not from user
            client.getChannel().sendAllSystemMessage
                ("<"+client.getNickName()+"> has left the channel"); 

        } else { 
            
            // SYSTEM means it a system message and not from user
            client.getChannel().sendAllSystemMessage
                ("<"+client.getNickName()+"> " + words.get(1)); 
        }
            client.removeFromChannel(); 
            client.setProtocolShouldClose();

        }

    private void runPart(Client client, ArrayList<String> words) {

        if (words.size() == 1) {
            client.sendMessage("461 USER :Not enough parameters"); 
        } else { 
            if (words.size() == 2) {

                if (client.isInChannel()) {

                    if (client.getChannel().getName().equals(words.get(1))) {

                        client.removeFromChannel();
                        client.sendMessage("405"); 
                    } else {

                        client.sendMessage("403 <"+words.get(1)+"> :No such channel"); 
                    }
                }
            }
        }
    }

    private void runNames(Client client, ArrayList<String> words) {

        if (words.size() == 1) {

            StringBuilder names = new StringBuilder();

            Iterator<Channel> it = client.getOper().getChannels().iterator();

            // Building a string with all data
            while (it.hasNext()) {

                names.append(it.next().getNameReply(false));
            }
            
            names.append("366 :End of /NAMES list" + '\n');

            String finalnames = names.toString();
            client.sendMessage(finalnames); 

        } else {

            String channel = words.get(1);
            // checking if there is #before and id channel exist
            channel = channel.substring(1, channel.length());

            if (client.getOper().isChannelExist(channel) != null) {

                client.sendMessage(client.getOper().
                        isChannelExist(channel).getNameReply(true));
            } else {
                client.sendMessage("403 <"+words.get(1)+"> :No such channel"); 
            }
        }
    }
}
