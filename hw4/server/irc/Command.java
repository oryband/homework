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
                break;

                // USER
            case 2:

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
                break;

                // QUIT 
            case 3:
                break; //TODO

                // JOIN
            case 4: 

                if (words.size() == 1) {

                    client.sendMessage("461 USER :Not enough parameters"); 
                } else { 

                    if (words.size() == 2) {

                        if (client.getOper().
                                isChannelExist(words.get(1)) == null) {

                            if (client.isInChannel() == true) {

                                client.removeFromChannel();
                            }
                            // Creating new channel with client as admin
                            client.getOper().addChannel(words.get(1),client);
                                    
                            client.addChannel(
                                    client.getOper()
                                    .isChannelExist(words.get(1)));

                            client.sendMessage("366 "+words.get(1)+
                                    " :End of /NAMES list");
                        } else {

                            if (client.isInChannel() == false) {

                                // JOIN him to an exist channel   
                                Channel channel = client.getOper()
                                    .isChannelExist(words.get(1));

                                channel.addUser(client);

                                client.sendMessage(channel.getNameReply());

                            } else {

                                if (client.getChannel().getName().
                                        equals(words.get(1)) == false) {
                                System.out.println("YES THE iiiiiROOM NAME ARE EVEM!");

                                    // Removing client from current channel
                                    client.removeFromChannel();

                                    client.getOper().isChannelExist(words.get(1)).
                                        addUser(client);
                                        }
                                System.out.println("YES THE ROOM NAME ARE EVEM!");
                            }
                        }
                    }
                }
                break;
        }
    }
}
