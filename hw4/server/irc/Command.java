//package irc;

import java.util.ArrayList;

import java.lang.String;


public class Command{

    private String name;
    private ArrayList<Integer> numerics;

    public Command(String name, ArrayList<Integer> numerics) {

        this.name = name;
        this.numerics = numerics;
    }

    public void run(Client client, ArrayList<String> words) {

        if (name.equals("NICK") == true) {

            if (words.size() == 1) {

                client.sendMessage("431 No nickname given"); 
            } else 
                if (words.size() == 2) {

                    if (client.getOper().isNickNameExist(words.get(1))) {

                        client.sendMessage("433 "+words.get(1)+
                                ":Nickname is already in use"); 
                    } else {

                        client.setNickName(words.get(1));
                        client.sendMessage("401");
                    }
                }

        } else {
            if (name.equals("USER") == true) {

                if (words.size() == 1) {

                    client.sendMessage("461 USER :Not enough parameters"); 
                } else 
                    if (words.size() == 2) {

                        if (!client.isUserNameExist()) {

                            client.setUser(words.get(1));
                            client.sendMessage("402");
                        } else {

                            client.sendMessage("462 :You may not reregister");
                        }
                    }



            } else {
                if (name.equals("QUIT") == true) {


                }
            }
        }

    }
}
