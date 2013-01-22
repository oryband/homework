/** @author Eldar Damari, Ory Band. */

package irc;

import java.util.ArrayList;


public class Command {
    private String name;
    private COMMAND command;
    private ArrayList<Integer> numerics;

    public enum COMMAND {
        // Error codes.
        NICK  ( "NICK"  ),
        USER  ( "USER"  ),
        QUIT  ( "QUIT"  ),
        JOIN  ( "JOIN"  ),
        PART  ( "PART"  ),
        NAMES ( "NAMES" ),
        LIST  ( "LIST"  ),
        KICK  ( "KICK"  );

        private final String _text;

        COMMAND(String text) {
            _text = text;
        }

        public String getText() {
            return this._text;
        }
    };


    public Command(String name, ArrayList<Integer> numerics) {
        this.name = name;
        this.numerics = numerics;

        // Set command type.
        for (COMMAND command : COMMAND.values()) {
            if (this.name.equals(command.getText())) {
                this.command = command;
                return;
            }
        }
    }


    /**
     * Runs command on client.
     */
    public void run(Client client, ArrayList<String> words) {
        switch(this.command) {
            case NICK:
                this.runNick(client,words);
                break;
            case USER:
                this.runUser(client,words);
                break;
            case QUIT:
                this.runQuit(client, words);
                break;
            case JOIN:
                this.runJoin(client, words);
                break;
            case PART:
                this.runPart(client, words);
                break;
            case NAMES:
                this.runNames(client, words);
                break;
            case LIST:
                this.runList(client, words);
                break;
            case KICK:
                this.runKick(client, words);
                break;
        }
    }


    private void runNick(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            IrcProtocol.reply(IrcProtocol.STATUS.NONICKNAMEGIVEN, client);
        } else {
            if (words.size() == 2) {
                if (client.getOper().isNickNameExist(words.get(1))) {
                    IrcProtocol.replyBrackets(
                            IrcProtocol.STATUS.NICKNAMEINUSE,
                            client,
                            words.get(1));
                } else {
                    client.setNickName(words.get(1));
                    client.sendMessage(
                            IrcProtocol.STATUS.NICKACCEPTED.getText());
                }
            }
        }
    }


    private void runUser(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            IrcProtocol.replyNotEnoughParams(client, COMMAND.USER.getText());
        } else {
            if (words.size() == 2) {
                if ( !client.isUserNameExist() ) {
                    client.setUser(words.get(1));
                    client.sendMessage(
                            IrcProtocol.STATUS.USERACCEPTED.getText());
                } else {
                    IrcProtocol.reply(
                            IrcProtocol.STATUS.ALREADYREGISTERED, client);
                }
            }
        }
    }


    private void runJoin(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            IrcProtocol.replyNotEnoughParams(client, COMMAND.JOIN.getText());
        } else {
            if (words.size() == 2) {
                // Check for '#' before channel name.
                String channelName = words.get(1);
                if (channelName.charAt(0) != '#') {
                    return;
                }

                channelName = channelName.substring(1, channelName.length());

                // If client is trying to join the same channel he already is in,
                // remove him from that channel.
                if (client.isInChannel() &&
                        ! client.getChannel().getName().equals(channelName)) {

                    client.removeFromChannel();
                }

                Channel channel = client.getOper().isChannelExist(channelName);

                // If this is a new (non-existent) channel,
                // create channel and set client as chanop.
                if (channel == null) {
                    client.getOper().addChannel(channelName, client);
                    client.addChannel(
                            client.getOper()
                            .isChannelExist(channelName));

                    client.sendMessage(client.getChannel().getNameReply(true));
                // Add client to channel if it already exists.
                } else {
                    client.addChannel(channel);
                    client.sendMessage(channel.getNameReply(true));
                }
            }
        }
    }


    private void runQuit(Client client, ArrayList<String> words) {
        // Send standard QUIT message.
        if (words.size() == 1) {
            if (client.isInChannel()) {
                client.getChannel().sendAllSystemMessage(
                        "<" + client.getNickName() + "> has left the channel");
            }
        // Send custom QUIT message ("QUIT Goodbye world!").
        } else {
            client.getChannel().sendAllSystemMessage(
                    "<" + client.getNickName() + "> " + words.get(1)); 
        }

        client.removeFromChannel();
        client.setProtocolShouldClose();
    }


    private void runPart(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            IrcProtocol.replyNotEnoughParams(client, COMMAND.PART.getText());
        } else { 
            if (words.size() == 2) {
                if (client.isInChannel()) {
                    if (client.getChannel().getName().equals(words.get(1))) {
                        client.removeFromChannel();
                        IrcProtocol.numericReply(
                                IrcProtocol.STATUS.PARTSUCCESS,
                                client);
                    } else {
                        IrcProtocol.replyBrackets(
                                IrcProtocol.STATUS.NOSUCHCHANNEL,
                                client,
                                words.get(1));
                    }
                }
            }
        }
    }

    private void runNames(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            StringBuilder names = new StringBuilder();

            // Build string with all data.
            for (Channel channel : client.getOper().getChannels()) {
                names.append(channel.getNameReply(false));
            }
            
            names.append(
                    IrcProtocol.STATUS.ENDOFNAMES.getNumber() + " :" +
                    IrcProtocol.STATUS.ENDOFNAMES.getText() + '\n');

            String finalnames = names.toString();

            client.sendMessage(finalnames); 

        } else {
            String channel = words.get(1);

            // Check if there is # before and id channel exist.
            channel = channel.substring(1, channel.length());

            if (client.getOper().isChannelExist(channel) != null) {
                client.sendMessage(
                        client.getOper().
                        isChannelExist(channel).getNameReply(true));
            } else {
                IrcProtocol.replyBrackets(
                        IrcProtocol.STATUS.NOSUCHCHANNEL, client, channel);
            }
        }
    }
    

    private void runList(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            client.sendMessage(client.getOper().getListReply());
        }
    }

    
    private void runKick(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            IrcProtocol.replyNotEnoughParams(client, COMMAND.KICK.getText());
        } else {
            if (client.isInChannel()) {
                // check if admin requseting service
                if (client.getNickName().charAt(0) == '@') {
                    // check if user name in server
                    if (client.getOper().isNickNameExist(words.get(1))) {
                        Client clientToKick = client.getOper().
                            getClient(words.get(1));

                        String adminName = client.getNickName().substring(
                                1, client.getNickName().length());

                        if ( ! clientToKick.isInChannel() ) {
                            return;
                        }

                        String kickName = clientToKick.getChannel().getName();

                        // Check if admin and user in the same channel!
                        // and that admin dont try to kick himself
                        if (client.getChannel().getName().equals(kickName) &&
                                ! adminName.equals(words.get(1))) {

                            clientToKick.removeFromChannel();
                        }
                    }
                } else {
                    client.sendMessage(
                            IrcProtocol.STATUS.CHANOPRIVSNEEDED.getNumber() +
                            " #" + client.getChannel().getName() + " " +
                            IrcProtocol.STATUS.CHANOPRIVSNEEDED.getText()); 
                }
            }
        }
    }
}
