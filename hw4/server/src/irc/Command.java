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
    public String run(Client client, ArrayList<String> words) {
        switch(this.command) {
            case NICK:
                return this.runNick(client,words);
            case USER:
                return this.runUser(client,words);
            case QUIT:
                return this.runQuit(client, words);
            case JOIN:
                return this.runJoin(client, words);
            case PART:
                return this.runPart(client, words);
            case NAMES:
                return this.runNames(client, words);
            case LIST:
                return this.runList(client, words);
            case KICK:
                return this.runKick(client, words);
            default:
                return "ERROR";
        }
    }


    private String runNick(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            return IrcProtocol.reply(IrcProtocol.STATUS.NONICKNAMEGIVEN);
        } else if (words.size() == 2) {
            if (client.getOper().isNickNameExist(words.get(1))) {
                return IrcProtocol.replyBrackets(
                        IrcProtocol.STATUS.NICKNAMEINUSE,
                        words.get(1));
            } else {
                client.setNickName(words.get(1));
                return IrcProtocol.textReply(
                        IrcProtocol.STATUS.NICKACCEPTED);
            }
        } else {
            return "";
        }
    }


    private String runUser(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            return IrcProtocol.replyNotEnoughParams(COMMAND.USER.getText());
        } else if (words.size() == 2) {
            if ( ! client.isUserNameExist() ) {
                client.setUser(words.get(1));
                return IrcProtocol.textReply(IrcProtocol.STATUS.USERACCEPTED);
            } else {
                return IrcProtocol.reply(IrcProtocol.STATUS.ALREADYREGISTERED);
            }
        } else {
            return "";
        }
    }


    private String runJoin(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            return IrcProtocol.replyNotEnoughParams(COMMAND.JOIN.getText());
        } else {
            if (words.size() == 2) {
                // Check for '#' before channel name.
                String channelName = words.get(1);
                if (channelName.charAt(0) != '#') {
                    return "";  // Silently drop illegal command.
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
                            client.getOper().isChannelExist(channelName));


                    return client.getChannel().getNameReply(true);
                // Add client to channel if it already exists.
                } else {
                    client.addChannel(channel);
                    return channel.getNameReply(true);
                }
            } else {
                return "";
            }
        }
    }


    private String runQuit(Client client, ArrayList<String> words) {
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

        return "";
    }


    private String runPart(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            return IrcProtocol.replyNotEnoughParams(COMMAND.PART.getText());
        } else if (words.size() == 2 && client.isInChannel()) {
            if (client.getChannel().getName().equals(words.get(1))) {
                client.removeFromChannel();

                return IrcProtocol.numericReply(
                        IrcProtocol.STATUS.PARTSUCCESS);
            } else {
                return IrcProtocol.replyBrackets(
                        IrcProtocol.STATUS.NOSUCHCHANNEL,
                        words.get(1));
            }
        } else {
            return "";
        }
    }


    private String runNames(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            StringBuilder names = new StringBuilder();

            // Build string with all data.
            for (Channel channel : client.getOper().getChannels()) {
                names.append(channel.getNameReply(false));
            }
            
            names.append(
                    IrcProtocol.STATUS.ENDOFNAMES.getNumber() + " :" +
                    IrcProtocol.STATUS.ENDOFNAMES.getText() + '\n');

            String finalNames = names.toString();

            return finalNames; 
        } else {
            String channel = words.get(1);

            // Check if there is # before and id channel exist.
            channel = channel.substring(1, channel.length());

            if (client.getOper().isChannelExist(channel) != null) {
                return client.getOper().
                        isChannelExist(channel).getNameReply(true);
            } else {
                return IrcProtocol.replyBrackets(
                        IrcProtocol.STATUS.NOSUCHCHANNEL, channel);
            }
        }
    }
    

    private String runList(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            return client.getOper().getListReply();
        } else {
            return "";
        }
    }

    
    private String runKick(Client client, ArrayList<String> words) {
        if (words.size() == 1) {
            return IrcProtocol.replyNotEnoughParams(COMMAND.KICK.getText());
        } else if (client.isInChannel()) {
            // check if admin requseting service
            if (client.getNickName().charAt(0) == '@') {
                // check if user name in server
                if (client.getOper().isNickNameExist(words.get(1))) {
                    Client clientToKick = client.getOper().
                        getClient(words.get(1));

                    String adminName = client.getNickName().substring(
                            1, client.getNickName().length());

                    if ( ! clientToKick.isInChannel() ) {
                        return "";
                    }

                    String kickName = clientToKick.getChannel().getName();

                    // Check if admin and user in the same channel!
                    // and that admin dont try to kick himself
                    if (client.getChannel().getName().equals(kickName) &&
                            ! adminName.equals(words.get(1))) {

                        clientToKick.removeFromChannel();
                    }

                    return "";
                } else {
                    return "";
                }
            } else {
                return IrcProtocol.STATUS.CHANOPRIVSNEEDED.getNumber() +
                    " #" + client.getChannel().getName() + " " +
                    IrcProtocol.STATUS.CHANOPRIVSNEEDED.getText();
            }
        } else {
            return "";
        }
    }
}
