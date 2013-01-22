/** @author Eldar Damari, Ory Band. */

package irc;

import java.util.ArrayList;
import java.util.Scanner;


public class IrcProtocol implements ProtocolInterface {
    private boolean shouldClose;
    private Oper oper;


    /** Enumerates all error/reply codes. */
    public enum STATUS {
        // Error codes.
        NOSUCHCHANNEL     ( 403, "No such channel"             ),
        UNKNOWNCOMMAND    ( 421, "Unknown command"             ),
        NONICKNAMEGIVEN   ( 431, "No nickname given"           ),
        NICKNAMEINUSE     ( 433, "Nickname is already in use"  ),
        NOTREGISTERED     ( 451, "You have not registered"     ),
        NEEDMOREPARAMS    ( 461, "Not enough parameters"       ),
        ALREADYREGISTERED ( 462, "You may not reregister"      ),
        CHANOPRIVSNEEDED  ( 482, "You’re not channel operator" ),

        // Reply codes.
        NAMEREPLY    ( 353                       ),
        ENDOFNAMES   ( 366, "End of /NAMES list" ),
        LISTSTART    ( 321                       ),
        LIST         ( 322                       ),
        LISTEND      ( 323                       ),
        NICKACCEPTED ( 401                       ),
        USERACCEPTED ( 402                       ),
        USERKICKED   ( 404                       ),
        PARTSUCCESS  ( 405                       );


        private final int    _number;
        private final String _text;

        STATUS(int number) {
            _number = number;
            _text   = number + "";  // Use number string representation.
        }

        STATUS(int number, String text) {
            _number = number;
            _text = text;
        }

        public int getNumber() {
            return this._number;
        }

        public String getText() {
            return this._text;
        }
    };


    /**
     * @param reply STATUS reply to send to client.
     * @param client client to send reply to.
     */
    public static void reply(IrcProtocol.STATUS reply, Client client) {
        client.sendMessage(reply.getNumber() + " :" + reply.getText());
    }

    public static void numericReply(IrcProtocol.STATUS reply, Client client) {
        client.sendMessage(reply.getNumber() + "");
    }

    public static void textReply(IrcProtocol.STATUS reply, Client client) {
        client.sendMessage(reply.getText());
    }

    public static void replyNotEnoughParams(Client client, String command) {
        client.sendMessage(
                IrcProtocol.STATUS.NEEDMOREPARAMS.getNumber() +
                " " + command + " :" +
                IrcProtocol.STATUS.NEEDMOREPARAMS.getText());
    }

    public static void replyBrackets(
            IrcProtocol.STATUS reply, Client client, String brackets) {

        client.sendMessage(
                reply.getNumber() + "<" + brackets + "> :" + reply.getText()); 
    }

    public IrcProtocol(Oper oper) {
        this.oper = oper;
    }


    public void setShouldClose(boolean status) {
        this.shouldClose = status;
    }
    

    public boolean getShouldClose() {
        return this.shouldClose;
    }


    public void close() {
        this.setShouldClose(true);
    }


    public void processInput(String msg, Client client) {
        // Split message to words.
        ArrayList<String> words = split(msg);

        // Don't process an empty message.
        if (words.size() == 0) {
            return;
        }

        String command = words.get(0);

        // Set up new client if it has just connected.
        if (client.newUser()) {

            // Wait for NICK command if user hasn't done it yet.
            if ( ! client.hasNickname() ) {

                if (command.equals("NICK")) {
                    this.oper.getCommands().get(command).run(client, words);
                } else {
                    reply(IrcProtocol.STATUS.NOTREGISTERED, client);
                }
            // Wait for USER command afterwards.
            } else {
                if ( ! client.hasUser() ) {
                    if (command.equals("USER")) {
                        this.oper.getCommands().get(command).run(client, words);
                    } else {
                        reply(IrcProtocol.STATUS.NOTREGISTERED, client);
                    }
                }
            }
        // Process command if client has registered properly.
        } else {
            // COMMAND type message.
            if (client.canRegister()) {
                if (this.oper.getCommands().containsKey(command)) {
                    this.oper.getCommands().get(command).run(client, words);
                // DATA type message.
                } else {
                    if (client.isInChannel()) {
                        String line = buildString(words);
                        client.getChannel().sendAll(client.getNickName(), line);
                    }
                }
            }
        }
    }


    public ArrayList<String> split(String msg) {
        ArrayList<String> lines = new ArrayList<String>();

        // Don't split an empty message.
        if (msg.length() == 0) {
            return lines;
        }

        // Get message without delimeter.
        String message = msg.substring(0, msg.length() - 1);

        // We check if the user entered a single command with no parameters.
        if (this.oper.getCommands().containsKey(message)) {
            lines.add(message);
            return lines;
        }

        Scanner s = new Scanner(msg);
        s.useDelimiter("\\s");

        boolean flag = true;
        StringBuilder str = new StringBuilder();
        while (s.hasNext()) {
            if (flag) {
                lines.add(s.next());
                flag = false;
            } else {
                str.append(s.next());
                str.append(" ");
            }
        }

        if (str.length() == 0) {
            return lines;
        }

        String words = str.toString();
        words = words.substring(0, words.length() - 1);
        lines.add(words);

        s.close();

        return lines;
    }


    public String buildString(ArrayList<String> words) {
        StringBuilder str = new StringBuilder();

        // Append words to string.
        for (String word : words) {
            str.append(word);
            str.append(" ");
        }

        String line = str.toString();

        line = line.substring(0, line.length() - 1);  // Remove EOS char.

        return line;
    }
}
