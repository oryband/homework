/** @author Eldar Damari, Ory Band. */

package irc;

import java.util.ArrayList;
import java.util.Scanner;


public class IrcProtocol implements AsyncServerProtocol<String> {
    private Client client;
    private boolean shouldClose;
    private boolean connectionTerminated;
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


    public IrcProtocol(Oper oper, Client client) {
        this.oper = oper;
        this.client = client;
        this.shouldClose = false;
        this.connectionTerminated = false;
    }


    public void setShouldClose(boolean status) {
        this.shouldClose = status;
    }

    public boolean shouldClose() {
        return this.shouldClose;
    }


    public void close() {
        this.setShouldClose(true);
    }


	public void connectionTerminated() {
		this.connectionTerminated = true;
	}


    public boolean isEnd(String msg) {
        // Split message to words.
        ArrayList<String> words = split(msg);

        // Don't process an empty message.
        if (words.size() == 0) {
            return false;
        }

        String command = words.get(0);

        return command.equals(Command.COMMAND.QUIT.getText());
    }


    public String processMessage(String msg) {
        // Split message to words.
        ArrayList<String> words = split(msg);

        // Don't process an empty message.
        if (words.size() == 0 || this.connectionTerminated) {
            return "";
        }

        String command = words.get(0);

        if (this.isEnd(msg)) {
            setShouldClose(true);
            this.oper.getCommands().get(command).run(this.client, words);
            return "Goodbye.";
        }


        // Set up new client if it has just connected.
        if (this.client.newUser()) {
            // Wait for NICK command if user hasn't done it yet.
            if ( ! this.client.hasNickname() ) {
                if (command.equals("NICK")) {
                    return this.oper.getCommands().get(command).
                            run(this.client, words);
                } else {
                    return reply(IrcProtocol.STATUS.NOTREGISTERED);
                }
            // Wait for USER command afterwards.
            } else {
                if ( ! this.client.hasUser() ) {
                    if (command.equals("USER")) {
                        return this.oper.getCommands().get(command).
                                run(this.client, words);
                    } else {
                        return reply(IrcProtocol.STATUS.NOTREGISTERED);
                    }
                } else {
                    return "";
                }
            }
        // Process command if client has registered properly.
        } else {
            // COMMAND type message.
            if (this.client.canRegister()) {
                if (this.oper.getCommands().containsKey(command)) {
                    return this.oper.getCommands().get(command).
                            run(this.client, words);
                // DATA type message.
                } else {
                    if (this.client.isInChannel()) {
                        String line = buildString(words);
                        client.getChannel().sendAll(client.getNickName(), line);
                    }

                    return "";
                }
            } else {
                return "";
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

    /**
     * @param reply STATUS reply to send to client.
     * @param client client to send reply to.
     */
    public static String reply(IrcProtocol.STATUS reply) {
        return reply.getNumber() + " :" + reply.getText();
    }

    public static String numericReply(IrcProtocol.STATUS reply) {
        return reply.getNumber() + "";
    }

    public static String textReply(IrcProtocol.STATUS reply) {
        return reply.getText();
    }

    public static String replyNotEnoughParams(String command) {
        return IrcProtocol.STATUS.NEEDMOREPARAMS.getNumber() +
            " " + command + " :" +
            IrcProtocol.STATUS.NEEDMOREPARAMS.getText();
    }

    public static String replyBrackets(
            IrcProtocol.STATUS reply, String brackets) {

        return reply.getNumber() + "<" + brackets + "> :" + reply.getText(); 
    }
}
