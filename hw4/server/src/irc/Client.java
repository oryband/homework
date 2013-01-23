/**
 * Represents a client session.
 *
 * @author Eldar Damari, Ory Band.
 */

package irc;

import java.io.IOException;
import java.net.Socket;


public class Client implements Runnable {

    private Socket           socket;
    private EncoderInterface encoder;
    private IrcTokenizer     tokenizer;
    private IrcProtocol      protocol;
    private Oper             oper;

    private String nickName;
    private String user;

    private Channel channel;
    private boolean inChannel;
    private boolean isAdmin;

    private boolean newUser;


    public Client(
            IrcTokenizer     tokenizer,
            EncoderInterface encoder,
            Socket           socket,
            Oper             oper) {


        this.socket    = socket;
        this.encoder   = encoder;
        this.tokenizer = tokenizer;
        this.oper      = oper;

        this.protocol = new IrcProtocol(this.oper, this);

        this.nickName = new String();
        this.user     = new String();

        this.channel   = null;
        this.inChannel = false;
        this.isAdmin   = false;

        this.newUser = true;

        // Welcome message.
        this.sendMessage(
                "Connected to miniIRC server on "
                + socket.getInetAddress() + ":" + socket.getPort());
    }


    public void run() {
        while ( ! socket.isClosed() && ! protocol.shouldClose()) {
            if ( ! this.tokenizer.isAlive() ) {
                this.protocol.close();
            } else {
                try {
                    String msg = this.tokenizer.nextToken();
                    String reply = this.protocol.processMessage(msg); 
                    sendMessage(reply);
                } catch (IOException e) {
                    System.out.println(
                            "Error analyzing message from client '" +
                            this.user + "/" + this.nickName + "'.");

                    continue;
                }
            }
        }

        // Connection has closed.
        this.oper.removeClient(this);

        System.out.println(
                "Client " + this.user + "/" + this.nickName +
                " has disconnected.");

        System.out.println(
                "Currently connected users: " + this.oper.clients.toString());

        try {
            this.socket.close();
        } catch (IOException e) {
            System.out.println(
                    "Error closing socket for client " +
                    this.user + "/" + this.nickName + "'.");
        }
    }


    public void addChannel(Channel channel) {
        this.channel = channel;
        this.inChannel = true;
        channel.addUser(this);
    } 


    public void setIsInChannel(boolean status) {
        this.inChannel = status;
    }

    public void setNickName(String nick) {
        this.nickName = nick;
        this.checkNewUser();
    }

    public void setUser(String user) {
        this.user = user;
        this.checkNewUser();
    }

    public void setAsAdmin() {
        this.isAdmin = true;
        this.nickName = "@" + this.nickName;
    }

    public void setAsNotAdmin() {
        this.isAdmin = false;
        this.nickName = this.nickName.substring(1, this.nickName.length());
    }

    public void setProtocolShouldClose() {
        this.protocol.setShouldClose(true);
    }


    public Socket getSocket() {
        return this.socket;
    }

    public String getNickName() {
        return this.nickName;
    }

    public Channel getChannel() {
        return this.channel;
    }

    public Oper getOper() {
        return this.oper;
    }


    public boolean isUserNameExist() {
        return this.user.length() != 0;
    }

    public boolean isInChannel() {
        return this.inChannel;
    }

    public boolean isAdmin() {
        return this.isAdmin;
    }


    public boolean hasNickname() {
        return this.nickName.length() != 0;
    }

    public boolean hasUser() {
        return this.user.length() != 0;
    }

    public boolean newUser() {
        return this.newUser;
    }


    /** Sets user has 'not new' if it has already set up a NICK and USER. */
    private void checkNewUser() {
        if (this.hasNickname() && this.hasUser()) {
            this.newUser = false;
        }
    }


    public boolean canRegister() {
        return this.nickName.length() != 0 && this.user.length() != 0;
    }


    public void removeFromChannel() {
        this.channel.removeUser(this);

        // Delete channel if empty.
        if (this.channel.isEmpty()) {
            this.oper.removeChannel(this.channel);
        }

        this.channel = null;
        this.inChannel = false;
    }


    public void sendMessage(String msg) {
        String NEW_LINE = System.getProperty("line.separator");
        String newmsg = msg + NEW_LINE; 
        byte[] buf = this.encoder.toBytes(newmsg);

        try {
            this.socket.getOutputStream().write(buf, 0, buf.length);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
