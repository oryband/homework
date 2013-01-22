/**
 * Handles protocol interpretation.
 *
 * @author Eldar Damari, Ory Band.
 */

package irc;

import java.util.ArrayList;


public interface ProtocolInterface {
    public void    close();
    public void    setShouldClose(boolean status);
    public boolean getShouldClose();

    /**
     * @param msg message to process.
     * @param client client object to operate on, depending on message received.
     */
    public void    processInput(String msg, Client client);

    /**
     * @param msg message to split.
     */
    public ArrayList<String> split(String msg);

    /**
     * @param words word list to build string from.
     */
    public String buildString(ArrayList<String> words);
}
