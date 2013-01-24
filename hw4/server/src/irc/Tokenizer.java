/**
 * Handles message/stream tokenizing.
 *
 * @author Eldar Damari, Ory Band
 */

package irc;

import java.io.IOException;


public interface Tokenizer {
    /**
     * @return tokenized (delimeter-cropped) string.
     */
    public String nextToken() throws IOException;

    /**
     * Indicates whether reading to StringBuffer has failed.
     */
    public boolean isAlive();
}
