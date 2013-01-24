/** @author Eldar Damari, Ory Band */

package irc;

import java.lang.StringBuffer;
import java.io.InputStreamReader;
import java.io.IOException;


public class IrcTokenizer implements Tokenizer {
    private final char delimiter;
    private InputStreamReader stream;
    private boolean closed;


    /**
     * @param stream Stream to tokenize.
     * @param delimeter delimeter character. i.e. '\n'.
     */
    public IrcTokenizer(InputStreamReader stream, char delimiter) {
        this.stream    = stream;
        this.delimiter = delimiter;
        this.closed    = false;
    }


    public boolean isAlive() {
        return ! this.closed;
    }


    public String nextToken() throws IOException {
        // Don't tokenize if some error happened before reaching here.
        if ( ! isAlive() ) {
            throw new IOException("Can't tokenize - Tokenizer is closed.");
        }

        String ans = null;

        try {
            int c = 0;
            StringBuffer buffer = new StringBuffer();

            // Keep reading from stream until delimeter or end of
            // message is reached.
            while ( (c = this.stream.read()) != -1) {
                if ( (char) c == this.delimiter ) {
                    break;
                } else {
                    buffer.append( (char) c );
                }
            }

            ans = buffer.toString();

        } catch (IOException e) {
            this.closed = true;  // Close tokenizer in case of error.
            throw e;
        }

        return ans;
    }
}
