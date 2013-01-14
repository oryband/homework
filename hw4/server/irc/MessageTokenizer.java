/*package irc;*/


import java.io.InputStreamReader;
import java.lang.StringBuffer;
import java.lang.String;
import java.io.IOException;


public class MessageTokenizer implements TokenizerInterface {

    private final char delimiter;
    private InputStreamReader isr;
    private boolean closed;

    public MessageTokenizer(
            InputStreamReader inputstream,
            char delimiter) {

        this.isr = inputstream;
        this.delimiter = delimiter;
        this.closed = false;
            }
    /**
     * Indicate if reading to StringBuffer failed.
     */
    public boolean isAlive() {
        return !this.closed;
    }

    /**
     * Analize given inputstrem and returns a string.
     *
     * @return string after croped from input stream.
     */
    public String nextToken() throws IOException {

        try {
            if (!isAlive()) {
                throw new IOException();
            } else {

                String ans = null;

                try {
                    int c = 0;
                    StringBuffer sb = new StringBuffer();
                    while ((c = this.isr.read()) != -1) {

                        if ((char)c == this.delimiter) {
                            break;
                        } else {
                            sb.append((char)c);
                        }
                    }
                    ans = sb.toString();
                } 
                catch (IOException e) {

                    this.closed = true;
                    throw e;
                }
                return ans;
            }
        } catch (IOException e) {
            throw e;
        }
    }
}
