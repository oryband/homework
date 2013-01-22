/** @author Eldar Damari, Ory Band */

package irc;

import java.nio.charset.Charset;


public class IrcEncoder implements EncoderInterface {
    private Charset charset;


    /**
     * @param charset character set to encode/decode.
     */
    public IrcEncoder(String charset) {
        this.charset = Charset.forName(charset);
    }


    /**
     * @param str string to encode to byte array.
     */
    public byte[] toBytes(String str) {
        return str.getBytes(this.charset);
    }


    /**
     * @param buffer byte array to conver to string.
     */
    public String fromBytes(byte[] buffer) {
        return new String(buffer, 0, buffer.length, this.charset);
    }

    /**
     * Return the Charset Type.
     */
    public Charset getCharSet() {
        return this.charset;
    }
}
