package irc;


import java.lang.String;
import java.nio.charset.Charset;


public class IrcEncoder implements EncoderInterface {

    Charset charset;


    public IrcEncoder(String charsetType) {
        this.charset = Charset.forName(charsetType);
    }

    /**
     * Encodes the string into a sequence of bytes using the 
     * given charset.
     *
     * @param str string to encode.
     */
    public byte[] toBytes(String str) {
        return str.getBytes(this.charset);
    }

    /**
     * Read from buffer and convert it to a String.
     *
     * @param buffer byte[] to String.
     */
    public String fromBytes(byte[] buffer) {
        return new String(buffer,0,buffer.length,this.charset);
    }

    /**
     * Return the Charset Type.
     */
    public Charset getCharSet() {
        return this.charset;
    }
}
