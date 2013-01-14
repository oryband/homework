//package irc;


import java.lang.String;
import java.nio.charset.Charset;


public interface EncoderInterface {

    public byte[] toBytes(String str);
    public String fromBytes(byte[] buffer);
    public Charset getCharSet();

}

