import java.nio.charset.Charset;

public interface Encoder {
    public byte [] toBytes(String s);
    public String fromBytes(byte [] buf);
    public Charset getCharset();
}
