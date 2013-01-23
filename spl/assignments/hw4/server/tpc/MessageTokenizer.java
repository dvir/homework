import java.io.IOException;
import java.io.InputStreamReader;

public class MessageTokenizer implements Tokenizer {

    public final char _delimiter;
    private final InputStreamReader _isr;
    private boolean _closed;

    public MessageTokenizer (InputStreamReader isr, char delimiter) {
        _delimiter = delimiter;
        _isr = isr;
        _closed = false;
    }

    public String nextToken() throws IOException {
        if (!isAlive())
            throw new IOException("tokenizer is closed");

        String ans = null;
        try {
            // we are using a blocking stream, so we should always end up
            // with a message, or with an exception indicating an error in
            // the connection.
            int c;
            StringBuilder sb = new StringBuilder();
            // read char by char, until encountering the framing character, or
            // the connection is closed.
            while ((c = _isr.read()) != -1) {
                if (c == _delimiter)
                    break;
                else
                    sb.append((char) c);
            }
            ans = sb.toString();
        } catch (IOException e) {
            _closed = true;
            throw new IOException("Connection is dead");
        }
        return ans;
    }

    public boolean isAlive() {
        return !_closed;
    }
}
