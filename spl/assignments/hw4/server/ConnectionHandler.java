import java.io.IOException;
import java.net.Socket;

public class ConnectionHandler implements Runnable {

    private final Socket _socket;
    private final Encoder _encoder;
    private final Tokenizer _tokenizer;
    private final MessagingProtocol _protocol;

    /**
     * Construct a ConnectionHandler from a set of Sockett, Encoder,
     * Tokenizer and MessagingProtocol.
     */
    public ConnectionHandler(Socket s, Encoder encoder, Tokenizer tokenizer, MessagingProtocol protocol) {
        _socket = s;
        _encoder = encoder;
        _tokenizer = tokenizer;
        _protocol = protocol;
    }

    /**
     * Runnable method for ConnectionHandler.
     * Each ConnectionHandler (possibly) runs in it's own
     * thread to avoid blocking when having multiple
     * ConnectionHandler running concurrently.
     */
     public void run() {
        // while the protocol isn't initiating a shutdown
        // procedure, and the socket is still open - try to read
        // a new token from the socket and process it by the protocol.
        while (!_protocol.shouldClose() && !_socket.isClosed()) {                          
            try {
                if (!_tokenizer.isAlive())
                    _protocol.connectionTerminated();
                else {
                    String msg = _tokenizer.nextToken();
                    String ans = _protocol.processMessage(msg);
                    if (ans != null) {
                        if (!this.send(ans)) {
                            // exception thrown inside send.
                            // close connection
                            break;
                        }
                    }
                }
            } catch (IOException e) {
                // in case an error occurred, terminate connection.
                _protocol.connectionTerminated();
                break;
            }
        }
       
        try {
            _socket.close();
        } catch (IOException ignored) {

        }
       
        System.out.println("thread done");
    }

    /**
     * Send a string message to the associated socket,
     * encoded by the given Encoder.
     */
    public boolean send(String data) { 
        try {
            byte[] buf = _encoder.toBytes(data + "\n");
            _socket.getOutputStream().write(buf, 0, buf.length);
        } catch (IOException e) {
            _protocol.connectionTerminated();
        }

        return true;
    }
}
