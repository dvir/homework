import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

class Server {
    public static void main(String[] args) 
        throws NumberFormatException, IOException 
    {
        if (args.length != 1) {
            System.err.println("please supply only one argument, the port to bind.");
            return;
        }

        /*The characteristic of the server concurrency model is determined by the selected implementation for the scm 
         *         instance (SingleThread, ThreadPerClient or ThreadPool - as described in the next sections)*/
        ServerConcurrencyModel scm = new ThreadPerClient();  

        // set encoding to UTF-8
        Encoder encoder = new SimpleEncoder("UTF-8");

        // receive port from command line
        ServerSocket socket = new ServerSocket(Integer.parseInt(args[0]));

        // start client accepting loop
        while (true) {
            Socket s = socket.accept();
            Tokenizer tokenizer = new MessageTokenizer(new InputStreamReader(s.getInputStream(), encoder.getCharset()), '\n');

            // create IRCProtocol instance for this specific User
            MessagingProtocol protocol = new IRCProtocol();

            // associate socket, encoder, tokenizer and protocol with ConnectionHandler
            Runnable connectionHandler = new ConnectionHandler(s, encoder, tokenizer, protocol);

            // let the IRCProtocol know of the ConnectionHandler
            ((IRCProtocol)protocol).setConnectionHandler((ConnectionHandler) connectionHandler);

            // apply ServerConcurrencyModel to the ConnectionHandler
            scm.apply(connectionHandler);
        }
    }
}
