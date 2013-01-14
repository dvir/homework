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
        Encoder encoder = new SimpleEncoder("UTF-8");
        ServerSocket socket = new ServerSocket(Integer.parseInt(args[0]));
        while (true) {
            Socket s = socket.accept();
            Tokenizer tokenizer = new MessageTokenizer(new InputStreamReader(s.getInputStream(), encoder.getCharset()), '\n');
            MessagingProtocol protocol = new IRCProtocol();
            Runnable connectionHandler = new ConnectionHandler(s, encoder, tokenizer, protocol);
            ((IRCProtocol)protocol).setConnectionHandler((ConnectionHandler) connectionHandler);
            scm.apply(connectionHandler);
        }
    }
}
