import java.util.*;

public class IRCProtocol implements MessagingProtocol {
    public enum ERR {
        /*
        403 ERR NOSUCHCHANNEL “< channelname > :No such channel”
        —Used to indicate the given channel name is invalid.
        */
        NOSUCHCHANNEL(403, "No such channel"),

        /*
        421 ERR UNKNOWNCOMMAND “< command > :Unknown command”
        – Returned to a registered client to indicate that the command sent is unknown
        by the server.
        */
        UNKNOWNCOMMAND(421, "Unknown command"),
        
        /*
        431 ERR NONICKNAMEGIVEN “:No nickname given”
        — Returned when a nickname parameter expected for a command and isn’t found.
        */
        NONICKNAMEGIVEN(431, "No nickname given"),
       
        /*
        433 ERR NICKNAMEINUSE “< nick > :Nickname is already in use”
        — Returned when a NICK message is processed that results in an attempt to
        change to a currently existing nickname.
        */
        NICKNAMEINUSE(433, "Nickname is already in use"),
       
        /*
        451 ERR NOTREGISTERED “:You have not registered”
        — Returned by the server to indicate that the client must be registered before
        the server will allow it to be parsed in detail.
        */
        NOTREGISTERED(451, "You have not registered"),
        
        /*
        461 ERR NEEDMOREPARAMS “< command > :Not enough parameters”
        — Returned by the server by numerous commands to indicate to the client that
        it didn’t supply a needed parameter.
        */
        NEEDMOREPARAMS(461, "Not enough parameters"),
        
        /*
        462 ERR ALREADYREGISTRED “:You may not reregister”
        — Returned by the server to any link which tries to change part of the registered
        details (such as user details from second USER message).
        */
        ALREADYREGISTERED(462, "You may not reregister"),
       
        /*
        482 ERR CHANOPRIVSNEEDED “< channel > :You’re not channel operator”
        — Any command requiring ’chanop’ privileges (such as KICK messages) must
        return this error if the client making the attempt is not a chanop on the specified
        channel.
        */
        CHANOPRIVSNEEDED(482, "You’re not channel operator");

        private final int _numericReply;
        private final String _text;

        ERR(int numericReply, String text) { 
            _numericReply = numericReply; 
            _text = text;
        }

        public int getNumericReply() { 
            return _numericReply; 
        }

        public String getText() { 
            return _text; 
        }
    };

    public enum RPL {
        /*
         * “< channel >< nick >< nick > [...]]]”
         */
        NAMEREPLY(353),

        /*
         * “< channel > :End of /NAMES list”
         */
        ENDOFNAMES(366),

        /*
         * Replies RPL LISTSTART, RPL LIST, RPL LISTEND mark the start, actual
         * replies with data and end of the server’s response to a LIST command. If there
         *  are no channels available to return, only the start and end reply must be sent.
         */
        LISTSTART(321),

        LIST(322),

        LISTEND(323),

        /*
         * Returned by the server to indicate the NICK command was accepted
         */
        NICKACCEPTED(401),

        /*
         * Returned by the server to indicate the USER command was accepted
         */
        USERACCEPTED(402),

        /*
         * Returned by the server to indicate the KICK command was accepted
         */
        USERKICKED(404),

        /*
         * Returned by the server to indicate the PART command was accepted
         */
        PARTSUCCESS(405);

        private final int _numericReply;

        RPL(int numericReply) { 
            _numericReply = numericReply; 
        }

        public int getNumericReply() { 
            return _numericReply; 
        }
    };

    private boolean _shouldClose;

    private User _user;

    public IRCProtocol() {
        _user = new User();
        _shouldClose = false;
    }

    public void setConnectionHandler(ConnectionHandler ch) {
        _user.setConnectionHandler(ch);
    }

    public boolean shouldClose() {
        return _shouldClose;
    }

    public void connectionTerminated() {
        _shouldClose = true;
        _user.hasQuit();
    }

    public String processMessage(String msg) {
        if (msg.length() == 0) {
            // 3.3.1 empty messages are silently dropped
            return null;
        }

        String[] params = msg.split(" ", 2);
        String command = params[0];
        String data = "";
        if (msg.indexOf(" ") > -1) {
            data = msg.substring(msg.indexOf(" ")+1);
        }
        
        if (command.equals("QUIT")) {
            return quit(data);
        } else if (command.equals("JOIN")) {
            return join(data);
        } else if (command.equals("PART")) {
            return part(data);
        } else if (command.equals("PRIVMSG")) {
            return privmsg(data);
        } else if (command.equals("NOTICE")) {
//            return notice(data);
        } else if (command.equals("TOPIC")) {
//            return topic(data);
        } else if (command.equals("NICK")) {
            return nick(data);
        } else if (command.equals("USER")) {
            return user(data);
        } else if (command.equals("NAMES")) {
            return names(data);
        } else if (command.equals("LIST")) {
            return list();
        }

        return command + " :Unknown command";
    }

    private static String errorReply(IRCProtocol.ERR type, User user) {
        return errorReply(type, user, "");
    }

    private static String errorReply(IRCProtocol.ERR type, User user, String text) {
        if (text.length() > 0) {
            text = " " + text;
        }

        return ":server " + type.getNumericReply() + " " + user.getNick() + text + " :" + type.getText();
    }

    private static String reply(IRCProtocol.RPL type, User user) {
        return reply(type, user, "");
    }

    private static String reply(IRCProtocol.RPL type, User user, String text) {
        if (text.length() > 0) {
            text = " " + text;
        }

        return ":server " + type.getNumericReply() + " " + user.getNick() + text;
    }

    /*
     * NAMES [<channel>]
     */
    private String names(String data) {
        return IRCProtocol.sendNames(_user, data);
    }

    /*
     * LIST
     */
    private String list() {
        List<Channel> channels = Channel.getChannels();

        _user.send(reply(IRCProtocol.RPL.LISTSTART, _user));
        for (int ii = 0; ii < channels.size(); ++ii) {
            _user.send(reply(IRCProtocol.RPL.LIST, _user, channels.get(ii).getName()));
        }
        _user.send(reply(IRCProtocol.RPL.LISTEND, _user));

        return null;
    }

    /*
     * PRIVMSG target :message
     */
    private String privmsg(String data) {
        String[] params = data.split(" ", 2);

        if (data.length() < 2) {
            return errorReply(IRCProtocol.ERR.NEEDMOREPARAMS, _user, "PRIVMSG");
        }

        String target = params[0];
        String message = params[1].substring(1);

        Channel channel = Channel.getChannel(target);
        if (null == channel) {
            return errorReply(IRCProtocol.ERR.NOSUCHCHANNEL, _user, data);
        }

        channel.notifyPrivmsg(_user, message);
        return null; 
    }

    /*
     * USER <username> <hostname> <servername> <realname> (RFC 1459)
     */
    private String user(String data) {
        if (_user.isRegistered()) {
            return errorReply(IRCProtocol.ERR.ALREADYREGISTERED, _user);
        }

        String[] params = data.split(" ", 4);

        if (data.length() < 4) {
            return errorReply(IRCProtocol.ERR.NEEDMOREPARAMS, _user, "USER");
        }

        _user.register(params[0], params[1], params[2], params[3]);
        return reply(IRCProtocol.RPL.USERACCEPTED, _user);
    }

    private String nick(String data) {
        if (data.length() == 0) {
            return errorReply(IRCProtocol.ERR.NEEDMOREPARAMS, _user, "NICK");
        }

        // check if the nick is taken
        User other = User.getUser(data);
        if (other != null) {
            // nick is taken
            return errorReply(IRCProtocol.ERR.NICKNAMEINUSE, _user);
        }

        String oldNick = _user.getNick();

        // set the user nick
        _user.setNick(data);

        // notify other users in the user's channels about the nick change
        _user.notifyNick(oldNick);

        return reply(IRCProtocol.RPL.NICKACCEPTED, _user);
    }

    private String join(String data) {
        if (data.length() == 0) {
            return errorReply(IRCProtocol.ERR.NEEDMOREPARAMS, _user, "JOIN");
        }

        String[] params = data.split(" "); // names and keys
        String[] channelNames = params[0].split(",");

        for (int ii = 0; ii < channelNames.length; ++ii) {
            // call Channel factory to either get the requested channel
            // or create it
            Channel channel = Channel.getChannel(channelNames[ii], true);

            if (null == channel.getChanOp()) {
                channel.setChanOp(_user);
            }

            _user.addChannel(channel);
            channel.addUser(_user);
            channel.notifyJoin(_user);
        }

        return null;
    }

    private String part(String data) {
        if (data.length() == 0) {
            return errorReply(IRCProtocol.ERR.NEEDMOREPARAMS, _user, "PART");
        }

        String[] params = data.split(" "); // names
        String[] channelNames = params[0].split(",");

        for (int ii = 0; ii < channelNames.length; ++ii) {
            // get channel by name
            Channel channel = Channel.getChannel(channelNames[ii]);
            _user.removeChannel(channel);
            channel.removeUser(_user);
            channel.notifyPart(_user);
        }

        return reply(IRCProtocol.RPL.PARTSUCCESS, _user);
    }

    private String quit(String data) {
        _shouldClose = true;
        _user.notifyQuit(data);

        return null;
    }

    public static String sendNames(User user, Channel channel) {
        return sendNames(user, channel.getName());
    }

    public static String sendNames(User user, String data) {
        if (data.length() == 0) {
            List<Channel> channels = Channel.getChannels();
            for (int ii = 0; ii < channels.size(); ++ii) {
                Channel channel = channels.get(ii); 
                user.send(IRCProtocol.RPL.NAMEREPLY, channel.getName() + " " + channel.getNames()); 
            }

            user.send(reply(IRCProtocol.RPL.ENDOFNAMES, user, ":End of /NAMES list.")); 
        } else {
            Channel channel = Channel.getChannel(data);
            if (null == channel) {
                return errorReply(IRCProtocol.ERR.NOSUCHCHANNEL, user, data);
            }

            user.send(reply(IRCProtocol.RPL.NAMEREPLY, user, channel.getName() + " " + channel.getNames())); 
            user.send(reply(IRCProtocol.RPL.ENDOFNAMES, user, channel.getName() + " :End of /NAMES list.")); 
        }

        return null;
    }

    public boolean isEnd(String msg) {
        return msg.equalsIgnoreCase("bye");
    }
}
