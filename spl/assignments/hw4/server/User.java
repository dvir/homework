import java.util.*;

public class User { 
    private static List<User> _allUsers = new ArrayList<User>();

    public static List<User> getUsers() {
        return _allUsers;
    }

    /**
     * Find user with a given nickname and return the 
     * User object representing it.
     */
    public static User getUser(String nick) {
        for (int ii = 0; ii < _allUsers.size(); ++ii) {
            if (_allUsers.get(ii).getNick().compareTo(nick) == 0) {
                return _allUsers.get(ii);
            }
        }

        return null;
    }
    
    public static void removeUser(User user) {
        for (int ii = 0; ii < _allUsers.size(); ++ii) {
            if (_allUsers.get(ii).equals(user)) {
                _allUsers.remove(ii);
                break;
            }
        }
    }

    public static User createUser() {
        User user = new User();
        _allUsers.add(user);
        return user; 
    }

    /** NON-STATIC DEFINITIONS **/

    private ConnectionHandler _ch;
    private boolean _isRegistered;
    private String _nick;
    private String _name;
    private List<Channel> _channels;

    public User() {
        _ch = null;
        _isRegistered = false;
        _nick = "";
        _name = "";
        _channels = new ArrayList<Channel>();
    }

    public void setConnectionHandler(ConnectionHandler ch) {
        _ch = ch;
    }

    public boolean hasConnectionHandler() {
        return (null != _ch);
    }

    public List<Channel> getChannels() {
        return _channels;
    }

    public void setNick(String nick) {
        _nick = nick;
    }

    public String getNick() {
        return _nick;
    }

    public void setName(String name) {
        _name = name;
    }

    public String getName() {
        return _nick;
    }

    public void send(String data) {
        _ch.send(data);
    }

    public void send(IRCProtocol.RPL type, String data) {
        _ch.send(":server " + getNick() + " " + type.getNumericReply() + " " + data);
    }

    public void notifyQuit(String data) {
        List<Channel> channels = getChannels();
        for (int ii = 0; ii < channels.size(); ++ii) {
            channels.get(ii).notifyQuit(this, data);
        }
    }

    public void quitNotification(User user, String message) {
        String response = ":" + user.getNick() + " QUIT";
        if (message.length() > 0) {
            response += " " + message;
        }

        send(response);
    }

    public void joinNotification(User user, Channel channel) {
        String response = ":" + user.getNick() + " JOIN " + channel.getName();
        send(response);
    }

    public void partNotification(User user, Channel channel) {
        String response = ":" + user.getNick() + " PART " + channel.getName();
        send(response);
    }

    public void notifyNick(String oldNick) {
        List<Channel> channels = getChannels();
        for (int ii = 0; ii < channels.size(); ++ii) {
            channels.get(ii).notifyNick(this, oldNick);
        }
    }

    public void nickNotification(User user, String oldNick) {
        String response = ":" + oldNick + " NICK " + user.getNick();
        send(response);
    }
    
    public void privmsgNotification(User user, Channel channel, String message) {
        String response = ":" + user.getNick() + " PRIVMSG " + channel.getName() + " :" + message;
        send(response);
    }

    /*
     * USER <username> <hostname> <servername> <realname> (RFC 1459)
     */
    public void register(String username, String hostname, String servername, String realname) {
        // remove prefixing colon if present
        if (realname.charAt(0) == ':') {
            realname = realname.substring(1);
        }

        _name = realname;
        _isRegistered = true;
    }

    public void addChannel(Channel channel) {
        _channels.add(channel);
    }

    public void removeChannel(Channel channel) {
        for (int ii = 0; ii < _channels.size(); ++ii) {
            if (_channels.get(ii).equals(channel)) {
                _channels.remove(ii);
                return;
            }
        }
    }

    public boolean isRegistered() {
        return _isRegistered;
    }

    public void hasQuit() {
        User.removeUser(this);
        System.out.println(getNick() + " has quit.");
    }
}
