package protocol;

import java.nio.charset.CharacterCodingException;

import reactor.*;
import tokenizer.*;

import java.util.*;

public class User { 
    /**
     * Static list of all users created
     */
    private static List<User> _allUsers = new ArrayList<User>();

    /**
     * get list of users stored statitcally
     */
    public static List<User> getUsers() {
        return _allUsers;
    }

    /**
     * Find user with a given nickname and return the 
     * User object representing it.
     */
    public static User getUser(String nick) {
	synchronized (_allUsers) {
            for (int ii = 0; ii < _allUsers.size(); ++ii) {
                if (_allUsers.get(ii).getNick().compareTo(nick) == 0) {
                    return _allUsers.get(ii);
                }
            }
    
            return null;
	}
    }
   
    /**
     * Remove a user from all users list.
     */
    public static void removeUser(User user) {
	synchronized (_allUsers) {
            for (int ii = 0; ii < _allUsers.size(); ++ii) {
                if (_allUsers.get(ii).equals(user)) {
                    _allUsers.remove(ii);
                    break;
                }
            }
	}
    }

    /**
     * Factory method to create a user.
     */
    public static User createUser() {
	synchronized (_allUsers) {
            User user = new User();
            _allUsers.add(user);
            return user; 
	}
    }


    /** NON-STATIC DEFINITIONS **/

    private ConnectionHandler _ch;
    private boolean _isRegistered;
    private String _nick;
    private String _name;
    private List<Channel> _channels;

    /**
     * Create user. 
     * NOTE: Private because this is a factory class.
     */
    private User() {
        _ch = null;
        _isRegistered = false;
        _nick = "";
        _name = "";
        _channels = new ArrayList<Channel>();
    }

    /**
     * Set ConnectionHandler relevant to this user,
     * so we can send him messages directly.
     */
    public void setConnectionHandler(ConnectionHandler ch) {
        _ch = ch;
    }

    /**
     * Whether we already received a ConnectionHandler or not.
     */
    public boolean hasConnectionHandler() {
        return (null != _ch);
    }

    /**
     * Return a list of channels this user is in.
     */
    public List<Channel> getChannels() {
        return _channels;
    }

    /**
     * Add a channel to the user's current channels list.
     */
    public void addChannel(Channel channel) {
        _channels.add(channel);
    }

    /**
     * Remove a channel from the user's current channels list.
     */
    public void removeChannel(Channel channel) {
	synchronized (_channels) {
            for (int ii = 0; ii < _channels.size(); ++ii) {
                if (_channels.get(ii).equals(channel)) {
                   _channels.remove(ii);
                   return;
                }
            }
	}
    }

    /**
     * Set nick for the current user.
     */
    public void setNick(String nick) {
        _nick = nick;
    }

    /**
     * Get the user nick.
     */
    public String getNick() {
        return _nick;
    }

    /**
     * Set the registered name for the current User
     */
    public void setName(String name) {
        _name = name;
    }

    /**
     * Get the user registered name.
     */
    public String getName() {
        return _nick;
    }

    /**
     * Send a message to the user socket.
     */
    public void send(String data) {
	try {
        	_ch.send(data);
	} catch (CharacterCodingException e) {
		System.out.println("Character Coding Exception!");
		System.exit(1);
	}
    }

    /**
     * Send an IRC numeric reply message to the user.
     */
    public void send(IRCProtocol.RPL type, String data) {
        this.send(":server " + getNick() + " " + type.getNumericReply() + " " + data);
    }

    /**
     * Notify the user's channels that he quit the server.
     */
    public void notifyQuit(String data) {
	synchronized (_channels) {
            for (int ii = 0; ii < _channels.size(); ++ii) {
                _channels.get(ii).notifyQuit(this, data);
            }
	}
    }

    /**
     * Process a quit notification received.
     */
    public void quitNotification(User user, String message) {
        String response = ":" + user.getNick() + " QUIT";
        if (message.length() > 0) {
            response += " " + message;
        }

        send(response);
    }

    /**
     * Process a join notification received.
     */
    public void joinNotification(User user, Channel channel) {
        String response = ":" + user.getNick() + " JOIN " + channel.getName();
        send(response);
    }

    /**
     * Process a part notification received.
     */
    public void partNotification(User user, Channel channel) {
        String response = ":" + user.getNick() + " PART " + channel.getName();
        send(response);
    }

    /**
     * Notify the user's channels that he changed nickname.
     */
    public void notifyNick(String oldNick) {
	synchronized (_channels) {
            for (int ii = 0; ii < _channels.size(); ++ii) {
                _channels.get(ii).notifyNick(this, oldNick);
            }
	}
    }

    /**
     * Process a nick notification received.
     */
    public void nickNotification(User user, String oldNick) {
        String response = ":" + oldNick + " NICK " + user.getNick();
        send(response);
    }
    
    /**
     * Process a message received.
     */
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

    /**
     * Returns whether the user has already registered.
     */
    public boolean isRegistered() {
        return _isRegistered;
    }

    /**
     * Execute procedures that should happen when the User
     * quits the server.
     */
    public void hasQuit() {
        User.removeUser(this);
        System.out.println(getNick() + " has quit.");
    }
}
