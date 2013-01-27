import java.util.*;

public class Channel {
    /**
     * Static list of channels created by our factory.
     */
    private static List<Channel> _allChannels = new ArrayList<Channel>();

    /**
     * Returns list of channels created by our factory.
     */
    public static List<Channel> getChannels() {
        return _allChannels;
    }

    /**
     * Find channel with a given name and return the 
     * Channel object representing it.
     * If it doesn't exist and @param create is true, create it.
     */
    public static Channel getChannel(String name, boolean create) {
	synchronized (_allChannels) {
            for (int ii = 0; ii < _allChannels.size(); ++ii) {
                if (_allChannels.get(ii).getName().compareTo(name) == 0) {
                    return _allChannels.get(ii);
                }
            }
    
            // if we got here, channel doesn't exist it.
            if (create) {
                // create it.
                Channel newChannel = new Channel(name);
                _allChannels.add(newChannel);
                return newChannel;
            }

	    return null;
	}
    }

    /**
     * Proxy for getChannel, defining @param create to be false.
     */
    public static Channel getChannel(String name) {
        return getChannel(name, false);
    }

    /**
     * Remove a channel from our factory.
     */
    public static void removeChannel(Channel channel) {
	synchronized (_allChannels) {
            for (int ii = 0; ii < _allChannels.size(); ++ii) {
                if (_allChannels.get(ii).equals(channel)) {
                    _allChannels.remove(ii);
                    break;
                }
            }
	}
    }


    /** NON-STATIC DEFINITIONS **/
    
    private String _name; // the channel name
    private List<User> _users; // the list of users in the Channel
    private User _chanop; // the chanop user.

    /**
     * Channel constructor, receiving only a name.
     * NOTE: Private because this is factory class.
     */
    private Channel(String name) {
        _name = name;
        _users = new ArrayList<User>();
        _chanop = null;
    }

    /**
     * Set the channel's chanop user.
     */
    public void setChanOp(User user) {
        _chanop = user;
    }

    /**
     * Get the channel's chanop user.
     */
    public User getChanOp() {
        return _chanop;
    }

    /**
     * Get a list of users currently in the channel.
     */
    public List<User> getUsers() {
        return _users;
    }

    /**
     * Add user to the channel users list.
     */
    public synchronized void addUser(User user) {
        _users.add(user);
    }
    
    /**
     * Remove user from the channel users list.
     */
    public synchronized void removeUser(User user) {
        for (int ii = 0; ii < _users.size(); ++ii) {
            if (_users.get(ii).equals(user)) {
                _users.remove(ii);
                break;
            }
        }

        if (_users.size() == 0) {
            Channel.removeChannel(this);
        }
    }

    /**
     * Get the channel name.
     */
    public String getName() {
        return _name;
    }
    
    /**
     * Send message to all users in the channel.
     */
    public synchronized void notifyPrivmsg(User user, String data) {
        for (int jj = 0; jj < _users.size(); ++jj) {
            User currentUser = _users.get(jj);
            if (currentUser.equals(user)) {
                continue;
            }

            currentUser.privmsgNotification(user, this, data);
        }
    }

    /**
     * Remove user from users list and then notify the remaining users
     * that the user has quit.
     */
    public synchronized void notifyQuit(User user, String data) {
        removeUser(user);

        for (int jj = 0; jj < _users.size(); ++jj) {
            _users.get(jj).quitNotification(user, data);
        }
    }
    
    /**
     * Notify all the users in the channel that a user has changed his nick.
     */
    public synchronized void notifyNick(User user, String oldNick) {
        for (int jj = 0; jj < _users.size(); ++jj) {
            _users.get(jj).nickNotification(user, oldNick);
        }
    }

    /**
     * Notify all the users in the channel that a user has joined 
     * and send him the channel names list.
     */
    public synchronized void notifyJoin(User user) {
        String names = "";
        for (int jj = 0; jj < _users.size(); ++jj) {
            _users.get(jj).joinNotification(user, this);

            names += _users.get(jj).getNick() + " ";
        }

        IRCProtocol.sendNames(user, this);
    }

    /**
     * Get a space-separated string of all names in the channel, 
     * with their chan mode prefixing their name.
     */
    public synchronized String getNames() {
        String names = "";
        for (int jj = 0; jj < _users.size(); ++jj) {
            User user = _users.get(jj);

            if (null != getChanOp() && getChanOp().equals(user)) {
                names += "@";
            }

            names += user.getNick() + " ";
        }

        names = names.trim();
        return names;
    }

    /**
     * Notify all the users in the channel that a user has parted.
     */
    public synchronized void notifyPart(User user) {
        String names = "";
        for (int jj = 0; jj < _users.size(); ++jj) {
            _users.get(jj).partNotification(user, this);

            names += _users.get(jj).getNick() + " ";
        }
    }
}
