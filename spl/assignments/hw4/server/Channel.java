import java.util.*;

public class Channel {
    private static List<Channel> _allChannels = new ArrayList<Channel>();

    public static List<Channel> getChannels() {
        return _allChannels;
    }

    /**
     * Find channel with a given name and return the 
     * Channel object representing it.
     */
    public static Channel getChannel(String name, boolean create) {
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

    public static Channel getChannel(String name) {
        return getChannel(name, false);
    }

    public static void removeChannel(Channel channel) {
        for (int ii = 0; ii < _allChannels.size(); ++ii) {
            if (_allChannels.get(ii).equals(channel)) {
                _allChannels.remove(ii);
                break;
            }
        }
    }


    private String _name;
    private List<User> _users;
    private User _chanop;

    public Channel(String name) {
        _name = name;
        _users = new ArrayList<User>();
        _chanop = null;
    }

    public void setChanOp(User user) {
        _chanop = user;
    }

    public User getChanOp() {
        return _chanop;
    }

    public List<User> getUsers() {
        return _users;
    }

    public String getName() {
        return _name;
    }
    
    /**
     * Send message to all users in the channel.
     */
    public void notifyPrivmsg(User user, String data) {
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
    public void notifyQuit(User user, String data) {
        removeUser(user);

        for (int jj = 0; jj < _users.size(); ++jj) {
            _users.get(jj).quitNotification(user, data);
        }
    }
    
    /**
     * Notify all the users in the channel that a user has changed his nick.
     */
    public void notifyNick(User user, String oldNick) {
        for (int jj = 0; jj < _users.size(); ++jj) {
            _users.get(jj).nickNotification(user, oldNick);
        }
    }
    
    /**
     * Notify all the users in the channel that a user has joined 
     * and send him the channel names list.
     */
    public void notifyJoin(User user) {
        String names = "";
        for (int jj = 0; jj < _users.size(); ++jj) {
            _users.get(jj).joinNotification(user, this);

            names += _users.get(jj).getNick() + " ";
        }

        IRCProtocol.sendNames(user, this);
    }

    public String getNames() {
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

    public void addUser(User user) {
        _users.add(user);
    }
    
    /**
     * Notify all the users in the channel that a user has parted.
     */
    public void notifyPart(User user) {
        String names = "";
        for (int jj = 0; jj < _users.size(); ++jj) {
            _users.get(jj).partNotification(user, this);

            names += _users.get(jj).getNick() + " ";
        }
    }

    public void removeUser(User user) {
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
}
