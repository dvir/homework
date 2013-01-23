class SingleThread implements ServerConcurrencyModel {
    public void apply (Runnable connectionHandler) {
        connectionHandler.run(); // NOTE HERE - the connection handler runs in the main server thread
    }
}
