import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

class ThreadPool implements ServerConcurrencyModel {

    private ExecutorService _executor;

    public ThreadPool(int iNumberOfThreads) {
        _executor = Executors.newFixedThreadPool(iNumberOfThreads);
    }

    public void apply (Runnable connectionHandler) {
        _executor.execute(connectionHandler);
    }
}
