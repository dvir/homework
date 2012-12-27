package ScienceCenter;

import java.util.*;
import java.util.concurrent.*;

public class HeadOfLaboratory {
	String _name;
	String _spec;
	int _numOfScientists;
	double _cost;
	ExecutorService _executor;
	
	//constructors
	public HeadOfLaboratory(String name, String spec, int numOfScientists, double cost){
		_name = name;
		_spec = spec;
		_numOfScientists = numOfScientists;
		_cost = cost;
		_executor = Executors.newFixedThreadPool(_numOfScientists);
	}
	
	//behavior
	public synchronized void addScientists(int amount) {
		_executor.shutdown();
		
		_numOfScientists += amount;
		_executor = Executors.newFixedThreadPool(_numOfScientists);
	}
	
	public void runExperiment(Experiment exp, Observer obs) {
		RunnableExperiment task = new RunnableExperiment(exp);
		task.addObserver(obs);
		_executor.execute(task);
	}
	
	public String getSpec() {
		return _spec;
	}
}
