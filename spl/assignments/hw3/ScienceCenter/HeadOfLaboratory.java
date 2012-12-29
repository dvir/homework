package ScienceCenter;

import java.util.*;
import java.util.concurrent.*;

public class HeadOfLaboratory {
	private String _name;
	private String _spec;
	private int _numOfScientists;
	private double _cost;
	private ExecutorService _executor;

	public HeadOfLaboratory(String name, String spec, int numOfScientists, double cost){
		_name = name;
		_spec = spec;
		_numOfScientists = numOfScientists;
		_cost = cost;
		_executor = Executors.newFixedThreadPool(_numOfScientists);
	}
	
	/**
	 * Copy constructor
	 * @param lab The laboratory to copy
	 */
	public HeadOfLaboratory(HeadOfLaboratory lab) {
		_name = lab.getName();
		_spec = lab.getSpec();
		_numOfScientists = lab.getNumOfScientists();
		_cost = lab.getCost();
		_executor = Executors.newFixedThreadPool(_numOfScientists);
	}
	
	private int getNumOfScientists() {
		return _numOfScientists;
	}

	public synchronized void addScientists(int amount) {
		_executor.shutdown();
		
		_numOfScientists += amount;
		_executor = Executors.newFixedThreadPool(_numOfScientists);
	}
	
	public void runExperiment(Experiment exp, Observer obs) {
		exp.init();
		
		RunnableExperiment task = new RunnableExperiment(exp);
		task.addObserver(obs);
		_executor.execute(task);
	}
	
	public String getName() {
		return _name;
	}
	
	public String getSpec() {
		return _spec;
	}
	
	public double getCost() {
		return _cost;
	}
	
	public double getCostPerScientist() {
		return _cost / _numOfScientists;
	}
	
	public String toString() {
		return _name + " is a " + _spec + " lab, with " + _numOfScientists + " scientists. ($" + _cost + ")";
	}
}
