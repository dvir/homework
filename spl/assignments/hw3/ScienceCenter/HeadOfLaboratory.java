package ScienceCenter;

import java.util.*;
import java.util.concurrent.*;

public class HeadOfLaboratory {
	private String _name; // head of laboratory professor name
	private String _spec; // laboratory specialization
	private int _numOfScientists; // numbers of scientists in the laboratory
	private double _cost; // lab cost
	private ExecutorService _executor; // Thread pool in the size of the amount of scientists in the lab.

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
	
	/**
	 * Initiate the executor service with a thread pool in the size of the amount of scientists.
	 */
	public synchronized void startLab() {
		_executor = Executors.newFixedThreadPool(_numOfScientists);
	}
	
	/**
	 * Shutdown laboratory by shutting down the ExecuterService.
	 */
	public void shutdownLab() {
	   _executor.shutdown();
	}	

	/**
	 * Add scientists to the laboratory.
	 * Will shutdown the executor service and only then re-create it with the new amount of scientists.
	 * @param amount The amount of scientists to increase by.
	 */
	public synchronized void addScientists(int amount) {
		shutdownLab();
		
		_numOfScientists += amount;
		
		startLab();
	}
	
	/**
	 * Send an experiment to the laboratory for execution with an optional Observer object to notify when the experiment has finished.
	 * @param exp The experiment.
	 * @param obs The observer to notify when the experiment has finished.
	 */
	public void runExperiment(Experiment exp, Observer obs) {
		exp.init();
		
		RunnableExperiment task = new RunnableExperiment(exp);
		
		if (obs != null) {
			task.addObserver(obs);
		}
		_executor.execute(task);
	}
	
	private int getNumOfScientists() {
		return _numOfScientists;
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
		StringBuilder sb = new StringBuilder();
		sb.append(_name).append(" is a ").append(_spec).append(" lab, with ").append(_numOfScientists).append(" scientists. ($").append(_cost).append(")");
		return sb.toString();
	}
}
