/**
 * 
 */
package ScienceCenter;

import java.util.*;

/**
 * @author Dvir
 *
 */
public class RunnableExperiment extends Observable implements Runnable {
	private int _days; // Days that the experiment took 
	private long _startTime;
	private long _endTime;
	private long _runtime;
	private Experiment _experiment;
	
	public RunnableExperiment(Experiment e) {
		this._experiment = e;
		this._days = 1;
		this._startTime = 0;
		this._endTime = 0;
		this._runtime = -1;
	}
	
	public void run() {
		_startTime = new Date().getTime();
		
		// get required equipment
		
		// find a laboratory to handle our experiment
		
		
		_endTime = new Date().getTime();
		_runtime = _endTime - _startTime;
		
		// do something with runtime, like give rewards(!!!)
		this.notifyObservers();
	}
	
	public long getRuntime() {
		return _runtime;
	}
	
	public Experiment getExperiment() {
		return _experiment;
	}
}
