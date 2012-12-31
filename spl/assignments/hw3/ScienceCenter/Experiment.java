/**
 * 
 */
package ScienceCenter;

import java.util.*;
/**
 * @author Dvir, Or
 *
 */
public class Experiment {
	// experiment state object.
	private enum State {
		INCOMPLETE, // hasn't started yet
		INPROGRESS, // in progress
		COMPLETE // finished
	};	
	
	private int _id; // the experiment id
	private String _spec; // the experiment specialization
	private int _allowedRuntime; // the allowed runtime for the experiment, in real life hours.
	private int _reward; // reward for finishing the experiment in time.
	private State _state; // the experiment current state.
	private List<EquipmentPackage> _reqEquipment; // a list of required equipment package for this experiment.
	private List<Experiment> _preExperiments; // a list of pre-required experiments before this one.
	private List<Experiment> _remainingPreExperiments; // a list of remaining pre-required experiments that has to be completed before executing this one.
	
	private int _days = 0; // days that the experiment took to finish
	private long _startTime = 0; // experiment start time 
	private double _runtime = -1; // experiment total runtime
	private long _timeTookToTakeEquipment = 0; // the total time it took to take equipment from the repository
	private long _startWaitingForScientistTime = 0; // the time the experiment was sent to a laboratory
	private long _timeWaitedForScientist = -1; // the time it took until an available scientist was found for the experiment
	
	public Experiment(int id, String spec, int allowedRuntime, List<Experiment> preExperiments, List<EquipmentPackage> reqEquipment, int reward) {
		this._id = id;
		this._spec = spec;
		this._allowedRuntime = allowedRuntime;
		this._preExperiments = preExperiments;
		this._remainingPreExperiments = preExperiments;
		this._reqEquipment = reqEquipment;
		this._reward = reward;
		this._state = State.INCOMPLETE;
	}
	
	/**
	 * Used to create an experiment "holder"
	 * @param id
	 */
	public Experiment(int id) {
		this._id = id;
		this._spec = "";
		this._allowedRuntime = 0;
		this._preExperiments = null;
		this._remainingPreExperiments = null;
		this._reqEquipment = null;
		this._reward = 0;
		this._state = State.INCOMPLETE;
	}
	
	/**
	 * Reset an experiment.
	 * Used in scenarios when an experiment was interrupted.
	 */
	public void reset() {
		this._state = State.INCOMPLETE;
		this._remainingPreExperiments = _preExperiments;
		this._days = -1;
		this._startTime = 0;
		this._runtime = -1;		
		this._timeTookToTakeEquipment = 0;
		this._startWaitingForScientistTime = 0;
		this._timeWaitedForScientist = -1;
	}
	
	/**
	 * Sets the experiment in progress.
	 */
	public synchronized void init() {
		_state = State.INPROGRESS;
		
		// record the time in which the experiment was sent to the laboratory.
		_startWaitingForScientistTime = new Date().getTime();
	}
	
	/**
	 * Start the experiment by starting the runtime counter.
	 */
	public synchronized void start() {
		// save the time we waited for a scientist to process the experiment.
		_timeWaitedForScientist = (new Date().getTime() - _startWaitingForScientistTime);
		
		// start the experiment time counter.
		_startTime = new Date().getTime();
	}
	
	/**
	 * Complete experiment
	 */
	public synchronized void complete() {
		long endTime = new Date().getTime();
		_runtime = ((double)(endTime - _startTime - _days*1600) / 100);
		
		_state = State.COMPLETE;
	}
	
	/**
	 * Advance days counter by one.
	 */
	public synchronized void nextDay() {
		_days++;
	}
	
	/**
	 * Removes an experiment from the pre-required experiments by comparing IDs with the given experiment.
	 * @param exp The experiment to remove.
	 */
	public void removePreExperiment(Experiment exp) {
		ListIterator<Experiment> it = _remainingPreExperiments.listIterator();
		while (it.hasNext()) {
			Experiment preExp = it.next();
			if (preExp.getId() == exp.getId()) {
				_remainingPreExperiments.remove(it.previousIndex());
				break;
			}
		}
	}
	
	/**
	 * Checks if the given experiment is a pre-required experiment for this experiment.
	 * @param exp The experiment to check.
	 * @return true if it is, false otherwise.
	 */
	public boolean isPreExperiment(Experiment exp) {
		ListIterator<Experiment> it = _remainingPreExperiments.listIterator();
		while (it.hasNext()) {
			Experiment preExp = it.next();
			if (preExp.getId() == exp.getId()) {
				return true;
			}
		}
		
		return false;
	}	
	
	/**
	 * Checks if the experiment finished on the allowed time frame.
	 * @return true if it did, false otherwise.
	 */
	public synchronized boolean isFinishedOnTime() {
		if (_state != State.COMPLETE) {
			// the experiment isn't finished yet!
			throw new RuntimeException("Cannot retrieve total runtime of an experiment that isn't finished yet. (Experiment: " + _id + " | Runtime: " + _runtime);
		}
		
		return ((getAllowedRuntime() * 1.15) >= getRuntime());
	}
	
	public double getRuntime() throws RuntimeException {
		if (_state != State.COMPLETE) {
			// the experiment isn't finished yet!
			throw new RuntimeException("Cannot retrieve total runtime of an experiment that isn't finished yet. (Experiment: " + _id + " | Runtime: " + _runtime);
		}
		
		return _runtime;
	}	
	
	public void increaseTimeTookToTakeEquipment(long time) {
		_timeTookToTakeEquipment += time;
	}
	
	public long getTimeTookToTakeEquipment() {
		return _timeTookToTakeEquipment;
	}
	
	public long getTimeWaitedForScientist() {
		return _timeWaitedForScientist;
	}

	public boolean isComplete() {
		return (_state == State.COMPLETE);
	}	
	
	public boolean isInProgress() {
		return (_state == State.INPROGRESS);
	}
	
	public int getId() {
		return _id;
	}
	
	public int getReward() {
		return _reward;
	}
	
	public double getAllowedRuntime() {	
		return _allowedRuntime;
	}
	
	public List<Experiment> getPreExperiments() {
		return _preExperiments;
	}
	
	public List<Experiment> getRemainingPreExperiments() {
		return _remainingPreExperiments;
	}	
	
	public List<EquipmentPackage> getRequiredEquipment() {
		return _reqEquipment;
	}
	
	public String getSpec() {
		return _spec;
	}	
	
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getId()).append(" is a ").append(_spec).append(" experiment. ");
		sb.append("\n");
		sb.append("\t").append("Status: ").append(isFinishedOnTime() ? "succeeded" : "failed").append(".");
		sb.append("\n");
		sb.append("\t").append("Runtime: ").append(getRuntime()).append(" / ").append(getAllowedRuntime() * 1.15);
		sb.append("\n");
		sb.append("\t").append("Reward: $").append(isFinishedOnTime() ? getReward() : getReward()*0.1).append(".");
		sb.append("\n");
		sb.append("\t").append("Total time to take equipment: " + ((double)getTimeTookToTakeEquipment() / 1000) + "s.");
		sb.append("\n");
		sb.append("\t").append("Total time waited for a scientist: " + ((double)getTimeWaitedForScientist() / 1000) + "s.");
		
		sb.append("\n");
		sb.append("\t").append("Pre required experiments:");
		sb.append("\n");
		if (getPreExperiments().size() == 0) {
			sb.append("\t\t").append("None");
		}
		
		ListIterator<Experiment> it = getPreExperiments().listIterator();
		while (it.hasNext()) {
			Experiment exp = it.next();
			sb.append("\t\t").append(exp.getId());
		}
		
		return sb.toString();
	}
}
