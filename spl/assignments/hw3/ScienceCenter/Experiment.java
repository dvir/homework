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
	private enum State {
		INCOMPLETE,
		INPROGRESS,
		COMPLETE
	};	
	
	private int _id;
	private String _spec;
	private int _allowedRuntime;
	private List<Experiment> _remainingPreExperiments;
	private List<Experiment> _preExperiments;
	private List<EquipmentPackage> _reqEquipment;
	private int _reward;
	private State _state;
	
	private int _days = 0; // Days that the experiment took to finish
	private long _startTime = 0;
	private double _runtime = -1;
	private long _timeTookToTakeEquipment = 0;
	private long _startWaitingForScientistTime = 0;
	private long _timeWaitedForScientist = -1;
	
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
	public void init() {
		_state = State.INPROGRESS;
		_startWaitingForScientistTime = new Date().getTime();
	}
	
	/**
	 * Start the experiment by starting the runtime counter and setting the state to State.INPROGRESS.
	 */
	public synchronized void start() {
		_timeWaitedForScientist = (new Date().getTime() - _startWaitingForScientistTime);
		_startTime = new Date().getTime();
	}
	
	public synchronized void complete() {
		long endTime = new Date().getTime();
		_runtime = ((double)(endTime - _startTime - _days*1600) / 100);
		
		_state = State.COMPLETE;
	}
	
	/**
	 * Advance days counter by one.
	 */
	public void nextDay() {
		_days++;
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
	
	public double getRuntime() throws RuntimeException {
		if (_state != State.COMPLETE) {
			// the experiment isn't finished yet!
			throw new RuntimeException("Cannot retrieve total runtime of an experiment that isn't finished yet. (Experiment: " + _id + " | Runtime: " + _runtime);
		}
		
		return _runtime;
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
	
	public synchronized boolean isFinishedOnTime() {
		if (_state != State.COMPLETE) {
			// the experiment isn't finished yet!
			throw new RuntimeException("Cannot retrieve total runtime of an experiment that isn't finished yet. (Experiment: " + _id + " | Runtime: " + _runtime);
		}
		
		return ((getAllowedRuntime() * 1.15) >= getRuntime());
	}
	
	public boolean isInProgress() {
		return (_state == State.INPROGRESS);
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
}
