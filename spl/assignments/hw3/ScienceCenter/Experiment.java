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
	private List<Experiment> _preExperiments;
	private List<EquipmentPackage> _reqEquipment;
	private int _reward;
	private State _state;
	
	private int _days; // Days that the experiment took to finish
	private long _startTime;
	private int _runtime;	
	
	public Experiment(int id, String spec, int allowedRuntime, List<Experiment> preExperiments, List<EquipmentPackage> reqEquipment, int reward) {
		this._id = id;
		this._spec = spec;
		this._allowedRuntime = allowedRuntime;
		this._preExperiments = preExperiments;
		this._reqEquipment = reqEquipment;
		this._reward = reward;
		this._state = State.INCOMPLETE;
		
		this._days = 0;
		this._startTime = 0;
		this._runtime = -1;		
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
		this._reqEquipment = null;
		this._reward = 0;
		this._state = State.INCOMPLETE;
		
		this._days = 0;
		this._startTime = 0;
		this._runtime = -1;
	}
	
	/**
	 * Reset an experiment.
	 * Used in scenarios when an experiment was interrupted.
	 */
	public void reset() {
		this._state = State.INCOMPLETE;
		this._days = 0;
		this._startTime = 0;
		this._runtime = -1;		
	}
	
	/**
	 * Start the experiment by starting the runtime counter and setting the state to State.INPROGRESS.
	 */
	public void start() {	
		_startTime = new Date().getTime();
		
		_state = State.INPROGRESS;
	}
	
	public void complete() {
		long endTime = new Date().getTime();
		_runtime = (int)((endTime - _startTime - _days*1600) / 100);
		
		_state = State.COMPLETE;
	}
	
	/**
	 * Advance days counter by one.
	 */
	public void nextDay() {
System.out.println("Experiment " + _id + " starting day " + (_days+1) + ".");
		_days++;
	}
	
	public int getId() {
		return _id;
	}
	
	public int getReward() {
		return _reward;
	}
	
	public int getAllowedRuntime() {
		return _allowedRuntime;
	}
	
	public int getRuntime() throws RuntimeException {
		if (_state != State.COMPLETE) {
			// the experiment isn't finished yet!
			throw new RuntimeException("Cannot retrieve total runtime of an experiment that isn't finished yet.");
		}
		
		return _runtime;
	}
	
	public List<Experiment> getPreExperiments() {
		return _preExperiments;
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
		ListIterator<Experiment> it = _preExperiments.listIterator();
		while (it.hasNext()) {
			Experiment preExp = it.next();
			if (preExp.getId() == exp.getId()) {
				_preExperiments.remove(it.previousIndex());
				break;
			}
		}
	}
}
