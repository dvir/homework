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
	private long _runtime;
	private List _preExperiments;
	private List _reqEquipment;
	private int _reward;
	private State _state;
	
	public Experiment(int id, String spec, int runtime, List preExperiments, List reqEquipment, int reward) {
		this._id = id;
		this._spec = spec;
		this._runtime = runtime;
		this._preExperiments = preExperiments;
		this._reqEquipment = reqEquipment;
		this._reward = reward;
		this._state = State.INCOMPLETE;
	}
}
