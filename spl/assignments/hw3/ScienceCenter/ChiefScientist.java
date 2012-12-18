/**
 * 
 */
package ScienceCenter;

import java.util.*;
import java.util.Observable;
import java.util.Observer;

/**
 * @author Dvir, Or
 *
 */
public class ChiefScientist implements Observer {
	private List<HeadOfLaboratory> _laboratories;
	private List<Experiment> _experiments;
	private ScienceStore _store;
	private Statistics _stats;
	private Repository _repository;

	public void update(Observable o, Object arg) {
		this.update(o);
	}
	
	public void update(Observable o) {
		RunnableExperiment re = ((RunnableExperiment) o);
		ChiefScientistAssistant.finishedExperiment(re.getExperiment(), re.getRuntime());
	}
}
