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
	private List<HeadOfLaboratory> _labs;
	private List<Experiment> _experiments;
	private List<Experiment> _incompleteExperiments;
	private ScienceStore _store;
	private Statistics _stats;
	private Repository _repository;
	
	public ChiefScientist(List<HeadOfLaboratory> labs, List<Experiment> experiments, ScienceStore store, Statistics stats, Repository repository) {
		_labs = labs;
		_experiments = experiments;
		_incompleteExperiments = experiments;
		_store = store;
		_stats = stats;
		_repository = repository;
	}

	public void update(Observable o, Object arg) {
		this.update(o);
	}
	
	public void update(Observable o) {
		RunnableExperiment re = ((RunnableExperiment) o);
		ChiefScientistAssistant.finishedExperiment(re.getExperiment());
	}
	
	public List<Experiment> getExperiments() {
		return _experiments;
	}
	
	public List<Experiment> getIncompleteExperiments() {
		return _incompleteExperiments;
	}
	
	public void removeIncompleteExperiment(Experiment e) {
		_incompleteExperiments.remove(e);
	}
	
	public void reduceBudget(double amount) {
		_stats.reduceBudget(amount);
	}
	
	public void increaseBudget(double amount) {
		_stats.increaseBudget(amount);
	}	
	
	public EquipmentPackage searchRepository(String name) {
		return _repository.searchRepository(name);
	}
	
	/**
	 * Finds a laboratory with the specified spec.
	 * If none is found, we will purchase it from the store.
	 * @param spec The spec name
	 * @return A laboratory with the given @param spec.
	 */
	public HeadOfLaboratory findLaboratory(String spec) {
		ListIterator<HeadOfLaboratory> it = _labs.listIterator();
		while (it.hasNext()) {
			HeadOfLaboratory lab = it.next();
			if (lab.getSpec().compareTo(spec) == 0) {
				// we found it!
				return lab;
			}
		}
		
		// if we got here, we couldn't find a lab with the given spec.
		return null;
	}
	
	public void addLaboratory(HeadOfLaboratory lab) {
		_labs.add(lab);
	}
	
	public ScienceStore getScienceStore() {
		return _store;
	}
	
	public Repository getRepository() {
		return _repository;
	}
	
	public Statistics getStats() {
		return _stats;
	}
}
