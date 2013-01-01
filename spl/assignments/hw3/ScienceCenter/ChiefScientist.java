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
	private List<HeadOfLaboratory> _labs; // laboratories currently in the science center
	private List<Experiment> _experiments; // all the experiments we should execute
	private List<Experiment> _incompleteExperiments; // the experiments that we still didn't execute
	private ScienceStore _store; // the science store selling equipment, scientists, laboratories
	private Statistics _stats; // the statistics object of our science center, holding all the interesting data of our program execution
	private Repository _repository; // the repository holding the equipment we have in possession
	
	public ChiefScientist(List<HeadOfLaboratory> labs, List<Experiment> experiments, ScienceStore store, Statistics stats, Repository repository) {
		_labs = labs;
		_experiments = experiments;
		_incompleteExperiments = experiments;
		_store = store;
		_stats = stats;
		_repository = repository;
	}
	
	/**
	 * Update method which is executed by .notify() when a RunnableExperiment finishes.
	 * @param o The RunnableExperiment that has finished.
	 */
	public void update(Observable o) {
		RunnableExperiment re = ((RunnableExperiment) o);
		ChiefScientistAssistant.finishedExperiment(re.getExperiment());
	}
	
	public void update(Observable o, Object arg) {
		this.update(o);
	}	
	
	/**
	 * Remove an experiment from the list of incomplete experiments.
	 * @param exp The experiment to remove.
	 */
	public void removeIncompleteExperiment(Experiment exp) {
		_incompleteExperiments.remove(exp);
	}	
	
	/**
	 * Search the repository for a specific equipment package.
	 * @param name The name of the equipment package to look for.
	 * @return The equipment package matching @param name, null if it wasn't found.
	 */
	public EquipmentPackage searchRepository(String name) {
		return _repository.searchRepository(name);
	}
	
	/**
	 * Add a laboratory to our array of laboratories.
	 * @param lab The laboratory to add.
	 */
	public void addLaboratory(HeadOfLaboratory lab) {
		_labs.add(lab);
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
	
	/**
	 * Shutdown our ScienceCenter by telling each laboratory to shutdown.
	 */
	public void shutdownScienceCenter() {
		ListIterator<HeadOfLaboratory> it_lab = _labs.listIterator();
		while (it_lab.hasNext()) {
			it_lab.next().shutdownLab();
		}
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
	
	public List<Experiment> getExperiments() {
		return _experiments;
	}
	
	public List<Experiment> getIncompleteExperiments() {
		return _incompleteExperiments;
	}	
}
