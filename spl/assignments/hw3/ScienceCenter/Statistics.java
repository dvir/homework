/**
 * 
 */
package ScienceCenter;

import java.util.*;

/**
 * @author Dvir, Or
 *
 */
public class Statistics {
	private double _budget; // our ScienceStore budget.
	private List<EquipmentPackage> _purchasedEquipments; // a list of purchased equipment packages.
	private List<Scientist> _purchasedScientists; // a list of purchased scientists.
	private List<HeadOfLaboratory> _purchasedLaboratories; // a list of purchased laboratories.
	private List<Experiment> _completedExperiments; // a list of completed experiments.
	private double _moneyGainedExperiments; // sum of moeny gained from completing experiments.
	private double _moneySpent; // sum of money spent.
	private long _startTime; // our program start time.
	
	public Statistics() {
		_budget = 0;
		_moneyGainedExperiments = 0;
		_moneySpent = 0;
		_purchasedEquipments = new ArrayList<EquipmentPackage>();
		_purchasedScientists = new ArrayList<Scientist>();
		_purchasedLaboratories = new ArrayList<HeadOfLaboratory>();
		_completedExperiments = new ArrayList<Experiment>();
		_startTime = new Date().getTime();
	}
	
	/**
	 * Compute statistics for a purchased equipment package.
	 * @param ep The equipment package that was purchased.
	 */
	public synchronized void purchasedEquipmentPackage(EquipmentPackage ep) {
		_purchasedEquipments.add(ep);
		
		reduceBudget(ep.getCost());
		
		_moneySpent += ep.getCost();
	}

	/**
	 * Compute statistics for a purchased scientist.
	 * @param sc The scientist that was purchased.
	 */	
	public synchronized void purchasedScientist(Scientist sc) {
		_purchasedScientists.add(sc);
		
		reduceBudget(sc.getCost());
		
		_moneySpent += sc.getCost();
	}
	
	/**
	 * Compute statistics for a purchased laboratory.
	 * @param lab The laboratory that was purchased.
	 */	
	public synchronized void purchasedLaboratory(HeadOfLaboratory lab) {
		_purchasedLaboratories.add(lab);
		
		reduceBudget(lab.getCost());
		
		_moneySpent += lab.getCost();
	}		
	
	/**
	 * Complete experiment by granting the right reward and saving statistics for it.
	 * @param exp The completed experiment. 
	 */
	public synchronized void compeletedExperiment(Experiment exp) {
		double reward = exp.getReward(); // the reward we get for finishing an experiment
		
		// check to see if we didn't finish the experiment in time
		if (!exp.isFinishedOnTime()) {
			reward = reward * 0.1; // we get 10% of the reward if we didn't finish in 1.15*time.
		}	
		
		increaseBudget(reward);
		_moneyGainedExperiments += reward;
		_completedExperiments.add(exp);		
	}
	
	/**
	 * Decrease our ScienceCenter budget.
	 * @param amount The amount to decrease by.
	 */
	public synchronized void reduceBudget(double amount) {
		_budget -= amount;
	}
	
	/**
	 * Increase our ScienceCenter budget.
	 * @param amount The amount to increase by.
	 */
	public synchronized void increaseBudget(double amount) {
		_budget += amount;
	}
	
	public String toString() {
		StringBuilder sb = new StringBuilder();		
		
		sb.append("Scientists Purchased:").append("\n");
		ListIterator<Scientist> it_scientist = _purchasedScientists.listIterator();
		while (it_scientist.hasNext()) {
			sb.append(it_scientist.next().toString()).append("\n");
		}
		
		sb.append("EquipmentPackages Purchased:\n");
		ListIterator<EquipmentPackage> it_ep = _purchasedEquipments.listIterator();
		while (it_ep.hasNext()) {
			sb.append(it_ep.next().toString()).append("\n");
		}
		
		sb.append("Laboratories Purchased:\n");
		ListIterator<HeadOfLaboratory> it_laboratory = _purchasedLaboratories.listIterator();
		while (it_laboratory.hasNext()) {
			sb.append(it_laboratory.next().toString()).append("\n");
		}
		
		int failedExperimentsCount = 0;
		long totalTimeToTakeEquipment = 0;
		long totalTimeWaitingForScientist = 0;
		sb.append("Completed Experiments (").append(_completedExperiments.size()).append("):\n");
		ListIterator<Experiment> it_exp = _completedExperiments.listIterator();
		while (it_exp.hasNext()) {
			Experiment exp = it_exp.next();
			if (!exp.isFinishedOnTime()) {
				failedExperimentsCount++;
			}
			
			totalTimeToTakeEquipment += exp.getTimeTookToTakeEquipment();
			totalTimeWaitingForScientist += exp.getTimeWaitedForScientist();
			
			sb.append(exp.toString()).append("\n");
		}				
		
		sb.append("Budget: ").append(_budget).append("\n");
		sb.append("Money Gained: ").append(_moneyGainedExperiments).append("\n");
		sb.append("Money Spent: ").append(_moneySpent).append("\n");
		sb.append("Failed Experiments: ").append(failedExperimentsCount).append("\n");
		sb.append("Total time to take equipment: ").append( ((double)totalTimeToTakeEquipment / 1000)).append(" | Average: ").append(((double)totalTimeToTakeEquipment / 1000 / _completedExperiments.size())).append("\n");
		sb.append("Total time waiting for a scientist: ").append((double)totalTimeWaitingForScientist / 1000).append(" | Average: ").append((double)totalTimeWaitingForScientist / 1000 / _completedExperiments.size()).append("\n");
		sb.append("Total Runtime: ").append((double)(new Date().getTime() - _startTime) / 1000).append("s\n");
		
		return sb.toString();
	}
}
