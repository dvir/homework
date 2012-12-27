/**
 * 
 */
package ScienceCenter;

import java.util.*;

/**
 * @author Dvir, Or
 *
 */
public class ChiefScientistAssistant {
	private static ChiefScientist _chiefScientist;
	private static List<Experiment> _pendingExperiments = new ArrayList<Experiment>();
	
	public static void setChiefScientist(ChiefScientist chiefScientist) {
		_chiefScientist = chiefScientist;
	}
	
	public static void init() throws RuntimeException {
		if (_chiefScientist == null) {
			// we didn't receive any ChiefScientist yet.
			throw new RuntimeException("ChiefScientistAssistant can't start until the ChiefScientist has arrived.");
		}
		
		// scan experiments list for new experiments to execute.
		scanExperiments(null);
	}
	
	public static void finishedExperiment(Experiment exp) {
		double reward = exp.getReward(); // the reward we get for finishing an experiment
		
		// check to see if we didn't finish the experiment in time
		if (exp.getAllowedRuntime() * 1.15 < exp.getRuntime()) {
			reward = reward * 0.1; // we get 10% of the reward if we didn't finish in 1.1*time.
		}
		
		_chiefScientist.increaseBudget(reward);
		
System.out.println("Finished experiment " + exp.getId() + " and got " + reward + " / " + exp.getReward() + " as a reward. Runtime: " + exp.getRuntime());
		
		// check if we can queue new experiments that relied upon this experiment
		scanExperiments(exp);
	}
	
	/**
	 * Scans the unfinished experiments list for new experiments to execute.
	 * If the given experiment isn't null, try only experiments that listed it as a pre-requirement.
	 * @param completedExperiment The experiment that we just finished. null if we should just go over the whole list.
	 */
	public static void scanExperiments(Experiment completedExperiment) {
		// go over the incomplete experiments and look for ones without pre-requirements
		List<Experiment> experiments = _chiefScientist.getIncompleteExperiments();
		ListIterator<Experiment> it = experiments.listIterator();
		
		while (it.hasNext()) {
			Experiment exp = it.next();
			List<Experiment> preExperiments = exp.getPreExperiments();
			
			// if we got an experiment as input, check if it's a pre-requirement for this experiment, and if so remove it.
			if (completedExperiment != null) {
				exp.removePreExperiment(completedExperiment);
			}
			
			if (preExperiments.size() == 0) {
				// we don't have any pre-required experiments for this experiment!
				// add it to the pending experiments list
				_pendingExperiments.add(exp);
				
				// remove it from the incomplete experiments list.
				it.remove();				
			}
		}
		
		executePendingExperiments();
	}
	
	public static void executePendingExperiments() {
		// go over pending experiments and try to execute any of them that isn't currently ongoing
		ListIterator<Experiment> it = _pendingExperiments.listIterator();
		while (it.hasNext()) {
			Experiment exp = it.next();

			// any experiment from the pending experiments list is guaranteed to
			// have all its pre-required experiments done, so we need to find a laboratory
			// to run the experiment at.
			
			// find a laboratory to run it in
			HeadOfLaboratory lab = _chiefScientist.findLaboratory(exp.getSpec());
			
			// remove it from pending experiments list
			it.remove();
			
			// execute it!
			lab.runExperiment(exp, _chiefScientist);
		}		
	}
	
	public static void takeEquipment(List<EquipmentPackage> equipments) throws InterruptedException {
// try acquire
//		boolean hasEquipment = true;
//		ListIterator<EquipmentPackage> it_equipment = equipments.listIterator();
//		while (it_equipment.hasNext()) {
//			EquipmentPackage requiredEquipment = it_equipment.next();
//			EquipmentPackage repositoryEquipmentPackage = _chiefScientist.searchRepository(requiredEquipment.getName());
//			
//			if (!repositoryEquipmentPackage.tryTakeAmount(requiredEquipment.getAmount())) {
//				// can't acquire enough of this equipment type.
//				// don't execute this experiment, and release any equipment we already took from the repository.
//				hasEquipment = false;
//				break;
//			}
//		}
//		
//		if (!hasEquipment) {
//			// we couldn't secure all the equipment needed to run this experiment.
//			// release any equipment we already took from the repository by
//			// rolling back the acquiring loop.
//			while (it_equipment.hasPrevious()) {
//				EquipmentPackage requiredEquipment = it_equipment.previous();
//				EquipmentPackage repositoryEquipmentPackage = _chiefScientist.searchRepository(requiredEquipment.getName());
//				
//				repositoryEquipmentPackage.returnAmount(requiredEquipment.getAmount());
//			}
//			
//			continue;
//		}
		
		// try to acquire equipment.
		// NOTE: BLOCKS UNTIL ALL EQUIPMENT IS AVAILABLE.
		
		ListIterator<EquipmentPackage> it_equipment = equipments.listIterator();
		while (it_equipment.hasNext()) {
			EquipmentPackage requiredEquipment = it_equipment.next();
			EquipmentPackage repositoryEquipmentPackage = findEquipment(requiredEquipment.getName(), requiredEquipment.getAmount());
			
			repositoryEquipmentPackage.takeAmount(requiredEquipment.getAmount());
		}
	}
	
	/**
	 * Return equipments to the repository.
	 * @param equipments The equipmentPackages to return.
	 */
	public static void releaseEquipment(List<EquipmentPackage> equipments) {
		ListIterator<EquipmentPackage> it_equipment = equipments.listIterator();
		while (it_equipment.hasNext()) {
			EquipmentPackage requiredEquipment = it_equipment.next();
			EquipmentPackage repositoryEquipmentPackage = findEquipment(requiredEquipment.getName());
			
			repositoryEquipmentPackage.returnAmount(requiredEquipment.getAmount());
		}
	}	
	
	/**
	 * Proxy for findEquipment(String name, int amount).
	 * Passes 0 as the amount.
	 * @param name The equipment name.
	 * @return ep The equipment package of the requested name.
	 */
	public static EquipmentPackage findEquipment(String name) {
		return findEquipment(name, 0);
	}
	
	public static EquipmentPackage findEquipment(String name, int amount) {
		return new EquipmentPackage(name, amount, 0);
		
		// @todo: FIX THIS!
//		EquipmentPackage ep = _chiefScientist.searchRepository(name);
//		if (ep == null) {
//			// we don't have any equipment named @param name.
//			// purchase from the store and then retry.
//			buyEquipmentPackage(name, amount);
//			
//			// since _chiefScientist.purchase will add the equipment to the repository, we are guaranteed to find it this time.
//			return findEquipment(name, amount);
//		}
//		
//		if (ep.getAmount() < amount) {
//			// we don't have enough of equipment named @param name!
//			// purchase more from the store.
//			buyEquipmentPackage(name, amount - ep.getAmount());
//			
//			return findEquipment(name, amount);
//		}
//		
//		// if we got here, we got the equipment package we wanted with enough amount of items.
//		return ep;
	}
	
	public static void buyEquipmentPackage(String name, int amount) {
		// find the relevant equipment packages in the store
		List<EquipmentPackage> relevantEquipments = new ArrayList<EquipmentPackage>();
		
		List<EquipmentPackage> equipments = _chiefScientist.getScienceStore().getEquipmentPackages();
		ListIterator<EquipmentPackage> it = equipments.listIterator();
		while (it.hasNext()) {
			EquipmentPackage ep = it.next();
			if (ep.getName() == name) {
				// @todo: add by some sort of order? (price per item, price per item & amount needed, etc.)
				relevantEquipments.add(ep);
			}
		}
		
		// we have all the equipment packages of the type we need in relevantEquipments. Figure which to buy.
		it = relevantEquipments.listIterator();
		while (it.hasNext()) {
			EquipmentPackage ep = it.next();
			
			// @todo: CHANGE THIS! currently, we just purchasing everything of this type.
			_chiefScientist.getScienceStore().buyEquipmentPackage(ep);
		}
	}
}
