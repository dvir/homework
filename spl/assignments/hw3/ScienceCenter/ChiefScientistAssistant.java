/**
 * 
 */
package ScienceCenter;

import java.util.*;

/**
 * @author Dvir, Or
 *
 */
public class ChiefScientistAssistant implements Runnable {
	private ChiefScientist _chiefScientist;
	private List<Experiment> _pendingExperiments = new ArrayList<Experiment>();
	private static ChiefScientistAssistant _instance = null;
	
	public static ChiefScientistAssistant getInstance() {
		if (_instance == null) {
			_instance = new ChiefScientistAssistant();
		}
		
		return _instance;
	}
	
	public static void setChiefScientist(ChiefScientist chiefScientist) {
		getInstance().setChiefScientistInstance(chiefScientist);
	}
	
	public void setChiefScientistInstance(ChiefScientist chiefScientist) {
		_chiefScientist = chiefScientist;
	}
	
	public synchronized void run() throws RuntimeException {
		if (_chiefScientist == null) {
			// we didn't receive any ChiefScientist yet.
			throw new RuntimeException("ChiefScientistAssistant can't start until the ChiefScientist has arrived.");
		}
	
		optimize();
		
		// scan experiments list for new experiments to execute.
		scanExperiments(null);
		
		while (true) {
			executePendingExperiments();
			
			try {
				wait();
			} catch (InterruptedException e) {
				// nothing to do :(
				throw new RuntimeException("ChiefScientistAssistant was interrupted.");
			}
		}
	}

	private void optimize() {
		// this list will hold the equipment amounts we need so no experiment will -have- to purchase any. 
		// (though we might purchase some to optimize results)
		List<EquipmentPackage> minimalEquipmentNeeded = new ArrayList<EquipmentPackage>();
		List<Experiment> experiments = _chiefScientist.getExperiments();
		ListIterator<Experiment> it_exp = experiments.listIterator();
		while (it_exp.hasNext()) {
			Experiment exp = it_exp.next();
			List<EquipmentPackage> reqEquipments = exp.getRequiredEquipment();
			
			// for each experiment, we go over the equipment needed and add it to the minimalEquipmentNeeded array, 
			// with an amount that is max{reqEp.getAmount(), ep.getAmount()}
			// so we will have the minimal set of amounts of items we must have in the repository.
			ListIterator<EquipmentPackage> it_reqEp = reqEquipments.listIterator();
			while (it_reqEp.hasNext()) {
				EquipmentPackage reqEp = it_reqEp.next();
				
				// find it in the minimalEquipmentNeeded array
				boolean found = false;
				ListIterator<EquipmentPackage> it_ep = minimalEquipmentNeeded.listIterator();
				while (it_ep.hasNext()) {
					EquipmentPackage ep = it_ep.next();
					if (ep.getName().compareTo(reqEp.getName()) == 0) {
						// found it! increase amount by max{ max{reqEp.getAmount(), ep.getAmount()} - currentAmount), 0}  
						ep.increaseAmount(Math.max(0, Math.max(ep.getAmount(), reqEp.getAmount()) - ep.getAmount()));
						found = true;
						break;
					}
				}
				
				if (!found) {
					// create a new ep with the amount we currently have
					EquipmentPackage newMinimalEp = new EquipmentPackage(reqEp);
					minimalEquipmentNeeded.add(newMinimalEp);
				}
			}
		}
		
		// for each ep in the minimalEquipmentNeeded list, make sure we have at least ep.getAmount() in the repository.
		// buy the difference if needed.
		// find it in the minimalEquipmentNeeded array
		ListIterator<EquipmentPackage> it_ep = minimalEquipmentNeeded.listIterator();
		while (it_ep.hasNext()) {
			EquipmentPackage ep = it_ep.next();
			findEquipment(ep);
		}
	}
	
	public static void finishedExperiment(Experiment exp) {
		synchronized (getInstance()) {
			getInstance().completedExperiment(exp);
			getInstance().notify();
		}
	}
	
	public void completedExperiment(Experiment exp) {		
		_chiefScientist.getStats().compeletedExperiment(exp);
		
		// remove it from pendingExperiments
		ListIterator<Experiment> it = _pendingExperiments.listIterator();
		while (it.hasNext()) {
			if (it.next().getId() == exp.getId()) {
				it.remove();
				break;
			}
		}
		
		if (_chiefScientist.getIncompleteExperiments().size() == 0 && _pendingExperiments.size() == 0) {
			// no more experiments to be held. output statistics and quit!
			printStats();
			System.exit(0);
		}
		
		// check if we can queue new experiments that relied upon this experiment
		scanExperiments(exp);
	}
	
	/**
	 * Scans the unfinished experiments list for new experiments to execute.
	 * If the given experiment isn't null, try only experiments that listed it as a pre-requirement.
	 * @param completedExperiment The experiment that we just finished. null if we should just go over the whole list.
	 * 
	 */
	public synchronized void scanExperiments(Experiment completedExperiment) {
		// go over the incomplete experiments and look for ones without pre-requirements
		List<Experiment> experiments = _chiefScientist.getIncompleteExperiments();
		ListIterator<Experiment> it = experiments.listIterator();
		
		while (it.hasNext()) {
			Experiment exp = it.next();
			
			List<Experiment> preExperiments = exp.getRemainingPreExperiments();
			
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
	}
	
	public void executePendingExperiments() {
		// go over pending experiments and try to execute any of them that isn't currently ongoing
		ListIterator<Experiment> it = _pendingExperiments.listIterator();
		while (it.hasNext()) {
			Experiment exp = it.next();
			
			// skip experiments that are already in progress
			if (exp.isInProgress()) {
				continue;
			}
			
			if (exp.isComplete()) {
				//throw new RuntimeException("Experiment " + exp.getId() + " is already finished!");
				it.remove();
				continue;
			}

			// any experiment from the pending experiments list is guaranteed to
			// have all its pre-required experiments done, so we need to find a laboratory
			// to run the experiment at.
			
			// find a laboratory to run it in
			HeadOfLaboratory lab = findLaboratory(exp.getSpec());
			
			// execute it!
			lab.runExperiment(exp, _chiefScientist);
		}		
	}
	
	public static void takeEquipment(List<EquipmentPackage> equipments) throws InterruptedException {
		getInstance().getEquipment(equipments);
	}
	
	public void getEquipment(List<EquipmentPackage> equipments) throws InterruptedException {
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
		getInstance().returnEquipment(equipments);
	}
	
	public void returnEquipment(List<EquipmentPackage> equipments) {
		ListIterator<EquipmentPackage> it_equipment = equipments.listIterator();
		while (it_equipment.hasNext()) {
			EquipmentPackage requiredEquipment = it_equipment.next();
			EquipmentPackage repositoryEquipmentPackage = findEquipment(requiredEquipment.getName());
			
			repositoryEquipmentPackage.returnAmount(requiredEquipment.getAmount());
		}
	}	
	
	public EquipmentPackage findEquipment(String name, int amount) {
		EquipmentPackage ep = _chiefScientist.searchRepository(name);
		if (ep == null) {
			// we don't have any equipment named @param name.
			// purchase from the store and then retry.
			buyEquipmentPackage(name, amount);
			
			// since _chiefScientist.purchase will add the equipment to the repository, we are guaranteed to find it this time.
			return findEquipment(name, amount);
		}
		
		if (ep.getAmount() < amount) {
			// we don't have enough of equipment named @param name!
			// purchase more from the store.
			buyEquipmentPackage(name, amount - ep.getAmount());
			
			return findEquipment(name, amount - ep.getAmount());
		}
		
		// if we got here, we got the equipment package we wanted with enough amount of items.
		return ep;
	}
	
	/**
	 * Proxy for findEquipment(String name, int amount).
	 * @param ep The equipment package.
	 * @return ep The relevant equipment package with same name and at least the requested amount.
	 */
	public EquipmentPackage findEquipment(EquipmentPackage ep) {
		return findEquipment(ep.getName(), ep.getAmount());
	}	
	
	/**
	 * Proxy for findEquipment(String name, int amount).
	 * Passes 0 as the amount.
	 * @param name The equipment name.
	 * @return ep The equipment package of the requested name.
	 */
	public EquipmentPackage findEquipment(String name) {
		return findEquipment(name, 0);
	}	
	
	public void buyEquipmentPackage(String name, int amount) {
		// find the relevant equipment packages in the store
		List<EquipmentPackage> relevantEquipments = new ArrayList<EquipmentPackage>();
		
		List<EquipmentPackage> equipments = _chiefScientist.getScienceStore().getEquipmentPackages();

		for (int j = 0; j < equipments.size(); ++j) {
			EquipmentPackage ep = equipments.get(j);
			
			if (ep.getName().compareTo(name) == 0) {
				// sort relevantEquipments by {lex, cost per item, distance from amount needed}
				int i = 0;
				while (i < relevantEquipments.size()) {
					EquipmentPackage curr = relevantEquipments.get(i);
					
					if (curr.getName().compareTo(ep.getName()) > 0) {
						// lexicographically smaller then current item. stop and insert it before it.
						break;
					} else if (curr.getName().compareTo(ep.getName()) == 0) {
						// lexicographically equal to current, compare if amount is over what we need and how close the amount is to what we need.
						if ((ep.getAmount() - amount) > 0 && Math.abs(curr.getAmount() - amount) > Math.abs(ep.getAmount() - amount))  {
							// closer to the amount we need. put it before.
							break;
						}
					}
					i++;
				}
				
				relevantEquipments.add(i, ep);
			}
		}
		
		// we have all the equipment packages of the type we need in relevantEquipments. Figure which to buy.
		ListIterator<EquipmentPackage> it = relevantEquipments.listIterator();
		while (it.hasNext() && amount > 0) {
			EquipmentPackage ep = it.next();
			
			// if ScienceStore.buyEquipmentPackage returns true, we should add the package to the repository.
			// else, it was already purchased and handled by someone else.
			if (_chiefScientist.getScienceStore().buyEquipmentPackage(ep)) {
				_chiefScientist.getRepository().addEquipmentPackage(ep);
			}
			
			// reduce the amount we purchased from the amount we need 
			// so we can stop when we have enough (or a little bit more)
			amount -= ep.getAmount();
		}
	}
	
	public HeadOfLaboratory findLaboratory(String spec) {
		HeadOfLaboratory lab = _chiefScientist.findLaboratory(spec);
		if (lab == null) {
			// we don't have any lab of spec @param spec.
			// purchase from the store and then retry.
			buyLaboratory(spec);
			
			// since _chiefScientist.purchase will add the laboratory, we are guaranteed to find it this time.
			return findLaboratory(spec);
		}
		
		// if we got here, we got the lab with the spec we wanted.
		return lab;
	}
	
	public void buyLaboratory(String spec) {
		// find the relevant equipment packages in the store
		List<HeadOfLaboratory> relevantLaboratories = new ArrayList<HeadOfLaboratory>();
		
		List<HeadOfLaboratory> labs = _chiefScientist.getScienceStore().getLaboratories();
		ListIterator<HeadOfLaboratory> it = labs.listIterator();
		while (it.hasNext()) {
			HeadOfLaboratory lab = it.next();
			if (lab.getSpec().compareTo(spec) == 0) {
				// sorting by {cost per scientist, lowest cost}
				int i = 0;
				while (i < relevantLaboratories.size()) {
					HeadOfLaboratory curr = relevantLaboratories.get(i);

					// compare cost per scientist and lowest cost
					if (curr.getCostPerScientist() > lab.getCostPerScientist() || curr.getCost() > lab.getCost())  {
						// cheaper per scientist. put it before.
						break;
					}
					i++;
				}
				
				relevantLaboratories.add(i, lab);	
			}
		}
		
		// we have all the laboratories of the spec we need in relevantLaboratories. Figure which to buy.
		it = relevantLaboratories.listIterator();
		while (it.hasNext()) {
			HeadOfLaboratory lab = it.next();
			
			// if ScienceStore.buyEquipmentPackage returns true, we should add the package to the repository.
			// else, it was already purchased and handled by someone else.
			if (_chiefScientist.getScienceStore().buyLaboratory(lab)) {
				_chiefScientist.addLaboratory(lab);
			}
			
			break;
		}
	}	
	
	public static void purchasedEquipmentPackage(EquipmentPackage ep) {
		getInstance().getStats().purchasedEquipmentPackage(ep);
	}
	
	public static void purchasedScientist(Scientist sc) {
		getInstance().getStats().purchasedScientist(sc);
	}
	
	public static void purchasedLaboratory(HeadOfLaboratory lab) {
		getInstance().getStats().purchasedLaboratory(lab);
	}	
	
	public void increaseBudget(int amount) {
		_chiefScientist.increaseBudget(amount);
	}
	
	public void reduceBudget(int amount) {
		_chiefScientist.reduceBudget(amount);
	}
	
	public Statistics getStats() {
		return _chiefScientist.getStats();
	}
	
	public static void printStats() {
		System.out.println(getInstance().getStats());
	}
}
