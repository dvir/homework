/**
 * 
 */
package ScienceCenter;

import java.util.*;

/**
 * A singleton.
 * 
 * @author Dvir, Or
 *
 */
public class ChiefScientistAssistant implements Runnable {
	/**
	 * the ChiefScientistAssistant instance.
	 */
	private static ChiefScientistAssistant _instance = null;
	
	/**
	 * the chief scientist object instance.
	 */
	private ChiefScientist _chiefScientist; 
	
	/**
	 * a list of experiments that have no pre-requirements and are waiting execution.
	 */
	private List<Experiment> _pendingExperiments = new ArrayList<Experiment>(); 
	
	/**
	 * Get the current ChiefScientistAssistant instance.
	 * If it doesn't exist, create it first.
	 * @return The instance of our singleton.
	 */
	public static ChiefScientistAssistant getInstance() {
		if (_instance == null) {
			_instance = new ChiefScientistAssistant();
		}
		
		return _instance;
	}
	
	/**
	 * Set the ChiefScientist object instance in the ChiefScientistAssistant object.
	 * @param chiefScientist The chief scientist instance.
	 */
	public static void setChiefScientist(ChiefScientist chiefScientist) {
		getInstance().setChiefScientistInstance(chiefScientist);
	}
	
	/**
	 * Set the current ChiefScientist instance.
	 * @param chiefScientist
	 */
	public void setChiefScientistInstance(ChiefScientist chiefScientist) {
		_chiefScientist = chiefScientist;
	}
	
	public synchronized void run() throws RuntimeException {
		if (_chiefScientist == null) {
			// we didn't receive any ChiefScientist yet.
			throw new RuntimeException("ChiefScientistAssistant can't start until the ChiefScientist has arrived.");
		}
	
		// run optimizations
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

	/**
	 * Run optimizations.
	 * * Calculate the minimum amount of equipment needed for all the experiments to run and purchase them before-hand. 
	 */
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
	
	/**
	 * We finished running an experiment. Complete it and notify the ChiefScientistAssistant that it's done.
	 * @param exp The experiment we finished executing.
	 */
	public static void finishedExperiment(Experiment exp) {
		synchronized (getInstance()) {
			getInstance().completedExperiment(exp);
			getInstance().notify();
		}
	}
	
	/**
	 * React to the event where an experiment finished by:
	 *   - Reporting it to statistics object.
	 *   - Removing it from pending to be executed experiments list.
	 *   - Check if all of the experiments finished, and if so initiate shutdown procedures.
	 *   - Look for new experiments to execute that relied on this experiment.
	 * @param exp
	 */
	public synchronized void completedExperiment(Experiment exp) {		
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
			_chiefScientist.shutdownScienceCenter();
			Thread.currentThread().interrupt();
			System.exit(0);
			return;
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
	
	/**
	 * Execute experiments that are pending execution - don't have any pre-requirements.
	 */
	public synchronized void executePendingExperiments() {
		// go over pending experiments and try to execute any of them that isn't currently ongoing
		ListIterator<Experiment> it = _pendingExperiments.listIterator();
		while (it.hasNext()) {
			Experiment exp = it.next();
			
			// skip experiments that are already in progress
			if (exp.isInProgress()) {
				continue;
			}
			
			if (exp.isComplete()) {
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
	
	/**
	 * Reserve equipment for an experiment.
	 * @param equipments List of equipment packages to reserve.
	 * @throws InterruptedException
	 */
	public static void takeEquipment(List<EquipmentPackage> equipments) throws InterruptedException {
		getInstance().getEquipment(equipments);
	}

	/**
	 * Reserve equipment for an experiment.
	 * @param equipments List of equipment packages to reserve.
	 * @throws InterruptedException
	 */
	public void getEquipment(List<EquipmentPackage> equipments) throws InterruptedException {
		// make a list of equipment we need from the repository.
		List<EquipmentPackage> requiredRepositoryEquipments = new ArrayList<EquipmentPackage>();
		
		ListIterator<EquipmentPackage> it_equipment = equipments.listIterator();
		while (it_equipment.hasNext()) {
			EquipmentPackage requiredEquipment = it_equipment.next();
			EquipmentPackage repositoryEquipmentPackage = findEquipment(requiredEquipment.getName(), requiredEquipment.getAmount());
			requiredRepositoryEquipments.add(repositoryEquipmentPackage);
		}
		
		// try and take all the required equipment.
		// if one of the equipment packages is missing, we return all the equipment we previously took
		// and wait for some equipment to return before trying again.
		while (true) {
			boolean took_everything = true;
			it_equipment = equipments.listIterator();
			while (it_equipment.hasNext()) {
				EquipmentPackage requiredEquipment = it_equipment.next();
				EquipmentPackage ep = findEquipmentInList(requiredRepositoryEquipments, requiredEquipment);
				
				// check if we can take the amount we need from this equipment package.
				if (!ep.tryTakeAmount(requiredEquipment.getAmount())) {
					// not enough items in the equipment package.
					// return the equipment we already took.
					it_equipment.previous();
					while (it_equipment.hasPrevious()) {
						// find it in the repository array
						EquipmentPackage prev = it_equipment.previous();
						EquipmentPackage prev_repository = findEquipmentInList(requiredRepositoryEquipments, prev);
						prev_repository.returnAmount(prev.getAmount());
					}
					
					// wait until some are released from this package before trying again.
					synchronized (ep) {
						ep.wait();	
					}
					took_everything = false;
					break;
				}
			}
			
			if (took_everything) {
				// we got all the equipment we needed! stop the fetching loop
				break;
			}
		}
	}
	
	/**
	 * Searches for an equipment package in an equipments list.
	 * Comparing done by EquipmentPackage.equals (which compares by getName() string)
	 * @param equipments The equipment list to search on.
	 * @param ep The equipment package to look for.
	 * @return The equipment package with the name of the one we searched for, null if none was found.
	 */
	private EquipmentPackage findEquipmentInList(List<EquipmentPackage> equipments, EquipmentPackage ep) {
		ListIterator<EquipmentPackage> it_ep = equipments.listIterator();
		while (it_ep.hasNext()) {
			EquipmentPackage curr = it_ep.next();
			if (curr.equals(ep)) {
				return curr;
			}
		}
		
		return null;
	}
	
	
	/**
	 * Return equipments to the repository.
	 * @param equipments The equipmentPackages to return.
	 */
	public static void releaseEquipment(List<EquipmentPackage> equipments) {
		getInstance().returnEquipment(equipments);
	}

	/**
	 * Return equipments to the repository.
	 * @param equipments The equipmentPackages to return.
	 */
	public void returnEquipment(List<EquipmentPackage> equipments) {
		ListIterator<EquipmentPackage> it_equipment = equipments.listIterator();
		while (it_equipment.hasNext()) {
			EquipmentPackage requiredEquipment = it_equipment.next();
			EquipmentPackage repositoryEquipmentPackage = findEquipment(requiredEquipment.getName());
			
			repositoryEquipmentPackage.returnAmount(requiredEquipment.getAmount());
		}
	}	
	
	/**
	 * Search for an equipment package by a given name and amount.
	 * We buy more equipment if we couldn't find enough of it.
	 * @param name The equipment package name to look for.
	 * @param amount The amount of units we need.
	 * @return The equipment package with @param name provided and at least @param amount.
	 */
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
	
	/**
	 * Buy an equipment package from the science store by name and minimum amount of units.
	 * The EquipmentPackage will be added to the repository directly.
	 * @param name The name of the equipment package.
	 * @param amount The amount of units we need.
	 */
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
	
	/**
	 * Find a laboratory with a given @param spec.
	 * If we couldn't find one, purchase it from the science store.
	 * @param spec The specialization name we require the laboratory to be.
	 * @return The laboratory we found and null if we couldn't find one AND couldn't purchase one. 
	 */
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
	
	/**
	 * Buy a laboratory with a given @param spec from the science store.
	 * Buy the best "costPerScientist" laboratory.
	 * @param spec The specialization name of the laboratory we wish to purchase.
	 */
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

	/**
	 * Get statistics object instance from ChiefScientist.
	 * @return The Statistics object instance. 
	 */
	public Statistics getStats() {
		return _chiefScientist.getStats();
	}
	
	/**
	 * Print statistics of our science center work.
	 */
	public static void printStats() {
		System.out.println(getInstance().getStats());
	}
}
