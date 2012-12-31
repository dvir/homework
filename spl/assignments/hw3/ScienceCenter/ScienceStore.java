/**
 * 
 */
package ScienceCenter;

import java.util.*;

/**
 * @author Dvir
 *
 */
public class ScienceStore implements ScienceStoreInterface {
	List<EquipmentPackage> _equipments;
	List<Scientist> _scientists;
	List<HeadOfLaboratory> _labs;

	public ScienceStore (){
		
	}
	
	public ScienceStore (List<EquipmentPackage> equipments, List<Scientist> scientists, List<HeadOfLaboratory> labs){
		_equipments = equipments;
		_scientists = scientists;
		_labs = labs;
	}
	
	public List<EquipmentPackage> getEquipmentPackages() {
		return _equipments;
	}

	public List<Scientist> getScientists() {
		return _scientists;
	}

	public List<HeadOfLaboratory> getLaboratories() {
		return _labs;
	}

	public synchronized boolean buyEquipmentPackage(EquipmentPackage ep) {
		// we return false if we didn't actually delete anything - that means
		// it was already purchased and added to repository so we don't care.
		ListIterator<EquipmentPackage> it = _equipments.listIterator();
		while (it.hasNext()) {
			EquipmentPackage curr = it.next();
			if (curr.equals(ep)) { // object comparison
				it.remove();
				ChiefScientistAssistant.getInstance().getStats().purchasedEquipmentPackage(new EquipmentPackage(ep));
				return true;
			}
		}
		
		return false;
	}

	public synchronized boolean buyScientist(Scientist sc) {
		// we return false if we didn't actually delete anything - that means
		// it was already purchased and added to our lab so we don't care.
		ListIterator<Scientist> it = _scientists.listIterator();
		while (it.hasNext()) {
			Scientist curr = it.next();
			if (curr.equals(sc)) { // object comparison
				it.remove();
				ChiefScientistAssistant.getInstance().getStats().purchasedScientist(sc);
				return true;
			}
		}
		
		return false;		
	}

	public synchronized boolean buyLaboratory(HeadOfLaboratory lab) {
		// we return false if we didn't actually delete anything - that means
		// it was already purchased and added to repository so we don't care.
		ListIterator<HeadOfLaboratory> it = _labs.listIterator();
		while (it.hasNext()) {
			HeadOfLaboratory curr = it.next();
			if (curr.equals(lab)) { // object comparison
				it.remove();
				ChiefScientistAssistant.getInstance().getStats().purchasedLaboratory(new HeadOfLaboratory(lab));
				return true;
			}
		}
		
		return false;
	}	
}
