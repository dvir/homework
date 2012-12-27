/**
 * 
 */
package ScienceCenter;

import java.util.List;

/**
 * @author Dvir
 *
 */
public class ScienceStore implements ScienceStoreInterface {
	List<EquipmentPackage> _equipments;
	List<Scientist> _scientists;
	List<HeadOfLaboratory> _labs;

	//constructors
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

	public void buyEquipmentPackage(EquipmentPackage ep) {
		System.out.println("HERE!!!!!");
		System.exit(-1);
	}

	public void buyScientist(Scientist s) {

	}

	public void buyLaboratory(HeadOfLaboratory l) {

	}	
}
