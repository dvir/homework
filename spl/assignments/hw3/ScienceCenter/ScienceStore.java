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
	
	//state
	

	//constructors
	public ScienceStore (){
		
	}
	
	public ScienceStore (List<EquipmentPackage> epList, List<Scientist> scientistList, List<HeadOfLaboratory> labList){
		
	}
	
	
	//behavior
	/* (non-Javadoc)
	 * @see ScienceCenter.ScienceStoreInterface#getEquipmentPackages()
	 */
	@Override
	public List<EquipmentPackage> getEquipmentPackages() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see ScienceCenter.ScienceStoreInterface#getScientists()
	 */
	@Override
	public List<Scientist> getScientists() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see ScienceCenter.ScienceStoreInterface#getLaboratories()
	 */
	@Override
	public List<HeadOfLaboratory> getLaboratories() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see ScienceCenter.ScienceStoreInterface#buyEquipmentPackage(ScienceCenter.EquipmentPackage)
	 */
	@Override
	public boolean buyEquipmentPackage(EquipmentPackage ep) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see ScienceCenter.ScienceStoreInterface#buyScientist(ScienceCenter.Scientist)
	 */
	@Override
	public boolean buyScientist(Scientist s) {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see ScienceCenter.ScienceStoreInterface#buyLaboratory(ScienceCenter.HeadOfLaboratory)
	 */
	@Override
	public boolean buyLaboratory(HeadOfLaboratory l) {
		// TODO Auto-generated method stub
		return false;
	}

}
