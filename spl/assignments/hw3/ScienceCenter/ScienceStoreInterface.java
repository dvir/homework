package ScienceCenter;

import java.util.*;

/**
 * Interface for ScienceStore.
 * 
 * @author Dvir, Or
 * 
 * @invariant getEquipmentPackages().size() >= 0
 * 			  getScientists().size() >= 0
 * 			  getLaboratories().size() >= 0
 */
public interface ScienceStoreInterface {
	/**
	 * @pre: none
	 * @post: none
	 * @return List of EquipmentPackage available in the store.
	 */
	public List<EquipmentPackage> getEquipmentPackages();
	
	/**
	 * @pre: none
	 * @post: none
	 * @return List of Scientist available in the store.
	 */	
	public List<Scientist> getScientists();
	
	/**
	 * @pre: none
	 * @post: none
	 * @return List of HeadOfLaboratory available in the store.
	 */	
	public List<HeadOfLaboratory> getLaboratories();
	
	/**
	 * Searches the EquipmentPackage list in the store, and if we find the one
	 * requested, we remove it from the store.
	 * @param ep The equipment package we wish to purchase.
	 * @return true if it was found in the store, false otherwise.
	 * @pre: getEquipmentPackages().size() >= 0
	 * @post: if @ret == true getEquipmentPackages().size() == @pre(getEquipmentPackages().size() + 1) && false == getEquipmentPackages().contains(@param ep)
	 * 		  if @ret == false getEquipmentPackages().size() == @pre(getEquipmentPackages().size()) && false == getEquipmentPackages().contains(@param ep)
	 */
	public boolean buyEquipmentPackage(EquipmentPackage ep);
	
	/**
	 * Searches the Scientist list in the store, and if we find the one
	 * requested, we remove it from the store.
	 * @param s The scientist we wish to purchase.
	 * @return true if it was found in the store, false otherwise.
	 * @pre: getScientists().size() >= 0
	 * @post: if @ret == true getScientists().size() == @pre(getScientists().size() + 1) && false == getScientists().contains(@param s)
	 * 		  if @ret == false getScientists().size() == @pre(getScientists().size()) && false == getScientists().contains(@param s)
	 */	
	public boolean buyScientist(Scientist sc);
	
	/**
	 * Searches the HeadOfLaboratory list in the store, and if we find the one
	 * requested, we remove it from the store.
	 * @param lab The laboratory we wish to purchase.
	 * @return true if it was found in the store, false otherwise.
	 * @pre: getLaboratories().size() >= 0
	 * @post: if @ret == true getLaboratories().size() == @pre(getLaboratories().size() + 1) && false == getLaboratories().contains(@param lab)
	 * 		  if @ret == false getLaboratories().size() == @pre(getLaboratories().size()) && false == getLaboratories().contains(@param lab)
	 */	
	public boolean buyLaboratory(HeadOfLaboratory lab);
}
