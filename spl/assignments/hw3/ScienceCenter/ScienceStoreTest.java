/**
 * 
 */
package ScienceCenter;

import java.util.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.Before;

/**
 * @author Dvir
 *
 */
public class ScienceStoreTest {

	//state
	private ScienceStore _scienceStore;
	
	
	//behavior
	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		_scienceStore = new ScienceStore();
	}

	@Test
	public void test() {
		testGetEquipmentPackages();
		testGetScientists();
		testGetLaboratories();
		
		testBuyEquipmentPackage();
		testBuyLaboratory();
		testBuyScientist();
	}
	
	public void testGetEquipmentPackages(){
		assertNotNull("Could not get Equipment Packages List", _scienceStore.getEquipmentPackages());
	}
	
	public void testGetScientists(){
		assertNotNull("Could not get Scientists List", _scienceStore.getScientists());
	}
	
	public void testGetLaboratories(){
		assertNotNull("Could not get Laboratories List", _scienceStore.getLaboratories());
	}
	
	public void testBuyEquipmentPackage(){
		List<EquipmentPackage> epList = new ArrayList<EquipmentPackage>();
		List<Scientist> scientistList = new ArrayList<Scientist>();
		List<HeadOfLaboratory> labList = new ArrayList<HeadOfLaboratory>();
		
		EquipmentPackage equip1 = new EquipmentPackage("Black Dildo", 4, 400);
		EquipmentPackage equip2 = new EquipmentPackage("Lube", 2, 45);
		epList.add(equip1);
		epList.add(equip2);
		
		_scienceStore = new ScienceStore (epList, scientistList, labList);
		
		assertTrue("Did not insert Equipment Package", _scienceStore.getEquipmentPackages().contains(equip2));
		
		int sizeBefore = _scienceStore.getEquipmentPackages().size();
		assertTrue("Could not buy Equipment Package", _scienceStore.buyEquipmentPackage(equip2));
		
		int sizeAfter = _scienceStore.getEquipmentPackages().size();
		assertTrue("Did not remove Equipment Package from Equipment Packages List", sizeAfter + 1 == sizeBefore);
		
		assertFalse("Did not remove Equipment Package", _scienceStore.getEquipmentPackages().contains(equip2));
		
		assertFalse("Did not return false when trying to buy an unexistant Equipment Package", _scienceStore.buyEquipmentPackage(equip2));
	}

	public void testBuyLaboratory(){
		List<EquipmentPackage> epList = new ArrayList<EquipmentPackage>();
		List<Scientist> scientistList = new ArrayList<Scientist>();
		List<HeadOfLaboratory> labList = new ArrayList<HeadOfLaboratory>();
		
		HeadOfLaboratory lab1 = new HeadOfLaboratory("Dexter", "Secret Labs", 3, 400);
		HeadOfLaboratory lab2 = new HeadOfLaboratory("Dr. House", "Medical Puzzles", 4, 45);
		labList.add(lab1);
		labList.add(lab2);
		
		_scienceStore = new ScienceStore (epList, scientistList, labList);
		
		assertTrue("Did not insert Laboratory", _scienceStore.getLaboratories().contains(lab2));
		
		int sizeBefore = _scienceStore.getLaboratories().size();
		assertTrue("Could not buy Laboratory", _scienceStore.buyLaboratory(lab2));
		
		int sizeAfter = _scienceStore.getLaboratories().size();
		assertTrue("Did not remove Laboratory from Laboratories List", sizeAfter + 1 == sizeBefore);
		
		assertFalse("Did not remove Laboratory", _scienceStore.getLaboratories().contains(lab2));
		
		assertFalse("Did not return false when trying to buy an unexistant Laboratory", _scienceStore.buyLaboratory(lab2));		
	}

	public void testBuyScientist(){
		List<EquipmentPackage> epList = new ArrayList<EquipmentPackage>();
		List<Scientist> scientistList = new ArrayList<Scientist>();
		List<HeadOfLaboratory> labList = new ArrayList<HeadOfLaboratory>();
		
		Scientist scientist1 = new Scientist("Professor X", "Mutants", 400);
		Scientist scientist2 = new Scientist("Master Splinter", "Ninja Turtles", 45);
		scientistList.add(scientist1);
		scientistList.add(scientist2);
		
		_scienceStore = new ScienceStore (epList, scientistList, labList);
		
		assertTrue("Did not insert Scientist", _scienceStore.getScientists().contains(scientist2));
		
		int sizeBefore = _scienceStore.getScientists().size();
		assertTrue("Could not buy Scientist", _scienceStore.buyScientist(scientist2));
		
		int sizeAfter = _scienceStore.getScientists().size();
		assertTrue("Did not remove Scientist from Scientists List", sizeAfter + 1 == sizeBefore);
		
		assertFalse("Did not remove Scientist", _scienceStore.getScientists().contains(scientist2));
		
		assertFalse("Did not return false when trying to buy an unexistant Scientist", _scienceStore.buyScientist(scientist2));		
	}
}
