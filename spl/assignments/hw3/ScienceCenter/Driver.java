/**
 * 
 */
package ScienceCenter;

import java.io.*;
import java.util.*;

/**
 * @author Dvir, Or
 *
 */
public class Driver {

	/**
	 * @param args "ant run -Darg0=InitialData.txt -Darg1=ExperimentsList.txt -Darg2=EquipmentForSale.txt -Darg3=ScientistsForSale.txt -Darg4=LaboratoriesForSale.txt"
	 */
	public static void main(String[] args) {
		String initialDataFileName = args[0];
		String experimentsListFileName = args[1];
		String equipmentForSaleFileName = args[2];
		String scientistsForSaleFileName = args[3];
		String laboratoriesForSaleFileName = args[4];
		
		Statistics stats = new Statistics();
		Repository repository = new Repository();
		
		List<String> lines;
		String[] data;
		
		lines = readfile(initialDataFileName);
		// Budget\nAMOUNT
		stats.increaseBudget(Integer.parseInt(lines.get(1)));
		
		// repository list
		int i = 3;
		for (; i < lines.size(); ++i) {
			if (lines.get(i).compareTo("Laboratories") == 0) {
				// we got to the laboratories section.
				i++;
				break;
			}
			
			data = lines.get(i).split("\t");
			EquipmentPackage ep = new EquipmentPackage(data[0], Integer.parseInt(data[1]), 0);
			repository.addEquipmentPackage(ep);
		}
		
		List<HeadOfLaboratory> labs = new ArrayList<HeadOfLaboratory>();
		for (; i < lines.size(); ++i) {
			data = lines.get(i).split("\t");
			HeadOfLaboratory lab = new HeadOfLaboratory(data[0], data[1], Integer.parseInt(data[2]), 0);
			labs.add(lab);
		}
		
		List<Experiment> experiments = new ArrayList<Experiment>();
		lines = readfile(experimentsListFileName);
		for (i = 0; i < lines.size(); ++i) {
			data = lines.get(i).split("\t");
			
			List<Experiment> preExperiments = new ArrayList<Experiment>();
			String[] preExperimentsIds = data[1].split(" ");
			for (int j = 0; j < preExperimentsIds.length; ++j) {
				int expId = Integer.parseInt(preExperimentsIds[j]);
				
				if (expId == 0) {
					// 0 represents "no pre-required experiments"
					break;
				}
				
				preExperiments.add(new Experiment(expId));
			}
			
			List<EquipmentPackage> reqEquipment = new ArrayList<EquipmentPackage>();
			String[] reqEquipmentData = data[3].split(" ");
			for (int j = 0; j < reqEquipmentData.length; ++j) {
				String[] equipmentData = reqEquipmentData[j].split(",");
				reqEquipment.add(new EquipmentPackage(equipmentData[0], Integer.parseInt(equipmentData[1]), 0));
			}
						
			Experiment exp = new Experiment(Integer.parseInt(data[0]), data[2], Integer.parseInt(data[4]), preExperiments, reqEquipment, Integer.parseInt(data[5]));
			experiments.add(exp);
		}
		
		List<EquipmentPackage> equipmentsForSale = new ArrayList<EquipmentPackage>();
		lines = readfile(equipmentForSaleFileName);
		for (i = 0; i < lines.size(); ++i) {
			data = lines.get(i).split("\t");
			EquipmentPackage ep = new EquipmentPackage(data[0], Integer.parseInt(data[1]), Integer.parseInt(data[2]));
			equipmentsForSale.add(ep);
		}		
		
		List<Scientist> scientistsForSale = new ArrayList<Scientist>();
		lines = readfile(scientistsForSaleFileName);
		for (i = 0; i < lines.size(); ++i) {
			data = lines.get(i).split("\t");
			Scientist scientist = new Scientist(data[0], data[1], Integer.parseInt(data[2]));
			scientistsForSale.add(scientist);
		}	
		
		List<HeadOfLaboratory> labsForSale = new ArrayList<HeadOfLaboratory>();
		lines = readfile(laboratoriesForSaleFileName);
		for (i = 0; i < lines.size(); ++i) {
			data = lines.get(i).split("\t");
			HeadOfLaboratory lab = new HeadOfLaboratory(data[0], data[1], Integer.parseInt(data[2]), Integer.parseInt(data[3]));
			labsForSale.add(lab);
		}	
		
		ScienceStore store = new ScienceStore(equipmentsForSale, scientistsForSale, labsForSale);

		ChiefScientist chiefScientist = new ChiefScientist(labs, experiments, store, stats, repository);
		ChiefScientistAssistant.setChiefScientist(chiefScientist);
		
		ChiefScientistAssistant.scanExperiments(null);
	}

   public static List<String> readfile(String inputFilename){
	    List<String> lines = new ArrayList<String>();
        try {
            File inFile = new File(inputFilename);
            FileReader ifr = new FileReader(inFile);
            BufferedReader ibr = new BufferedReader(ifr);

            String tempLine = "";
            while (tempLine != null)
            {
            	tempLine = ibr.readLine();
                if (tempLine != null)
                {
                	lines.add(tempLine);
                }
            }
            
            ibr.close();
            ifr.close();
            
        }

        catch(Exception e) {
            System.out.println("Error \""+e.toString()+"\" on file "+inputFilename);
            e.printStackTrace();
            System.exit(-1); //brutally exit the program
        }
        
        return lines;
    }
	
}
