/**
 * 
 */
package ScienceCenter;

import java.util.*;

/**
 * @author Dvir
 *
 */
public class RunnableExperiment extends Observable implements Runnable {
	private Experiment _experiment;
	
	public RunnableExperiment(Experiment exp) {
		this._experiment = exp;
	}
	
	public void run() {
		_experiment.start();
		
		long runtime = _experiment.getAllowedRuntime();
		
		// simulate the experiment
		// Each loop body is a day in real life.
		// Each day, we should do the following:
		// 1. Acquire equipment.
		// 2. Sleep for 8 hours.
		// 3. Release equipment.
		// 4. Check if we passed the experiment runtime. If so, we are done.
		// 5. Sleep for 16 hours.
		
		while(runtime > 0) {
			// Start a new day
			_experiment.nextDay();
			
			// 1. Acquire equipment.			
			try {
				ChiefScientistAssistant.takeEquipment(_experiment.getRequiredEquipment());
			} catch (InterruptedException e) {
				// Thread interrupted. Since we have no equipment, 
				// we just need to reset the experiment before exiting.
				_experiment.reset();
				
				// re-interrupt the thread
				Thread.currentThread().interrupt();				
			}
			
			// 2. Sleep for 8 hours.
			try {
				Thread.sleep(800);
			} catch (InterruptedException e) {
				// Thread interrupted. Return equipment and reset the experiment before exiting.
				ChiefScientistAssistant.releaseEquipment(_experiment.getRequiredEquipment());
				_experiment.reset();
				
				// re-interrupt the thread
				Thread.currentThread().interrupt(); 
			}
			
			// update experiment runtime counter
			runtime -= 8;
			
			// 3. Release equipment.
			ChiefScientistAssistant.releaseEquipment(_experiment.getRequiredEquipment());
			
			// 4. Check if we passed the experiment runtime. If so, we are done.
			if (runtime <= 0) {
				break;
			}
			
			// 5. Sleep for 16 hours.
			try {
				Thread.sleep(1600);
			} catch (InterruptedException e) {
				// Thread interrupted. Since we have no equipment, 
				// we just need to reset the experiment before exiting.
				_experiment.reset();
				
				// re-interrupt the thread
				Thread.currentThread().interrupt();				
			}
		}
		
		
		_experiment.complete();
		
System.out.println("Experiment " + _experiment.getId() + " notifying observers.");
		this.setChanged();
		this.notifyObservers();
	}
	
	public Experiment getExperiment() {
		return _experiment;
	}
}
