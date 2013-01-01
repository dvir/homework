package ScienceCenter;

import java.util.concurrent.*;

public class EquipmentPackage {
	private String _name; // the equipment package type.
	private int _amount; // the amount of this equipment in the package.
	private double _cost; // the cost to purchase this equipment package.
	private Semaphore _sm; // the Semaphore that handles the equipment package usage queue.
	
	public EquipmentPackage(String name, int amount, double cost){
		_name = name;
		_amount = amount;
		_cost = cost;
		_sm = new Semaphore(amount, true); // a fair semaphore
	}
	
	/**
	 * Copy constructor
	 * @param ep The equipment package to copy
	 */
	public EquipmentPackage(EquipmentPackage ep) {
		_name = ep.getName();
		_amount = ep.getAmount();
		_cost = ep.getCost();
		_sm = new Semaphore(_amount, true);
	}
	
	/**
	 * Takes some of the equipment from the package.
	 * @param amount The amount to take.
	 * @throws RuntimeException If the amount requested is more than available.
	 */
	public void takeAmount(int amount) throws InterruptedException {
		if (amount > _amount) {
			throw new RuntimeException("Requested too many "+_name+". ("+amount+" while only "+_amount+" are available)");
		}
		
		_sm.acquire(amount);
	}
	
	public boolean tryTakeAmount(int amount) {
		return _sm.tryAcquire(amount);
	}	
	
	/**
	 * Returns equipment back to the equipment package.
	 * @param amount The amount to return.
	 * @throws RuntimeException If the amount returned is more than we actually have.
	 */
	public synchronized void returnAmount(int amount) {
		if (amount > _amount) {
			throw new RuntimeException("Returned too many "+_name+". ("+amount+" while only "+_amount+" are available)");
		}
		
		_sm.release(amount);
		this.notifyAll();		
	}	
	
	/**
	 * Increase the amount of units available in the equipment package.
	 * @param amount The amount to increase by.
	 */
	public synchronized void increaseAmount(int amount) {
		_amount += amount;
		_sm.release(amount);
	}
	
	public String getName() {
		return _name;
	}
	
	public int getAmount() {
		return _amount;
	}
	
	public double getCost() {
		return _cost;
	}
	
	public double getCostPerItem() {
		return _cost / _amount;
	}
	
	public boolean equals(Object other) {
		return (other instanceof EquipmentPackage && ((EquipmentPackage)other).getName().compareTo(this.getName()) == 0);
	}	
	
	public String toString() {
		return _name + " - " + _amount + " ($" + _cost + ")";
	}
}
