package ScienceCenter;

import java.util.concurrent.*;

public class EquipmentPackage {
	private String _name;
	private int _amount;
	private double _cost;
	private Semaphore _sm;
	
	public EquipmentPackage(String name, int amount, double cost){
		_name = name;
		_amount = amount;
		_cost = cost;
		_sm = new Semaphore(amount, true);
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
	
	public void takeAmount(int amount) throws InterruptedException {
		_sm.acquire(amount);
	}
	
	public boolean tryTakeAmount(int amount) {
		return _sm.tryAcquire(amount);
	}
	
	public void returnAmount(int amount) {
		_sm.release(amount);
	}	
	
	public synchronized void increaseAmount(int amount) {
		_amount += amount;
		_sm.release(amount);
	}
	
	public String toString() {
		return _name + " - " + _amount + " ($" + _cost + ")";
	}
}
