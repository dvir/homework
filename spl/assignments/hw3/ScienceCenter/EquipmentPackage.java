package ScienceCenter;

import java.util.concurrent.*;

public class EquipmentPackage {
	//state
	private String _name;
	private int _amount;
	private double _cost;
	private Semaphore _sm;
	
	//constructors
	public EquipmentPackage(String name, int amount, double cost){
		_name = name;
		_amount = amount;
		_cost = cost;
		_sm = new Semaphore(amount, true);
	}
	
	//behavior
	
	public String getName() {
		return _name;
	}
	
	public int getAmount() {
		return _amount;
	}
	
	public double getCost() {
		return _cost;
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
}
