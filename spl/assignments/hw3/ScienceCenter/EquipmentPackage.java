package ScienceCenter;

public class EquipmentPackage {
	//state
	private String _name;
	private int _amount;
	private double _cost;
	
	//constructors
	public EquipmentPackage(String name, int amount, double cost){
		_name = name;
		_amount = amount;
		_cost = cost;
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
	
	public synchronized boolean takeAmount(int amount) {
		if (this._amount < amount) {
			return false;
		}
		
		this._amount -= amount;
		return true;
	}
	
	public void returnAmount(int amount) {
		this._amount += amount;
	}	
}
