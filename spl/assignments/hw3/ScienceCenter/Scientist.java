package ScienceCenter;

public class Scientist {
	private String _name;
	private String _spec;
	private double _cost;
	
	public Scientist(String name, String spec, double cost){
		_name = name;
		_spec = spec;
		_cost = cost;
	}
	
	public String getName() {
		return _name;
	}
	
	public String getSpec() {
		return _spec;
	}
	
	public double getCost() {
		return _cost;
	}
	
	public String toString() {
		return _name + " is a " + _spec + " scientist. ($" + _cost + ")";
	}	
}
