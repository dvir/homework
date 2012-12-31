package ScienceCenter;

public class Scientist {
	private String _name; // the scientist's name
	private String _spec; // the scientist's specialization
	private double _cost; // the cost to purchase the scientist.
	
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
		StringBuilder sb = new StringBuilder();
		sb.append(_name).append(" is a ").append(_spec).append(" scientist. ($").append(_cost).append(")");
		return sb.toString();
	}	
}
