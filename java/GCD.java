public class test {
	public static void main(String[] args) {
		int m = 48, n = 30, r = m % n;
		while (r != 0) {
			m = n;
			n = r;
			r = m % n;
		}
		
		System.out.println("Answer: " + n);
	}
}