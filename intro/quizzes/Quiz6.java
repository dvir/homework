public class Quiz6 {
	public static Boolean susu2 (int[] weights, int sum, int currentSum, int i) {
		Boolean state = false;
		
		if (i < 0) {
			if (sum == currentSum) {
				return true;
			}
			else {
				return false;
			}
		}
		
		// use this weight
		state = susu2(weights, sum, currentSum + weights[i], i-1);
		if (state) {
			System.out.print("1 ");
		}
		else {
			// don't use this weight
			state = susu2(weights, sum, currentSum, i-1);
			if (state) {
				System.out.print("0 ");
			}		
		}
		
		return state;
	}
	
	public static int susu3 (int[] weights, int sum, int currentSum, int i) {
		int count = 0;
		
		if (i < 0) {
			if (sum == currentSum) {
				return 1;
			}
			else {
				return 0;
			}
		}
		
		// use this weight
		count += susu3(weights, sum, currentSum + weights[i], i-1);
	
		// don't use this weight
		count += susu3(weights, sum, currentSum, i-1);
	
		return count;
	}	
	
	public static boolean hanuka (int[] candles, int day) {
		if (day == 9) {
			// if we got here, it is possible!
			return true;
		}
		
		// go over the candles and use candles for the current day (day+1 candles needed)
		for (int i = 0; i < candles.length; ++i) {
			// make sure we have enough candles of this color for the current day
			if (candles[i] >= day+1) {
				int[] candlesForColor = new int[candles.length];
				for (int j = 0; j < candles.length; ++j) {
					candlesForColor[j] = candles[j];
				}
				candlesForColor[i] -= day+1;
				if (hanuka(candlesForColor, day+1)) {
					return true;
				}
			}
		}
		
		// if we got here, it is not possible.
		return false;
	}
	
	public static void main (String[] args) {
		int sum;
		int[] weights;
		
		/* SUSU which weights */
		sum = 165;
		weights = new int[]{3, 17 ,26, 7, 9, 21, 6, 12, 13, 19, 8, 38};		
		
		susu2(weights, sum, 0, weights.length-1);
		System.out.println();
		
		/* SUSU solutions count */
		sum = 12; // sum to be reached
		weights = new int[]{3, 7, 9, 1, 2, 6, 4, 5}; // weights to be used
		
		System.out.println(susu3(weights, sum, 0, weights.length-1));
		
		/* Hanuka */
		int[] candles = {7, 5, 23, 5, 4};
		System.out.println(hanuka(candles, 1));
	}
}
