#include <cmath>
#include <iostream>
using namespace std;

long base2(int number) {
	long bin = 0;
	int i = 0;
	while (number > 0) {
		bin += pow(10, i) * (number % 2);
		i++;
		number /= 2;
	}

	return bin;
}   

int main(int argc, char *argv[]) {
	int num;
	cout << "Enter a decimal number to convert to binary: ";
	cin >> num;
	cout << base2(num) << endl;
	
	return 0;
}	
