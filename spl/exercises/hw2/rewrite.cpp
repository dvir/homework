#include <iostream>
using namespace std; 
 
int *pPointer;
 
void SomeFunction()
{
    int *nNumber = new int;
    *nNumber = 25;    
 
    // make pPointer point to nNumber:
 
    pPointer = nNumber;
}

int main()
{
	SomeFunction(); // make pPointer point to something

	cout<< "Value of *pPointer: "<< *pPointer <<endl;
}
