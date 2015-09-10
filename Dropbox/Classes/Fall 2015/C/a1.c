#include <math.h>
#include <stdio.h>

//helper function to get the length of an integer.
int get_integer_length (int number){
	int len = 1;
	while(number>9){
		len++;
		number /= 10;
	}
	return len;
}

// I guess the only pow function available just uses doubles?
// I'm just going to write a new one.

int Power(int n, int exp) {
	int i, power = 1;
	if (exp == 0) {
		return 1;
	}
	if (exp < 0) {
		return 0; 
	}// I'm not going to bother with negative exponents because I think that's beyond the scope of this assignme
	for(i = 0; i < exp; i++){
		power *= n;
	}
	return power;
}

int main() {
	int input, length, temp, rem , sum = 0;
	printf("Please enter a number:\n");
	scanf("%d", &input);
	length = get_integer_length(input);
	temp = input;
	while(temp != 0){
		rem = temp % 10;
		sum += Power(rem, length);
	}
	if(sum == input){
		printf("%d is an Armstrong number!", input);
	}
	else {
		printf("%d is not an Armstrong number.", input);
	}
}
