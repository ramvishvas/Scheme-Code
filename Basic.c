#include <stdio.h>
float maximum(float a, float b)
{
	if (a > b)
	{
		return a;
	}
	return b;
}
float minimum(float a, float b)
{
	if (a > b)
	{
		return b;
	}
	return a;
}

void takeInput()
{
	int n, i;
	float temp, max, min, sum;
	printf("Enter count of total No. : ");
	scanf("%d",&n);
	// check n is greater or not
	if (n >= 1)
	{
		// take 1st number
		printf("Enter 1 Number : ");
		scanf("%f", &temp);
		// consider 1st number as min, max and sum all
		max = temp;
		min = temp;
		sum = temp;
		i = 2; // 1st number is already taken
		while(i <= n)
		{
			printf("Enter %d Number : ",i);
			scanf("%f", &temp);
			// get new min and max
			max = maximum(max, temp);
			min = minimum(min, temp);
			sum += temp;
			++i;
		}
		printf("Minimum : %f\n", min);
		printf("Maximum : %f\n", max);
		printf("Average : %f\n", (sum/n));
	}
}
int main()
{
	takeInput();
	return 0;
}