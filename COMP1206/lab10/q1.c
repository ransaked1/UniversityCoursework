#include <math.h>

int CACHE(double *prod, double *sum, double d1, double d2);

int CACHE(double *prod, double *sum, double d1, double d2)
{
	int i1;
	int i2;

	i1 = floor(d1);
	i2 = floor(d2);

	*prod = i1 * i2;
	*sum = i1 + i2;

	if (*prod > 0 || i1 == *sum)
		return 1;

	return -1;
}