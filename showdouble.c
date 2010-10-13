#include <stdio.h>

void show_double(char* result, int size, double x) {
	snprintf(result, --size, "%g", x);
	result[size] = 0;
}
