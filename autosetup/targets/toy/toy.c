#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int myprint(const char *x, int d) {
	const char *y = x + d;
	printf(y);
}

typedef struct {
	int x;
	int y;
} mystruct1_t;

typedef struct {
	char x;
	int y;
} mystruct2_t;

int main(int argc, char *argv[]) {
	char *mystring = strdup("Hello, world!\n");
	mystruct1_t *mystruct1 = malloc(sizeof(mystruct1_t));
	mystruct2_t *mystruct2 = malloc(sizeof(mystruct2_t));
	printf("string at %p, struct1 at %p, struct2 at %p\n", mystring, mystruct1, mystruct2);

	myprint(mystring, argc-1);
}
