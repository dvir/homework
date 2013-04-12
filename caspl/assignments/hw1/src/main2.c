#include <stdio.h>
#include <stdlib.h>

#define	BUFFER_SIZE	(128)

extern int ones_counter(int x, int k);

int check(int x, int k) {
    if (x != 0 && k >= 0 && k <= 31) {
        return 1;
    }

    return 0;
}

int main(int argc, char** argv)
{
  char buf[BUFFER_SIZE];
  int x, k;
	
  printf("Enter x: ");
  fflush(stdout);
  fgets(buf, BUFFER_SIZE, stdin);
  x = atoi(buf);

  printf("Enter k: ");
  fflush(stdout);
  fgets(buf, BUFFER_SIZE, stdin);
  k = atoi(buf);

  ones_counter(x, k);
  return 0;
}
