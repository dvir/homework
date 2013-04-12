#include <stdio.h>

#define	BUFFER_SIZE	(128)

extern int my_func(char* buf);

int main(int argc, char** argv)
{
  char buf[BUFFER_SIZE];
	
  printf("Enter string: ");
  fflush(stdout);

  fgets(buf, BUFFER_SIZE, stdin);
  printf("Got string: %s\n", buf);

  my_func(buf);

  return 0;
}
