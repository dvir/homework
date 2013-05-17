#include "../include/util.h"

#define SYS_WRITE 4
#define SYS_OPEN 5
#define SYS_CLOSE 6
#define SYS_READ 3
#define SYS_LSEEK 19
#define SYS_EXIT 1
#define STDIN 0
#define STDOUT 1
#define STDERR 2

#define O_RDONLY 0


int parse_arguments(int argc, char** argv, int streams[], char* delim) {
	int stream = STDIN;
	int argIndex = 1;

	int streamsCount = 0;

	while (argc > argIndex) {
		/* got some arguments. check them */
		if (strcmp(argv[argIndex], "-o") == 0) {
			/* -o - we omit every 2nd character, starting from the first. */
			argIndex++;
            *delim = 'o';
        } else if (strcmp(argv[argIndex], "-e") == 0) {
			/* -o - we omit every 2nd character, starting from the second. */
			argIndex++;
            *delim = 'e';
        } else if (strcmp(argv[argIndex], "-n") == 0) {
			/* -o - we omit nothing. All characters are to be printed. */
			argIndex++;
            *delim = 'n';
		} else if (strcmp(argv[argIndex], "-i") == 0) {
			/* -i FILE */
			argIndex++;
		
			if (argc > argIndex) {
				/* got file name */
				stream = system_call(SYS_OPEN, argv[argIndex], O_RDONLY, 0);	
				argIndex++;
			} else {
				/* didn't receive a filename after -i! exit with an error */
				/* printf("-i should be followed with a filename to open.");*/
				return -1;
			}	
		} else {
			/* [FILE1], [FILE2], [FILE3]... */
			streams[streamsCount++] = system_call(SYS_OPEN, argv[argIndex], O_RDONLY, 0);
			argIndex++;
		}
	}

	if (streamsCount == 0) {
		/* no stream was selected. add the default or given -i stream to the party */
		streams[0] = stream;
		streamsCount++;
	}

	return streamsCount;
}

int main (int argc , char* argv[], char* envp[]) {
    char delim = 'n';
    char buf = 't';
    int streams[3];
    int streamsCount = 0;
    int i;
    int charCount = 0;

    streamsCount = parse_arguments(argc, argv, streams, &delim);   
    
    for (i = 0 ; i < streamsCount; i++) {
       while (system_call(SYS_READ, streams[i], &buf, 1)) {
           if ((delim == 'o' && (charCount & 1) == 1) 
                || (delim == 'e' && (charCount & 1) == 0)
                || (delim == 'n'))
           {
                system_call(SYS_WRITE, STDOUT, &buf, 1);
           }
           charCount++;
       }

       system_call(SYS_CLOSE, streams[i], 0, 0);
    }

    return 0;
}
