#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Takes an argument string and returns the delimiter. 
   Sets split_bytes if relevant. */ 
char getDelimiter(char* arg, int* split_bytes) {
	char delim;

	if (strcmp(arg, "C") == 0 ||
	    strcmp(arg, "W") == 0) {
		delim = arg[0];
	} else {
		/* we received a number of bytes to split after. */
		delim = 'N';
		*split_bytes = atoi(arg);
	}

	return delim;
}

int parse_arguments(int argc, char** argv, FILE** streams, int* streamsCount, char* delim, int* split_bytes) {
	FILE* stream = stdin;
	int argIndex = 1;

	*streamsCount = 0;
	*split_bytes = -1;

	while (argc > argIndex) {
		/* got some arguments. check them */
		if (strcmp(argv[argIndex], "-d") == 0) {
			/* -d [DELIMITER_TYPE = W] */
			argIndex++;

			/* check that we have more args to parse */
			/* and that the next one isn't an option argument */
			if (argc > argIndex && argv[argIndex][0] != '-') {
				/* got a delimiter_type. set it */
				*delim = getDelimiter(argv[argIndex++], split_bytes);
			}
		} else if (strcmp(argv[argIndex], "-i") == 0) {
			/* -i FILE */
			argIndex++;
		
			if (argc > argIndex) {
				/* got file name */
				stream = fopen(argv[argIndex], "r");	
				argIndex++;
			} else {
				/* didn't receive a filename after -i! exit with an error */
				printf("-i should be followed with a filename to open.");
				return 1;
			}	
		} else {
			/* [FILE1], [FILE2], [FILE3]... */
			streams[(*streamsCount)++] = fopen(argv[argIndex], "r");
			argIndex++;
		}
	}

	if (streamsCount == 0) {
		/* no stream was selected. add the default or given -i stream to the party */
		streams[0] = stream;
		(*streamsCount)++;
	}

	return 0;
}

int main(int argc, char** argv) {
	char c, delim = 'W';
	int split_bytes = -1, bytes_read = 0;
	int first = 1;
	int streamIndex = 0, streamsCount = 0;
	FILE* streams[3];
	
	parse_arguments(argc, argv, streams, &streamsCount, &delim, &split_bytes);

	for (streamIndex = 0; streamIndex < streamsCount; streamIndex++) {
		while ((c = fgetc(streams[streamIndex])) != EOF) { 
			bytes_read++;

			if (first == 1) {
				printf("\n");
				first = 0;
			}

			if ((delim == 'W' && isspace(c)) 
			    || (delim == 'N' && (bytes_read % split_bytes) == 0)
			    || (delim == 'C' && c == ',')) 
			{
				printf("\n");
			} else {
				printf("%c", c);
			}
		}

		fclose(streams[streamIndex]);
	}

	return 0;
}
