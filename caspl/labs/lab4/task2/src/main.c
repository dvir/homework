#include "../include/util.h"
#include "../include/task2.h"

#define SYS_WRITE 4
#define SYS_OPEN 5
#define SYS_CLOSE 6
#define SYS_READ 3
#define SYS_LSEEK 19
#define SYS_EXIT 1
#define GET_DENTS 141
#define STDIN 0
#define STDOUT 1
#define STDERR 2

#define O_RDONLY 0
#define O_WRONLY 1
#define O_APPEND 1024

void parse_arguments(int argc, char** argv, char* delim, char** prefix) {
	int argIndex = 1;

	while (argc > argIndex) {
		/* got some arguments. check them */
		if (strcmp(argv[argIndex], "-p") == 0) {
			/* -p <prefix> */
			argIndex++;
            *delim = 'p';
            *prefix = argv[argIndex++];
        } else if (strcmp(argv[argIndex], "-a") == 0) {
			/* -a <prefix> */
			argIndex++;
            *delim = 'a';
            *prefix = argv[argIndex++];
        }
	}
}

int is_prefix(char* str, char* prefix) {
    int str_length = strlen(str);
    int prefix_length = strlen(prefix);
    int i = 0;
    while (i < str_length && i < prefix_length) {
        if (str[i] != prefix[i]) {
            /* found a char that is different! prefix is not a prefix of str */
            return -1;
        }

        i++;
    }

    return 0;
}

int main (int argc , char* argv[], char* envp[]) {
    char delim = 'n';
    char* prefix = "";
    char buf[8192], read_buf;
    struct linux_dirent *dent;
    int fd = 0, tfd, this_fd;
    int bytesRead = 0;
    int ret = 0;
    int i = 0;
    char d_type;

    parse_arguments(argc, argv, &delim, &prefix); 

    fd = system_call(SYS_OPEN, ".", O_RDONLY | O_DIRECTORY, 0);
    if (fd < 0) {
        system_call(SYS_WRITE, STDOUT, "fail! open", 10);
        system_call(SYS_EXIT, 0x55, 0, 0);
    }
    bytesRead = system_call(GET_DENTS, fd, buf, 8192);
    if (bytesRead < 0) {
        system_call(SYS_WRITE, STDOUT, "fail! dent", 10);
        system_call(SYS_EXIT, 0x55, 0, 0);
    }
  
    for (i = 0; i < bytesRead; i += dent->d_reclen) {
        dent = (struct linux_dirent *) (buf + i);
        d_type = *(buf + i + dent->d_reclen - 1);

        if (delim == 'n'
            || (delim == 'p' && d_type == DT_REG && is_prefix(dent->d_name, prefix) == 0)) 
        {
            system_call(SYS_WRITE, STDOUT, dent->d_name, strlen(dent->d_name));
            system_call(SYS_WRITE, STDOUT, "\n", 1);
        }

        if (delim == 'a' && d_type == DT_REG && is_prefix(dent->d_name, prefix) == 0) {
            tfd = system_call(SYS_OPEN, dent->d_name, O_WRONLY | O_APPEND, 0);
            if (tfd < 0) {
                system_call(SYS_WRITE, STDOUT, "fail! append flame", 18);
                system_call(SYS_EXIT, 0x55, 0, 0);
            }

            /* read current file and append to the open file */ 
            this_fd = system_call(SYS_OPEN, argv[0], O_RDONLY, 0);
            if (this_fd < 0) {
                system_call(SYS_WRITE, STDOUT, "fail! read flame", 16);
                system_call(SYS_EXIT, 0x55, 0, 0);
            } else {
                while (system_call(SYS_READ, this_fd, &read_buf, 1)) {
                    ret = system_call(SYS_WRITE, tfd, &read_buf, 1);
                    if (ret < 0) {
                        system_call(SYS_WRITE, STDOUT, "fail! write flame", 17);
                        system_call(SYS_WRITE, STDOUT, itoa(ret), 2);
                        system_call(SYS_EXIT, 0x55, 0, 0);
                    }
                }
                
                system_call(SYS_CLOSE, this_fd, 0, 0);
            }

            system_call(SYS_CLOSE, tfd, 0, 0);
        }
    }

    system_call(SYS_CLOSE, fd, 0, 0);
    return 0;
}
