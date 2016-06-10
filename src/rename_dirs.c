#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include <errno.h>

#include <unistd.h>
#include <fcntl.h>
#include <ftw.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "config.h"
#include "utils.h"




char *randstring(size_t length) {

  static char charset[] = "abcdefghijklmnopqrstuvwxyz";        
  char *randomString = NULL;

  if (length) {
    randomString = malloc(sizeof(char) * (length +1));

    if (randomString) {            
      for (int n = 0;n < length;n++) {            
	int key = rand() % (int)(sizeof(charset) -1);
	randomString[n] = charset[key];
      }

      randomString[length] = '\0';
    }
  }

  return randomString;
}



char SPRINTF_BUFFER[1024];

char **COMMAND;

/*
 * Callback passed to ftw. For the given path, if that path is a
 * directory, create a corresponding directory in WORKING_DIR/anonymized.
 * If it is a regular file, pipe it to ./bin/anonymize INPUT_PATH (where
 * INPUT_PATH is a CLI argument containing names to replace). If TWOCOL
 * is set (again a CLI flag), "-t" is also passed as a flag, telling
 * ./bin/anonymize to parse a two-column CSV file rather than a simple
 * list of names.
 */
int walk_fn(const char *path, const struct stat *sb, int typeflag)
{
  char* token;
  char* base;
  char* directory;
  int length;
  int length2;

  int depth;
  int i;

  char* randomstring;

	if (S_ISREG(sb->st_mode)) { /* If this is a regular file: */
	} else { /* If this is a directory, create it */
		snprintf(SPRINTF_BUFFER, 1024, WORKING_DIR "/anonymized/%s", path);
		mkdir(SPRINTF_BUFFER, 0777);

		depth = 0;
		for (i = 0; i < strlen(path); i++) {
		  if (path[i] == '/') depth++;
		}
		token = strrchr(path, '/');

		if (token != NULL && depth==3) { 
		  length = strlen(token);
		  directory = malloc(length);
		  memcpy(directory, token+1, length);


		  length2 = strlen(path) - length;
		  base = malloc(length2); //strlen(path)-length-1);
		  memcpy(base, path,length2); //strlen(path-length-1));
		  base[length2] = '\0';

		  randomstring = randstring(6);
		  printf ("mv %-50s %s/%s\n", path, base, randomstring);
		}
	}
	return 0;
}

int main(int argc, char **argv)
{
  printf ("#!/bin/bash\n\n");
		  
	COMMAND = (char **) malloc(argc * sizeof(char *));
	memcpy(COMMAND+1, argv+2, (argc - 2) * sizeof(char *));
	COMMAND[0] = "./bin/anonymize";
	COMMAND[argc-1] = NULL;

	mkdir(WORKING_DIR, 0777);
	mkdir(WORKING_DIR "/anonymized", 0777);
	ftw(argv[1], walk_fn, 8);
	free(COMMAND);
}
