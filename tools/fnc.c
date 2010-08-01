#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <assert.h>
#include <libgen.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "fnc.h"

#define STR_BUFFER_SIZE 512

// copy the path to efene from the environment variable FNPATH if available
// if not found return NULL
// you have to free the variable
char* get_efene_path_from_env() {
	char *fnpath, *result;

	result = NULL;
	fnpath = getenv("FNPATH");

	if (fnpath != NULL) {
		result = strdup(fnpath);
		assert(result != NULL);
	}

	return result;
}

void show_usage() {
	printf("usage:\n");
}

int is_dir(const char *path) {
	struct stat st;
	return (stat(path, &st) == 0 && S_ISDIR(st.st_mode));
}

void run_shell(const char *basepath) {
	int count, status;
	char *fnpath, buffer[STR_BUFFER_SIZE];
	fnpath = get_efene_path_from_env();

	if (fnpath == NULL) {
		if (!is_dir("../ebin")) {

			count = snprintf(buffer, STR_BUFFER_SIZE, "%s/ebin", basepath);

			if(count > STR_BUFFER_SIZE) {
				fprintf(stderr, "base path seems huge, can't run command\n");
				exit(EXIT_FAILURE);
			}

			fnpath = strdup(buffer);
			assert(fnpath != NULL);

			if (!is_dir(fnpath)) {
				fprintf(stderr, "$FNPATH is not defined, %s and ../ebin don't exist, options:\n", fnpath);
				fprintf(stderr, " * set $FNPATH to the path where efene is installed\n");
				fprintf(stderr, " * run fnc -s from the bin directory\n");
				fprintf(stderr, " * stop doing maginc tricks with your path\n");
				exit(EXIT_FAILURE);
			}

			count = snprintf(buffer, STR_BUFFER_SIZE,
				"erl -run fn run shell -run init stop -noshell -pa \"%s\"\n",
				fnpath);

			if(count > STR_BUFFER_SIZE) {
				fprintf(stderr, "command path seems huge, can't run command\n");
				exit(EXIT_FAILURE);
			}

			status = system(buffer);
		}
		else {
			status = system("erl -run fn run shell -run init stop -noshell -pa ../ebin\n");
		}
	}
	else {
		count = snprintf(buffer, STR_BUFFER_SIZE,
			"erl -run fn run shell -run init stop -noshell -pa %s/ebin\n",
			fnpath);

		if(count > STR_BUFFER_SIZE) {
			fprintf(stderr, "$FNPATH seems huge, can't run command\n");
			exit(EXIT_FAILURE);
		}

		status = system(buffer);
		free(fnpath);
	}

	exit(status);
}

struct FnOptions* fn_options_new() {
	struct FnOptions* options = (struct FnOptions*) malloc(sizeof(struct FnOptions));

	if (options != NULL) {
		options->output_path = NULL;
		options->files = NULL;
		options->output_type = NULL;
		options->is_eval = 0;
		options->is_erl_eval = 0;
	}

	return options;
}

void fn_options_free_files(struct FnOptions* options) {
	int index;

	if (options != NULL && options->files != NULL && options->files_num > 0) {
		for (index = 0; index < options->files_num; index++) {
			free(options->files[index]);
		}

		free(options->files);
	}
}

void fn_options_delete(struct FnOptions* options) {
	if (options != NULL) {

		if (options->output_path != NULL) {
			free(options->output_path);
		}

		fn_options_free_files(options);

		if (options->output_type != NULL) {
			free(options->output_type);
		}

		free(options);
	}
}

void fn_options_print(struct FnOptions* options) {
	int index;

	if (options != NULL) {
		printf("options:\n");

		if (options->output_path != NULL) {
			printf("\toutput: %s\n", options->output_path);
		}

		if (options->files != NULL && options->files_num > 0) {
			for (index = 0; index < options->files_num; index++) {
				printf("\tfile: %s\n", options->files[index]);
			}
		}

		if (options->output_type != NULL) {
			printf("\toutput type: %s\n", options->output_type);
		}

		printf("\tis eval: %d\n", options->is_eval);
		printf("\tis erlang eval: %d\n", options->is_erl_eval);
	}
	else {
		printf("options: NULL\n");
	}
}

int fn_options_copy_output_type(struct FnOptions* options, const char* arg) {
	if (arg == NULL || options == NULL) {
		return 0;
	}

	if (options->output_type != NULL) {
		free(options->output_type);
	}

	options->output_type = strdup(optarg);
	assert(options->output_type != NULL);

	return 1;
}

int fn_options_copy_output_path(struct FnOptions* options, const char* arg) {
	if (arg == NULL || options == NULL) {
		return 0;
	}

	if (options->output_path != NULL) {
		free(options->output_path);
	}

	options->output_path = strdup(optarg);
	assert(options->output_path != NULL);

	return 1;
}

void fn_options_copy_extra_args(struct FnOptions* options, int optind, int argc, char **argv) {
	int index;

	if (options == NULL) {
		return;
	}

	if (optind == argc) {
		return;
	}

	fn_options_free_files(options);

	options->files_num = argc - optind;
	options->files = (char**) malloc(sizeof(char*) * options->files_num);

	for (index = optind; index < argc; index++) {
		options->files[index - optind] = strdup(argv[index]);
		assert(options->files[index - optind] != NULL);
	}
}

struct FnOptions* parse_options (int argc, char **argv) {
	int c;
	struct FnOptions* options = fn_options_new();

	while ((c = getopt (argc, argv, "o:ht:s")) != -1) {
		switch (c) {
			case 'o':
				if (fn_options_copy_output_path(options, optarg) == 0) {
					fprintf(stderr, "error copying output path\n");
				}

				break;
			case 'h':
				show_usage();
				exit(EXIT_SUCCESS);
			case 't':
				if (fn_options_copy_output_type(options, optarg) == 0) {
					fprintf(stderr, "error copying output type\n");
				}

				break;
			case 's':
				run_shell(dirname(dirname(argv[0])));
				exit(EXIT_SUCCESS);
			case '?':
				if (optopt == 'o') {
					fprintf(stderr, "output path option requires an argument\n");
				}
				else if (optopt == 't') {
					fprintf(stderr, "output type option requires an argument\n");
				}
				else if (isprint(optopt)) {
					fprintf(stderr, "Unknown option `-%c'.\n", optopt);
				}
				else {
					fprintf(stderr, "Unknown option character `\\x%x'.\n", optopt);
				}

				exit(EXIT_FAILURE);
		}
	}

	fn_options_copy_extra_args(options, optind, argc, argv);

	return options;
}

int main (int argc, char **argv) {
	struct FnOptions* options = parse_options(argc, argv);

	fn_options_print(options);
	fn_options_delete(options);
	return 0;
}
