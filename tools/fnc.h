// New BSD License, part of efene, see LICENSE for details
#ifndef __FNC_H__
#define __FNC_H__

struct FnOptions {
	char *output_path;
	int files_num;
	char **files;
	char *output_type;
	char mode;
	int is_test;
	char *appends;
	char *prepends;
};

#endif
