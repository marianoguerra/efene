#ifndef __FNC_H__
#define __FNC_H__

struct FnOptions {
	char *output_path;
	int files_num;
	char **files;
	char *output_type;
	int is_eval;
	int is_erl_eval;
};

#endif
