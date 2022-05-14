#ifndef __MAKE_WORDS__
#define __MAKE_WORDS__

//TODO: add function declarations.

static int is_alpha(char c);
int word_count(char* str);
void make_words(char *str, char ***pwords, int *pwc);
void print_words(char **words, int wc);


#endif

