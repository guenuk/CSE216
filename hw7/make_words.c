#include "make_words.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//whether c is an alphabet
static int is_alpha(char c) {
    return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z';
}

//TODO: return the number of words in str
int word_count(char* str) { 
    int ret=1;
    for(; *str; str++){
        if(is_alpha(*str)==0 && is_alpha(*(str+1))==1){
            ret++;
        }
    }
    return ret;
}

//TODO: return the array of words in str and the number of words
void make_words(char *str, char ***pwords, int *pwc) {

    int wc = word_count(str);
    char **words = malloc(wc * sizeof(char*));
    int i=0;
    char *begin;
    int count = 0;
    for(; *str; str++){
        if(is_alpha(*str)==1){
            if(is_alpha(*(str-1))==0){
                begin = str;
            }
            count++;
            if(is_alpha(*(str+1))==0){
                words[i] = malloc(count *sizeof(char));
                strncpy(words[i], begin, count);
                i++;
            }
        }
        else{
            count=0;
        }
    }

    *pwords = words;
    *pwc = wc;

}

//print words
void print_words(char **words, int wc) {
    int i;
    printf("--print words----------\n");
    for(i = 0; i < wc; i++)
        printf("%s\n", words[i]);
}

