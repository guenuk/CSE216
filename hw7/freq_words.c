#include "freq_words.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//TODO: return the number of unique words in sorted_words
int unique_word_count(char **sorted_words, int wc) {
    /*hint: because the words are sorted, duplicated words are grouped together
            skip counting the duplicated words*/
    int ret = 0;
    for(int i=0; i<wc; i++){
        int temp = 0;
        for(int j= i+1; j<wc; j++){
            if(strcmp(sorted_words[i], sorted_words[j])==0){
                temp++;
            }
        }
        if(temp==0){
            ret++;
        } 
        else{
            ret++;
            i += temp;
        }  
    }
    return ret;
}

//TODO: return the array of word frequencies (the number of occurrences)
//      and the number of unique words in sorted_words
void make_word_freqs(char **sorted_words, int wc, word_freq_t **pword_freqs, int *puwc) {
    int uwc = unique_word_count(sorted_words, wc);
    word_freq_t *word_freqs = malloc(uwc * sizeof(word_freq_t)); /*hint: using malloc, allocate word_freq_t array of uwc elements*/
    if(wc>0){
        // int j=0;
        // for(int i=1; i<wc; i++){
        //     // word_freqs[i] = malloc(sizeof(word_freq_t));
        //     if(strcmp(sorted_words[i],sorted_words[i-1])==0){
        //         word_freqs[j].word=sorted_words[i];
        //         int temp = 1;
        //         for(;i<wc;i++){
        //             if(strcmp(sorted_words[i],sorted_words[i-1])==0){
        //                 temp++;
        //             }
        //             else{
        //                 word_freqs[j].freq = temp;
        //                 j++;
        //                 i--;
        //                 break;
        //             }
        //         }
        //     }
        //     else{
        //         word_freqs[j].word= sorted_words[i];
        //         word_freqs[j].freq=1;
        //         j++;
        //     }
        // }
        int ind = 0;
        for(int i=0; i<wc; i++){
            int temp = 0;
            for(int j= i+1; j<wc; j++){
                if(strcmp(sorted_words[i], sorted_words[j])==0){
                    temp++;
                }   
            }
            word_freqs[ind].word=sorted_words[i];
            word_freqs[ind].freq= temp+1;
            ind++;
            if(temp!=0){
                i += temp;
            } 
        }
    }    
    /*hint: if wc > 0, initialize word_freqs[0]
            for i = 1 .. wc-1,
                       update the current word_freqs if sorted_words[i] is equal to
                                             sorted_words[i-1]
            initialize a new word_freqs   otherwise
    */


    *pword_freqs = word_freqs;
    *puwc = uwc;
}

//print the frequency of words
void print_word_freqs(word_freq_t *word_freqs, int uwc) {
    int i;
    printf("--print freqs----------\n");
    for(i = 0; i < uwc; i++)
        printf("%s: %d\n", word_freqs[i].word, word_freqs[i].freq);
}
