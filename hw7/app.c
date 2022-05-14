#include "make_words.h"
#include "sort_words.h"
#include "freq_words.h"
#include <stdio.h>

int main() {

    char *str = 
        "Eeny, meeny, miny, moe,"
        "Catch a tiger by the toe."
        "If he hollers, let him go,"
        "Eeny, meeny, miny, moe.";
        
    char **words;               //words
    word_freq_t *word_freqs;    //word frequencies
    int wc, uwc;                //word count, unique word count

    //make words from str
    make_words(str, &words, &wc);
    print_words(words, wc);
    printf("word count: %d\n", word_count(str));

    // //sort words
    sort_words(words, wc);
    print_words(words, wc);
    printf("word count: %d\n", word_count(str));

    //count the frequencies of words
    make_word_freqs(words, wc, &word_freqs, &uwc);
    print_word_freqs(word_freqs, uwc);
    printf("unique word count: %d\n", unique_word_count(words, wc));

    return 0;
}
