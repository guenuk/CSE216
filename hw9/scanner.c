#include "regex.h"
#include "common.h"
#include "scanner.h"
#include <string.h>

#ifndef MAC
//forward definitions
static int read_token_from_str(regex_t *regexp, char *str, int *token, char *tok_text);
static int token_text(regmatch_t *match, char *str, char *tok_text);

void open_scan(scan_t *sc, char *fname) {
    sc->line = 0;
    sc->token = 0;
    sc->buff[0] = 0;
    sc->pos = sc->buff;
    sc->text[0] = 0;
    sc->fp = stdin;

    //initialize regexp
    int res = regcomp(&sc->regexp, TOK_REGEXP, REG_EXTENDED);
    if(res != 0) {
        char msg[256];
        regerror(res, &sc->regexp, msg, sizeof(msg));
        ON_FALSE_EXIT(0, strmsg("error in regcomp: %d, %s", res, msg));
    }

    //initialize fp
    if(fname && fname[0]) {
        sc->fp = fopen(fname, "rt");
        ON_FALSE_EXIT(sc->fp != NULL, strmsg("cannot open %s", fname));
    }
}

void close_scan(scan_t *sc) {
    //free regexp
    regfree(&sc->regexp);

    //close fp
    if(sc->fp == NULL || sc->fp != stdin) {
        fclose(sc->fp);
        sc->fp = NULL;
    }
}

//read token and return 0 if it is EOF
int read_token(scan_t *sc) {
    while(1) {
        //read a line from the file
        if(*sc->pos == '\0') {
            char *res = fgets(sc->buff, sizeof(sc->buff), sc->fp);
            if(res == NULL) { //EOF
                sc->token = TOK_EOF;
                strcpy(sc->text, "EOF");
                return 0;
            }
            sc->pos = sc->buff; //reset pos to the beginnng of buff
            sc->line++;         //increate the line count
        }

        //read a token
        sc->pos += read_token_from_str(&sc->regexp, sc->pos, &sc->token, sc->text);

        //skip a white spaces or a comment
        if(sc->token == TOK_WS ||
           sc->token == TOK_COMMENT)
            continue;

        //return with the current token
        return 1;
    }
}

//return the length of tok_text
static int read_token_from_str(regex_t *regexp, char *str, int *token, char *tok_text) {
    regmatch_t matches[TOK_COUNT];

    int res = regexec(regexp, str, TOK_COUNT, matches, 0);
    if(res == 0) {  //if there is a match
        int i = 0;
        for(i = 1; i < TOK_COUNT; i++) {
            if(matches[i].rm_so != 0)
                continue;

            *token = i + TOK_ALL;
            return token_text(&matches[i], str, tok_text);
        }
    }
    else {  //no match -> punctuators
        *token = str[0];
        tok_text[0] = str[0];
        tok_text[1] = 0;
        return 1;
    }
}

//copy the token text to tok_text
static int token_text(regmatch_t *match, char *str, char *tok_text) {
    int i = 0, j = match->rm_so;
    while(j < match->rm_eo)
        tok_text[i++] = str[j++];
    tok_text[i] = 0;

    return match->rm_eo - match->rm_so;
}

#define SCANNER_APP 0
#if SCANNER_APP
int main(int argc, char **argv) {
    scan_t sc;
    if(argc == 1)
        open_scan(&sc, NULL);
    else if(argc == 2)
        open_scan(&sc, argv[1]);
    else
        ON_FALSE_EXIT(0, "Usage: a.exe [program_file]")

    while(read_token(&sc))
        printf("%s:%d ", sc.text, sc.token);

    close_scan(&sc);

    return 0;
}
#endif
#endif
