#include "common.h"
#include "scanner.h"
#include <string.h>

#ifdef MAC
//forward definitions
static int read_token_from_str(char *str, int *token, char *tok_text);

void open_scan(scan_t *sc, char *fname) {
    sc->line = 0;
    sc->token = 0;
    sc->buff[0] = 0;
    sc->pos = sc->buff;
    sc->text[0] = 0;
    sc->fp = stdin;

    printf("MAC\n");
    //initialize fp
    if(fname && fname[0]) {
        sc->fp = fopen(fname, "rt");
        ON_FALSE_EXIT(sc->fp != NULL, strmsg("cannot open %s", fname));
    }
}

void close_scan(scan_t *sc) {
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
        sc->pos += read_token_from_str(sc->pos, &sc->token, sc->text);

        //skip a white spaces or a comment
        if(sc->token == TOK_WS ||
           sc->token == TOK_COMMENT)
            continue;

        //return with the current token
        return 1;
    }
}

static int is_whitespace(char c) {
    return  c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static int is_alpha_(char c) {
    return  'a' <= c && c <= 'z' ||
            'A' <= c && c <= 'Z' ||
            c == '_';
}

static int is_number(char c) {
    return  '0' <= c && c <= '9';
}

static int is_alnum_(char c) {
    return is_alpha_(c) || is_number(c);
}


//return the length of tok_text
static int read_token_from_str(char *str, int *token, char *tok_text) {
    char *keywords[10] = {
        "read", "write", "if", "else", "while", ":=", "==", "!=", "<=", ">="
    };
    int i;
    for(i = 0; i < 10; i++) {
        if(strncmp(str, keywords[i], strlen(keywords[i])) == 0) {
            *token = TOK_READ + i;
            strcpy(tok_text, keywords[i]);
            return strlen(tok_text);
        }
    }
    if(is_whitespace(*str)) {
        *token = TOK_WS;
        for(i = 0; str[i] && is_whitespace(str[i]); i++)
            tok_text[i] = str[i];
        tok_text[i] = 0;
    }
    else if(strncmp(str, "//", 2) == 0) {
        *token = TOK_COMMENT;
        tok_text[0] = tok_text[1] = '/';
        for(i = 2; str[i] && str[i] != '\n'; i++)
            tok_text[i] = str[i];
        tok_text[i] = 0;
    }
    else if(is_alpha_(*str)) {
        *token = TOK_ID;
        for(i = 0; str[i] && is_alnum_(str[i]); i++)
            tok_text[i] = str[i];
        tok_text[i] = 0;
    }
    else if(is_number(*str)) {
        *token = TOK_NUM;
        for(i = 0; str[i] && is_number(str[i]); i++)
            tok_text[i] = str[i];
        tok_text[i] = 0;
    }
    else {  //no match -> punctuators
        *token = str[0];
        tok_text[0] = str[0];
        tok_text[1] = 0;
    }
    return strlen(tok_text);
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