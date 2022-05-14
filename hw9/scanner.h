#ifndef __SCANNER__
#define __SCANNER__

#include "regex.h"
#include <stdio.h>

enum tokens {
    TOK_ALL = 256,  //match for the whole expr
    TOK_WS,         //white space
    TOK_COMMENT,    //comment
    TOK_READ,       //read
    TOK_WRITE,      //write
    TOK_IF,         //if
    TOK_ELSE,       //else
    TOK_WHILE,      //while
    TOK_ASSIGN,     //assignment
    TOK_EQ,         //equal
    TOK_NE,         //not equal
    TOK_LE,         //less  than or equal to
    TOK_GE,         //greater than or equal to
    TOK_NUM,        //number
    TOK_ID,         //identifier
    TOK_EOF,        //end of file
    TOK_COUNT = (TOK_EOF - TOK_ALL + 1) //token count
};

#define TOK_REGEXP  "^([ \t\n\r]+)|"                /*whitespace*/\
                    "^(//[^\n]*\n)|"                /*comment*/\
                    "^(read)|"                      /*read*/\
                    "^(write)|"                     /*write*/\
                    "^(if)|"                        /*if*/\
                    "^(else)|"                      /*else*/\
                    "^(while)|"                     /*while*/\
                    "^(:=)|"                        /*assignment*/\
                    "^(==)|"                        /*EQ*/\
                    "^(!=)|"                        /*NE*/\
                    "^(<=)|"                        /*LE*/\
                    "^(>=)|"                        /*GE*/\
                    "^([0-9]+)|"                    /*number*/\
                    "^([_a-zA-Z][_a-zA-Z0-9]*)"     /*identifier*/

typedef struct scan {
    regex_t regexp;     //regular expression
    int token;          //current token
    char buff[1024];    //current line
    char *pos;          //current position in the buffer
    char text[256];     //text for the current token
    int line;
    FILE *fp;           //file pointer
} scan_t;

//open a scanner
extern void open_scan(scan_t *sc, char *fname);

//close a scanner
extern void close_scan(scan_t *sc);

//read token and return 0 if it is EOF
extern int read_token(scan_t *sc);

#endif