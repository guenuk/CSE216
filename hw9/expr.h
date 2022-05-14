//
// expr.h
//
#ifndef __EXPR__
#define __EXPR__

#include "refobj.h"

typedef struct expr {
    refobj_t ref;   //ref is at the beginning of rat
    int    ( *eval  )(struct expr *self);
    void   ( *print )(struct expr *self);
} expr_t;


//number
extern expr_t *expr_make_num(int n);

//variable
extern expr_t *expr_make_var(char *id);

//arithmetic operators
extern expr_t *expr_make_add(expr_t *a, expr_t *b);
extern expr_t *expr_make_sub(expr_t *a, expr_t *b);
extern expr_t *expr_make_mul(expr_t *a, expr_t *b);
extern expr_t *expr_make_div(expr_t *a, expr_t *b);

//comparators
//TODO: implement comparators
extern expr_t *expr_make_eq(expr_t *a, expr_t *b);
extern expr_t *expr_make_ne(expr_t *a, expr_t *b);
extern expr_t *expr_make_ge(expr_t *a, expr_t *b);
extern expr_t *expr_make_gt(expr_t *a, expr_t *b);
extern expr_t *expr_make_le(expr_t *a, expr_t *b);
extern expr_t *expr_make_lt(expr_t *a, expr_t *b);

#endif
