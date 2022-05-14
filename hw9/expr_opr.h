#ifndef __EXPR_OPR__
#define __EXPR_OPR__

#include "expr.h"

typedef struct expr_opr {
    refobj_t ref;   //first part is the same as expr_t
    int    ( *eval  )(struct expr *self);
    void   ( *print )(struct expr *self);

    struct expr *a;     //operand 1
    struct expr *b;     //operand 2
} expr_opr_t;

//common functions for binary operators
//
extern int expr_eval_opr(expr_t *self, int (*opr)(int a, int b));
extern void expr_print_opr(expr_t *self, char *opr);
extern void expr_release_opr(refobj_t *ref);
extern expr_opr_t *expr_make_opr(expr_t *a, expr_t *b);

#endif