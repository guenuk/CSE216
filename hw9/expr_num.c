//
// expr_num.c
//  numbers
//
#include "expr.h"

typedef struct expr_num {
    refobj_t ref;   //first part is the same as expr_t
    int    ( *eval  )(struct expr *self);
    void   ( *print )(struct expr *self);

    int n;           //number
} expr_num_t;

static int eval_num(expr_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_EXPR_NUM,
                  strmsg("tag (%d) is not OBJ_EXPR_NUM", self->ref.tag));

    expr_num_t *expr = (expr_num_t*) self;
    return expr->n;
}

static void print_num(expr_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_EXPR_NUM,
                  strmsg("tag (%d) is not OBJ_EXPR_NUM", self->ref.tag));

    expr_num_t *expr = (expr_num_t*) self;
    printf("%d", expr->n);
}

static void release_num(refobj_t *ref) {
    ON_FALSE_EXIT(ref->tag == OBJ_EXPR_NUM,
                  strmsg("tag (%d) is not OBJ_EXPR_NUM", ref->tag));    

    expr_num_t *expr = (expr_num_t*) ref;
    refobj_decref(&expr->ref);
    if(expr->ref.cnt_ref == 0) {
        refobj_free(&expr->ref);
    }
}

expr_t *expr_make_num(int n) {
    expr_num_t* expr  = refobj_alloc(OBJ_EXPR_NUM, sizeof(expr_num_t));
    expr->ref.release = release_num;
    expr->eval        = eval_num;
    expr->print       = print_num;

    expr->n  = n;
    return (expr_t*)expr;
}

