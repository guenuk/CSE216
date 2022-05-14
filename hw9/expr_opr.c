//
// expr_opr.c
//  common functions for binary operators
//

#include "expr.h"
#include "expr_opr.h"

int expr_eval_opr(expr_t *self, int (*opr)(int a, int b)) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_EXPR_OPR,
                  strmsg("tag (%d) is not OBJ_EXPR_OPR", self->ref.tag));
                  
    expr_opr_t *expr = (expr_opr_t*) self;
    int a = expr->a->eval(expr->a);
    int b = expr->b->eval(expr->b);
    int c = opr(a, b); //add, sub, mul, div, etc will be passed for opr
    return c;
}

void expr_print_opr(expr_t *self, char *opr) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_EXPR_OPR,
                  strmsg("tag (%d) is not OBJ_EXPR_OPR", self->ref.tag));

    expr_opr_t *expr = (expr_opr_t*) self;
    expr->a->print(expr->a);
    printf("%s", opr);
    expr->b->print(expr->b);
}

void expr_release_opr(refobj_t *ref) {
    ON_FALSE_EXIT(ref->tag == OBJ_EXPR_OPR,
                  strmsg("tag (%d) is not OBJ_EXPR_OPR", ref->tag));    

    expr_opr_t *expr = (expr_opr_t*)ref;
    refobj_decref(&expr->ref);
    if(expr->ref.cnt_ref == 0) {
        expr->a->ref.release(&expr->a->ref);
        expr->b->ref.release(&expr->b->ref);
        refobj_free(&expr->ref);
    }
}

expr_opr_t *expr_make_opr(expr_t *a, expr_t *b) {
    expr_opr_t* expr = refobj_alloc(OBJ_EXPR_OPR, sizeof(expr_opr_t));
    expr->ref.release = expr_release_opr;

    expr->a  = a;
    expr->b  = b;
    expr->a->ref.addref(&expr->a->ref);
    expr->b->ref.addref(&expr->b->ref);

    return expr;
}
