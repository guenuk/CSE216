//
// expr_var.c
//  variables
//

#include "expr.h"
#include "varstore.h"
#include <string.h>

typedef struct expr_var {
    refobj_t ref;   //first part is the same as expr_t
    int    ( *eval  )(struct expr *self);
    void   ( *print )(struct expr *self);

    char *id;       //variable
} expr_var_t;

static int eval_var(expr_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_EXPR_VAR,
                  strmsg("tag (%d) is not OBJ_EXPR_VAR", self->ref.tag));

    expr_var_t *expr = (expr_var_t*) self;
    return var_store_get(expr->id);
}

static void print_var(expr_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_EXPR_VAR,
                  strmsg("tag (%d) is not OBJ_EXPR_VAR", self->ref.tag));
                  
    expr_var_t *expr = (expr_var_t*) self;
    printf("%s", expr->id);
}

static void release_var(refobj_t *ref) {
    ON_FALSE_EXIT(ref->tag == OBJ_EXPR_VAR,
                  strmsg("tag (%d) is not OBJ_EXPR_VAR", ref->tag));    

    expr_var_t *expr = (expr_var_t*) ref;
    refobj_decref(&expr->ref);
    if(expr->ref.cnt_ref == 0) {
        free(expr->id);
        refobj_free(&expr->ref);
    }
}

expr_t *expr_make_var(char *id) {
    expr_var_t* expr  = refobj_alloc(OBJ_EXPR_VAR, sizeof(expr_var_t));
    expr->ref.release = release_var;
    expr->eval        = eval_var;
    expr->print       = print_var;

    expr->id  = strdup(id);
    return (expr_t*)expr;
}

