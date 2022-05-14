//
// stmt_write.c
//  write statement
//

#include "common.h"
#include "refobj.h"
#include "expr.h"
#include "stmt.h"
#include <stdio.h>

typedef struct stmt_write {
    refobj_t ref;   //ref is at the beginning of stmt
    void   ( *exec  )(struct stmt *self);
    void   ( *print )(struct stmt *self);

    expr_t *expr;
} stmt_write_t;

static void exec_write(stmt_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_WRITE,
                  strmsg("tag (%d) is not OBJ_STMT_WRITE", self->ref.tag));

    stmt_write_t *stmt = (stmt_write_t*) self;
    int val = stmt->expr->eval(stmt->expr);
    printf("ans: %d\n", val);
}

static void print_write(stmt_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_WRITE,
                  strmsg("tag (%d) is not OBJ_STMT_WRITE", self->ref.tag));

    stmt_write_t *stmt = (stmt_write_t*) self;
    printf("write ");
    stmt->expr->print(stmt->expr);
    printf("\n");
}

static void release_write(refobj_t *ref) {
    ON_FALSE_EXIT(ref->tag == OBJ_STMT_WRITE,
                  strmsg("tag (%d) is not OBJ_STMT_WRITE", ref->tag));
                  
    stmt_write_t *stmt = (stmt_write_t*) ref;
    refobj_decref(&stmt->ref);
    if(stmt->ref.cnt_ref == 0) {
        stmt->expr->ref.release(&stmt->expr->ref);
        refobj_free(&stmt->ref);
    }
}

stmt_t *stmt_make_write(expr_t *expr) {
    stmt_write_t* stmt = refobj_alloc(OBJ_STMT_WRITE, sizeof(stmt_write_t));
    stmt->ref.release  = release_write;
    stmt->exec         = exec_write;
    stmt->print        = print_write;

    stmt->expr = expr;
    expr->ref.addref(&expr->ref);
    return (stmt_t*) stmt;
}
