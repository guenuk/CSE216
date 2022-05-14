//
// stmt_assignment.c
//  assignment statement
//

#include "common.h"
#include "refobj.h"
#include "expr.h"
#include "stmt.h"
#include "varstore.h"
#include <stdio.h>
#include <string.h>

typedef struct stmt_assignment {
    refobj_t ref;   //ref is at the beginning of stmt
    void   ( *exec  )(struct stmt *self);
    void   ( *print )(struct stmt *self);

    char *id;
    expr_t *rhs;
} stmt_assignment_t;

static void exec_assignment(stmt_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_ASSIGN,
                  strmsg("tag (%d) is not OBJ_STMT_ASSIGN", self->ref.tag));

    stmt_assignment_t *stmt = (stmt_assignment_t*) self;
    int rhs = stmt->rhs->eval(stmt->rhs);
    var_store_set(stmt->id, rhs); //save the value of rhs at id
}

static void print_assignment(stmt_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_ASSIGN,
                  strmsg("tag (%d) is not OBJ_STMT_ASSIGN", self->ref.tag));

    stmt_assignment_t *stmt = (stmt_assignment_t*) self;
    printf("%s := ", stmt->id);
    stmt->rhs->print(stmt->rhs);
    printf("\n");
}

static void release_assignment(refobj_t *ref) {
    ON_FALSE_EXIT(ref->tag == OBJ_STMT_ASSIGN,
                  strmsg("tag (%d) is not OBJ_STMT_ASSIGN", ref->tag));
                  
    stmt_assignment_t *stmt = (stmt_assignment_t*) ref;
    refobj_decref(&stmt->ref);
    if(stmt->ref.cnt_ref == 0) {
        free(stmt->id);
        stmt->rhs->ref.release(&stmt->rhs->ref);
        refobj_free(&stmt->ref);
    }
}

stmt_t *stmt_make_assignment(char *id, expr_t *rhs) {
    stmt_assignment_t* stmt = refobj_alloc(OBJ_STMT_ASSIGN, sizeof(stmt_assignment_t));
    stmt->ref.release = release_assignment;
    stmt->exec        = exec_assignment;
    stmt->print       = print_assignment;

    stmt->id  = strdup(id);
    stmt->rhs = rhs;
    rhs->ref.addref(&rhs->ref);
    return (stmt_t*) stmt;
}
