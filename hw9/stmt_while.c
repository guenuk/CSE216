//
// stmt_while.c
//  while statement
//

#include "common.h"
#include "refobj.h"
#include "expr.h"
#include "stmt.h"
#include <stdio.h>

typedef struct stmt_while {
    refobj_t ref;   //ref is at the beginning of stmt
    void   ( *exec  )(struct stmt *self);
    void   ( *print )(struct stmt *self);

    expr_t *cond;
    stmt_t *loop_stmt;
} stmt_while_t;

/*  TODO: implement*/

static void exec_while(stmt_t *self){
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_WHILE, strmsg("tag (%d) is not OBJ_STMT_WHILE", self->ref.tag));
    stmt_while_t *stmt = (stmt_while_t*) self;
    //
    while(stmt->cond->eval(stmt->cond)){
        stmt->loop_stmt->exec(stmt->loop_stmt);
        
    }
    
}
static void print_while(stmt_t *self){
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_WHILE, strmsg("tag (%d) is not OBJ_STMT_WHILE", self->ref.tag));
    stmt_while_t *stmt = (stmt_while_t*) self;
        //
    printf("while ( ");
    stmt->cond->print(stmt->cond);
    printf(" ) \n");
    stmt->loop_stmt->print(stmt->loop_stmt);
}
static void release_while(refobj_t *ref){
    ON_FALSE_EXIT(ref->tag == OBJ_STMT_WHILE,
                  strmsg("tag (%d) is not OBJ_STMT_WHILE", ref->tag));
        stmt_while_t *stmt = (stmt_while_t*) ref;
        refobj_decref(&stmt->ref);
        if(stmt->ref.cnt_ref == 0) {
            stmt->cond->ref.release(&stmt->cond->ref);
            stmt->loop_stmt->ref.release(&stmt->loop_stmt->ref);
            refobj_free(&stmt->ref);
        }
}
stmt_t *stmt_make_while(expr_t *cond, stmt_t *loop_stmt){
    stmt_while_t* stmt = refobj_alloc(OBJ_STMT_WHILE, sizeof(stmt_while_t));
    stmt->ref.release = release_while;
    stmt->exec = exec_while; 
    stmt->print= print_while;

    stmt->cond= cond;
    cond->ref.addref(&cond->ref);   //

    stmt->loop_stmt = loop_stmt; 
    loop_stmt->ref.addref(&loop_stmt->ref); //
    
    return (stmt_t*) stmt; 
}

