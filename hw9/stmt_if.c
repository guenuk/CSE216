//
// stmt_if.c
//  if statement
//

#include "common.h"
#include "refobj.h"
#include "expr.h"
#include "stmt.h"
#include <stdio.h>

typedef struct stmt_if {
    refobj_t ref;   //ref is at the beginning of stmt
    void   ( *exec  )(struct stmt *self);
    void   ( *print )(struct stmt *self);

    expr_t *cond;
    stmt_t *then_stmt;
    stmt_t *else_stmt;
} stmt_if_t;

/*  TODO: implement 
*/
    static void exec_if(stmt_t *self){
        ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_IF,
                  strmsg("tag (%d) is not OBJ_STMT_IF", self->ref.tag));
        stmt_if_t *stmt = (stmt_if_t*) self; 
        if(stmt->cond->eval(stmt->cond)){
            stmt->then_stmt->exec(stmt->then_stmt);
        }
        else{
            stmt->else_stmt->exec(stmt->else_stmt);
        }
    }
    

    static void print_if(stmt_t *self){
        ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_IF,
                  strmsg("tag (%d) is not OBJ_STMT_IF", self->ref.tag));
        stmt_if_t *stmt = (stmt_if_t*) self; 
        printf("if ( ");
        stmt->cond->print(stmt->cond); //
        printf(" ) \n"); //
        stmt->then_stmt->print(stmt->then_stmt); //
        printf("else \n"); //
        stmt->else_stmt->print(stmt->else_stmt); //

    }
    static void release_if(refobj_t *ref){
        ON_FALSE_EXIT(ref->tag == OBJ_STMT_IF,
                  strmsg("tag (%d) is not OBJ_STMT_IF", ref->tag));
        stmt_if_t *stmt = (stmt_if_t*) ref; 
        refobj_decref(&stmt->ref);
        if(stmt->ref.cnt_ref == 0) {
            stmt->cond->ref.release(&stmt->cond->ref);
            stmt->then_stmt->ref.release(&stmt->then_stmt->ref);
            stmt->else_stmt->ref.release(&stmt->else_stmt->ref);
            refobj_free(&stmt->ref);
        }

    }
    
    stmt_t *stmt_make_if(expr_t *cond, stmt_t *then_stmt, stmt_t *else_stmt){
        stmt_if_t* stmt = refobj_alloc(OBJ_STMT_IF, sizeof(stmt_if_t));
        stmt->ref.release = release_if;
        stmt->exec = exec_if;
        stmt->print= print_if;

        stmt->cond= cond;
        cond->ref.addref(&cond->ref);   //

        stmt->then_stmt = then_stmt; 
        then_stmt->ref.addref(&then_stmt->ref); //

        stmt-> else_stmt = else_stmt;
        else_stmt->ref.addref(&else_stmt->ref); // 


        return (stmt_t*) stmt; 
    }



