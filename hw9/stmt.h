#ifndef __STMT__
#define __STMT__
#include "refobj.h"
#include "expr.h"
#include "list.h"

typedef struct stmt {
    refobj_t ref;   //ref is at the beginning of stmt
    void   ( *exec  )(struct stmt *self);
    void   ( *print )(struct stmt *self);
} stmt_t;

typedef struct stmt_list_entry {
    stmt_t *stmt;
    list_t lst;
} stmt_list_entry_t;

extern stmt_t *stmt_make_assignment(char *id, expr_t *rhs);
extern stmt_t *stmt_make_read(char *id);
extern stmt_t *stmt_make_write(expr_t *expr);
extern stmt_t *stmt_make_compound(list_t *stmt_list_head);

//TODO: implement if and while statements
extern stmt_t *stmt_make_if(expr_t *cond, stmt_t *then_stmt, stmt_t *else_stmt);
extern stmt_t *stmt_make_while(expr_t *cond, stmt_t *loop_stmt);

#endif
