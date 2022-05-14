//
// stmt_read.c
//  read statement
//

#include "common.h"
#include "refobj.h"
#include "expr.h"
#include "stmt.h"
#include "varstore.h"
#include <stdio.h>
#include <string.h>

typedef struct stmt_read {
    refobj_t ref;   //ref is at the beginning of stmt
    void   ( *exec  )(struct stmt *self);
    void   ( *print )(struct stmt *self);

    char *id;
} stmt_read_t;

static void exec_read(stmt_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_READ,
                  strmsg("tag (%d) is not OBJ_STMT_READ", self->ref.tag));

    stmt_read_t *stmt = (stmt_read_t*) self;
    printf("enter %s : ", stmt->id);
    int val;
    ON_FALSE_EXIT(scanf("%d", &val) == 1, strmsg("cannot read for %s", stmt->id));
    var_store_set(stmt->id, val);   //save val at id
}

static void print_read(stmt_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_READ,
                  strmsg("tag (%d) is not OBJ_STMT_READ", self->ref.tag));

    stmt_read_t *stmt = (stmt_read_t*) self;
    printf("read %s\n", stmt->id);
}

static void release_read(refobj_t *ref) {
    ON_FALSE_EXIT(ref->tag == OBJ_STMT_READ,
                  strmsg("tag (%d) is not OBJ_STMT_ASSIGN", ref->tag));

    stmt_read_t *stmt = (stmt_read_t*) ref;
    refobj_decref(&stmt->ref);
    if(stmt->ref.cnt_ref == 0) {
        free(stmt->id);
        refobj_free(&stmt->ref);
    }
}

stmt_t *stmt_make_read(char *id) {
    stmt_read_t* stmt = refobj_alloc(OBJ_STMT_READ, sizeof(stmt_read_t));
    stmt->ref.release = release_read;
    stmt->exec        = exec_read;
    stmt->print       = print_read;

    stmt->id = strdup(id);
    return (stmt_t*) stmt;
}
