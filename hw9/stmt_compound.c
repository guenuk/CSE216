//
// stmt_compound.c
//  compound statement
//

#include "common.h"
#include "refobj.h"
#include "expr.h"
#include "stmt.h"
#include <stdio.h>

typedef struct stmt_compound {
    refobj_t ref;   //ref is at the beginning of stmt
    void   ( *exec  )(struct stmt *self);
    void   ( *print )(struct stmt *self);

    list_t *stmt_list_head;
} stmt_compound_t;

static void exec_compound(stmt_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_COMPOUND,
                  strmsg("tag (%d) is not OBJ_STMT_COMPOUND", self->ref.tag));

    stmt_compound_t *stmt = (stmt_compound_t*) self;

    //execute each stmt in the list
    list_t *pos = stmt->stmt_list_head->next;
    for( ; pos != stmt->stmt_list_head; pos = pos->next) {
        stmt_list_entry_t *entry = containerof(pos, stmt_list_entry_t, lst);
        entry->stmt->exec(entry->stmt); 
    }
}

static void print_compound(stmt_t *self) {
    ON_FALSE_EXIT(self->ref.tag == OBJ_STMT_COMPOUND,
                  strmsg("tag (%d) is not OBJ_STMT_COMPOUND", self->ref.tag));

    stmt_compound_t *stmt = (stmt_compound_t*) self;
    printf("{\n");
    list_t *pos;
    for(pos=stmt->stmt_list_head->next; pos != stmt->stmt_list_head; pos = pos->next) {
        stmt_list_entry_t *entry = containerof(pos, stmt_list_entry_t, lst);
        entry->stmt->print(entry->stmt);
    }
    printf("}\n");
}

static void release_compound(refobj_t *ref) {
    ON_FALSE_EXIT(ref->tag == OBJ_STMT_COMPOUND,
                  strmsg("tag (%d) is not OBJ_STMT_COMPOUND", ref->tag));
                  
    stmt_compound_t *stmt = (stmt_compound_t*) ref;
    refobj_decref(&stmt->ref);
    if(stmt->ref.cnt_ref == 0) {
        list_t *pos;
        for(pos=stmt->stmt_list_head->next; pos != stmt->stmt_list_head; ) {
            stmt_list_entry_t *entry = containerof(pos, stmt_list_entry_t, lst);
            entry->stmt->ref.release(&entry->stmt->ref);
            pos = pos->next;
            free(entry);
        }
        free(stmt->stmt_list_head);
        refobj_free(&stmt->ref);
    }
}

stmt_t *stmt_make_compound(list_t *stmt_list_head) {
    stmt_compound_t* stmt = refobj_alloc(OBJ_STMT_COMPOUND, sizeof(stmt_compound_t));
    stmt->ref.release     = release_compound;
    stmt->exec            = exec_compound;
    stmt->print           = print_compound;

    //addref/release to the elements are skipped
    stmt->stmt_list_head = stmt_list_head;
    return (stmt_t*) stmt;
}

