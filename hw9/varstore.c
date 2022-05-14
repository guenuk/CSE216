//
// varstore.c
//  variable store
//
#include "common.h"
#include "list.h"
#include "varstore.h"
#include <stdio.h>
#include <string.h>

typedef struct var_entry {
    char *name;
    int value;

    list_t lst;
} var_entry_t;

//forward definitions
static void var_store_add(char *name, int value);
static var_entry_t * var_store_find(char *name);

static list_t var_list_head;

void var_store_init() {
    list_init_head(&var_list_head);
}

void var_store_destroy() {
    list_t *pos;
    for(pos = var_list_head.next; pos != &var_list_head; ) {
        var_entry_t *var = containerof(pos, var_entry_t, lst);
        pos = pos->next;
        free(var);
    }
}

int var_store_get(char *name) {
    var_entry_t *var = var_store_find(name);
    ON_FALSE_EXIT(var != NULL, strmsg("cannot find variable %s", name));
    return var->value;
}

void var_store_set(char *name, int value) {
    var_entry_t *var = var_store_find(name);
    if(var != NULL)
        var->value = value;
    else 
        var_store_add(name, value);
}

static void var_store_add(char *name, int value) {
    var_entry_t *var = malloc(sizeof(var_entry_t));
    var->name = name;
    var->value = value;
    list_add_to_last(&var_list_head, &var->lst);
}

static var_entry_t * var_store_find(char *name) {
    list_t *pos;
    for(pos = var_list_head.next; pos != &var_list_head; pos = pos->next) {
        var_entry_t *var = containerof(pos, var_entry_t, lst);
        if(strcmp(var->name, name) == 0)
            return var;
    }
    return NULL;
}

