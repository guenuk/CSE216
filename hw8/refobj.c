//
// refobj.c
//
#include "refobj.h"
#include "common.h"
#include <stdlib.h>

//statistics
refstat_t stat;

//forward definitions
static void addref_refobj(refobj_t *ref);
static void release_refobj(refobj_t *ref);

//allocate container and init ref obj
void *refobj_alloc(tag_t tag, size_t size) {
    char *obj = calloc(1, size);
    refobj_t *ref = (refobj_t*)obj;
    ref->tag = tag;
    ref->cnt_ref = 1;
    ref->addref = addref_refobj;
    ref->release = release_refobj;
    stat.cnt_obj[ref->tag]++;
    stat.cnt_ref[ref->tag]++;
    return obj;
}

//free the container of ref
void refobj_free(refobj_t *ref) {
    char* obj = (char*)ref;
    ON_FALSE_EXIT(ref->cnt_ref == 0,
        strmsg("destroying live refobj, cnt_ref: %d", ref->cnt_ref));
    stat.cnt_obj[ref->tag]--;
    free(obj);
}

//increase cnt_ref of ref
void refobj_incref(refobj_t *ref) {
    ON_FALSE_EXIT(ref->cnt_ref > 0,
        strmsg("nonpositive cnt_ref: %d", ref->cnt_ref));
    ref->cnt_ref++;
    stat.cnt_ref[ref->tag]++;
}

//decrease cnt_ref of ref
void refobj_decref(refobj_t *ref) {
    ON_FALSE_EXIT(ref->cnt_ref > 0,
        strmsg("nonpositive cnt_ref: %d", ref->cnt_ref));
    ref->cnt_ref--;
    stat.cnt_ref[ref->tag]--;
}

//check whether all refobjs are deallocated correctly
void refobj_check_dealloc() {
    int i, allzero = 1;

    for(i = 0; i < OBJ_COUNT; i++) {
        allzero &= stat.cnt_ref[i] == 0;
        allzero &= stat.cnt_obj[i] == 0;
    }

    if(!allzero) {
        fprintf(stderr, "cnt_ref = [ ");
        for(i = 0; i < OBJ_COUNT; i++)
            fprintf(stderr, "%d, ", stat.cnt_ref[i]);
        fprintf(stderr, "]\n");

        fprintf(stderr, "cnt_obj = [ ");
        for(i = 0; i < OBJ_COUNT; i++)
            fprintf(stderr, "%d, ", stat.cnt_obj[i]);
        fprintf(stderr, "]\n");

        ON_FALSE_EXIT(0, "error: deallocating refobjs");
    }
}

//default reference counting
static void addref_refobj(refobj_t *ref) {
    refobj_incref(ref);
}

//default reference counting
static void release_refobj(refobj_t *ref) {
    refobj_decref(ref);
    if(ref->cnt_ref == 0)
        refobj_free(ref);
}
