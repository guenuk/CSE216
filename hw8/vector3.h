//
// vector3.h
//
#ifndef __VECTOR3__
#define __VECTOR3__

#include "complex.h"

typedef struct vec3 {
    refobj_t ref;
    int ( *tostr )(char *buf, struct vec3 *self);
    complex_t *x, *y, *z;    
} vec3_t;

extern vec3_t *vec3_make(complex_t *x, complex_t *y, complex_t *z);
extern vec3_t *vec3_add(vec3_t *a, vec3_t *b);
extern vec3_t *vec3_sub(vec3_t *a, vec3_t *b);
extern vec3_t *vec3_smul(complex_t *s, vec3_t *a);
extern complex_t *vec3_prod(vec3_t *a, vec3_t *b);

#endif
