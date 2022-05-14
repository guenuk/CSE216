#ifndef __COMPLEX__
#define __COMPLEX__

#include "refobj.h"

typedef struct complex {
    refobj_t ref;
    double ( *re  )(struct complex *self);
    double ( *im  )(struct complex *self);
    double ( *mag )(struct complex *self);
    double ( *ang )(struct complex *self);
    int ( *tostr )(char *buf, struct complex *self);
} complex_t;

extern complex_t* complex_add(complex_t *a, complex_t *b);
extern complex_t* complex_sub(complex_t *a, complex_t *b);
extern complex_t* complex_mul(complex_t *a, complex_t *b);
extern complex_t* complex_div(complex_t *a, complex_t *b);

#endif
