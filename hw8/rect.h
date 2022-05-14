//
// rect.h
//
#ifndef __RECT__
#define __RECT__

#include "complex.h"

typedef struct rect {
    //struct complex
    refobj_t ref;
    double ( *re  )(struct complex *self);
    double ( *im  )(struct complex *self);
    double ( *mag )(struct complex *self);
    double ( *ang )(struct complex *self);
    int ( *tostr )(char *buf, struct complex *self);

    //struct rect specific
    double r;
    double i;
} rect_t;

extern complex_t* rect_make(double r, double i);

#endif
