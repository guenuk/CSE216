//
// polar.h
//
#ifndef __POLAR__
#define __POLAR__

#include "complex.h"

typedef struct polar {
    //struct complex
    refobj_t ref;
    
    double ( *re  )(struct complex *self);
    double ( *im  )(struct complex *self);
    double ( *mag )(struct complex *self);
    double ( *ang )(struct complex *self);
    int ( *tostr )(char *buf, struct complex *self);

    //struct polar specific
    double m;
    double a;
} polar_t;

extern complex_t* polar_make(double m, double a);
extern double deg2rad(double deg);
extern double rad2deg(double rad);

#endif
