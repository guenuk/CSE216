//
// complex.c
//
#include "complex.h"
#include "rect.h"
#include "polar.h"
#include <stdio.h>

//TODO: implement add using re and im; return rect_t
complex_t* complex_add(complex_t *a, complex_t *b) {
    
    double nr1 = a->re(a);
    double nr2 = b->re(b); 
    double ni1 = a->im(a);
    double ni2 = b->im(b); 
    
    return rect_make(nr1+nr2, ni1+ni2);
}

//TODO: implement sub using re and im; return rect_t
complex_t* complex_sub(complex_t *a, complex_t *b) {
    
    double nr1 = a->re(a);
    double nr2 = b->re(b);
    double ni1 = a->im(a);
    double ni2 = b->im(b);

    return rect_make(nr1-nr2, ni1-ni2);
    
}

//TODO: implement mul using mag and ang; return polar_t
complex_t* complex_mul(complex_t *a, complex_t *b) {
    double nm1 = a->mag(a);
    double nm2 = b->mag(b);
    double na1 = a->ang(a);
    double na2 = b->ang(b);
    return polar_make(nm1*nm2, na1+na2);
    
}

//TODO: implement div using mag and ang; return polar_t
complex_t* complex_div(complex_t *a, complex_t *b) {
    double nm1 = a->mag(a);
    double nm2 = b->mag(b);
    double na1 = a->ang(a);
    double na2 = b->ang(b);
    return polar_make(nm1/nm2, na1-na2);
    
}
