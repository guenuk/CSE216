//
// rect.c
//
#include "complex.h"
#include "rect.h"
#include <math.h>

//TODO: implement re, re( a + bi ) = a
static double re(complex_t *self) {
    rect_t *rec = (rect_t*)self;
    return rec->r;
}

//TODO: implement im, im( a + bi ) = b
static double im(complex_t *self) {
    rect_t *rec = (rect_t*)self;
    return rec->i;
}

//TODO: implement mag, mag( a + bi ) = sqrt(a*a + b*b)
static double mag(complex_t *self) {
    rect_t *rec = (rect_t*)self;
    return sqrt(rec->r*rec->r + rec->i*rec->i);
}

//TODO: implement ang, ang( a + bi ) = atan2(b, a)
static double ang(complex_t *self) {
    rect_t *rec = (rect_t*)self;
    return atan2(rec->i, rec->r);
}

//write a + bi to buf
static int tostr(char *buf, complex_t *self) {
    rect_t *rec = (rect_t*)self;
    ON_FALSE_EXIT(self->ref.tag == OBJ_RECT, strmsg("not rect type"));
    return sprintf(buf, "%g + %g i", rec->r, rec->i);
}

//TODO: implement rect_make
complex_t* rect_make(double r, double i) {
    rect_t* rec = refobj_alloc(OBJ_RECT, sizeof(rect_t));

    rec->re = re;
    rec->ang = ang;
    rec->im = im;
    rec -> mag = mag;
    rec -> tostr = tostr;
    rec-> r = r;
    rec->i = i;

    return rec;     
}
