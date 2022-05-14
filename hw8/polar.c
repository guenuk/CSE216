//
// polar.c
//
#include "complex.h"
#include "polar.h"
#include <math.h>

//TODO: implement re, re( m\a ) = m * cos a
static double re(complex_t *self) {
    polar_t *pol = (polar_t*)self;
    return pol->m * cos (pol->a);
}

//TODO: implement im, im( m\a ) = m * sin a
static double im(complex_t *self) {
    polar_t *pol = (polar_t*)self;
    return pol->m * sin (pol->a);
}

//TODO: implement mag, mag( m\a ) = m
static double mag(complex_t *self) {
    polar_t *pol = (polar_t*)self;
    return pol->m;
}

//TODO: implement ang, ang( m\a ) = a
static double ang(complex_t *self) {
    polar_t *pol = (polar_t*)self;
    return pol->a;
}

//write m\a to buf
static int tostr(char *buf, complex_t *self) {
    polar_t *pol = (polar_t*)self;
    ON_FALSE_EXIT(self->ref.tag == OBJ_POLAR, strmsg("not polar type"));
    return sprintf(buf, "%g \\ %g", pol->m, pol->a);
}

//TODO: implement polar_make
complex_t* polar_make(double m, double a) {
    polar_t* pol = refobj_alloc(OBJ_POLAR, sizeof(polar_t));

    pol->re = re;
    pol->ang = ang;
    pol->im = im;
    pol -> mag = mag;
    pol -> tostr = tostr; 
    
    pol->m = m;
    pol->a = a;

    return pol;     
}

//deg to radian
double deg2rad(double deg) {
    double pi = acos(-1);
    return pi / 180 * deg;
}

//radian to deg
double rad2deg(double rad) {
    double pi = acos(-1);
    return 180 / pi * rad;
}
