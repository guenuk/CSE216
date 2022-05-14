//
// app.c
//
#include <stdio.h>
#include "rect.h"
#include "polar.h"
#include "vector3.h"

void test_vector() {
    char buf[256];
    int i = 0;

    printf("--test vector--------------------\n");

    complex_t *x = rect_make(1, 0);
    complex_t *y = rect_make(0, 1);
    complex_t *z = rect_make(1, 1);

    vec3_t *a = vec3_make(x, x, x);
    x->ref.release(&x->ref);
    i += a->tostr(buf + i, a);    i += sprintf(buf + i, "\n");

    vec3_t *b = vec3_make(y, y, y);
    y->ref.release(&y->ref);
    i += b->tostr(buf + i, b);    i += sprintf(buf + i, "\n");

    vec3_t *c = vec3_add(a, b);
    a->ref.release(&a->ref);
    b->ref.release(&b->ref);
    i += c->tostr(buf + i, c);    i += sprintf(buf + i, "\n");

    vec3_t *d = vec3_smul(z, c);
    z->ref.release(&z->ref);
    i += d->tostr(buf + i, d);    i += sprintf(buf + i, "\n");

    complex_t *e = vec3_prod(c, d);
    i += e->tostr(buf + i, e);    i += sprintf(buf + i, "\n");
    c->ref.release(&c->ref);
    d->ref.release(&d->ref);
    e->ref.release(&e->ref);

    printf("%s", buf);
}

void test_complex() {
    char buf[256];
    int i = 0;

    printf("--test complex-------------------\n");
    

    complex_t *a = rect_make(1, 1);
    // printf("--test complex-------------------\n");
    i += a->tostr(buf + i, a);    i += sprintf(buf + i, "\n");

    complex_t *b = polar_make(1, deg2rad(45));
    i += b->tostr(buf + i, b);    i += sprintf(buf + i, "\n");

    complex_t *c = complex_mul(b, b);
    i += c->tostr(buf + i, c);    i += sprintf(buf + i, "\n");
    b->ref.release(&b->ref);

    complex_t *d = complex_add(a, c);
    
    i += d->tostr(buf + i, d);    i += sprintf(buf + i, "\n");
    a->ref.release(&a->ref);
    c->ref.release(&c->ref);
    d->ref.release(&d->ref);

    printf("%s", buf);
}

int main() {
    test_complex();
    test_vector();
    // refobj_check_dealloc();
    return 0;
}
