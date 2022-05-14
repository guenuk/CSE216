//
// vector3.c
//
#include "common.h"
#include "vector3.h"

//wirte [x, y, z] to buf
static int tostr(char *buf, vec3_t *self) {
    int i = 0;
    i += sprintf(buf + i, "[ ");
    i += self->x->tostr(buf + i, self->x);  i += sprintf(buf + i, ", ");
    i += self->y->tostr(buf + i, self->y);  i += sprintf(buf + i, ", ");
    i += self->z->tostr(buf + i, self->z);  i += sprintf(buf + i, " ]");
    return i;
}

//TODO: implement vec3_release
static void vec3_release(refobj_t *self) {
    vec3_t *vec = (vec3_t *)self; 
    refobj_decref(&vec->ref);
    if(vec-> ref.cnt_ref ==0){
        vec-> x -> ref.release(&vec -> x->ref);
        vec-> y -> ref.release(&vec -> y->ref);
        vec-> z -> ref.release(&vec -> z->ref);
        refobj_free(&vec->ref);
    }
}

//TODO: implement vec3_make
vec3_t *vec3_make(complex_t *x, complex_t *y, complex_t *z) {
    vec3_t* vec = refobj_alloc(OBJ_VEC3, sizeof(vec3_t));
    vec->ref.release = vec3_release;

    vec -> tostr = tostr; 
    // vec -> vec3_release = vec3_release;
    vec -> x = x; 
    vec -> y = y; 
    vec -> z = z;
    vec-> x->ref.addref(&vec->x->ref);
    vec-> y->ref.addref(&vec->y->ref);
    vec-> z->ref.addref(&vec->z->ref);
    return vec;     
}

//TODO: implement vec3_add
//e.g. add([1, 2, 3], [4, 5, 6]) = [1+4, 2+5, 3+6]
vec3_t *vec3_add(vec3_t *a, vec3_t *b) {
    

    complex_t *nx = complex_add(a->x, b->x);

    complex_t *ny = complex_add(a->y, b->y);
    complex_t *nz = complex_add(a->z, b->z);
    
    vec3_t *ret = vec3_make(nx,ny,nz);
    return ret; 
}

//TODO: implement vec3_sub
//e.g. sub([1, 2, 3], [4, 5, 6]) = [1-4, 2-5, 3-6]
vec3_t *vec3_sub(vec3_t *a, vec3_t *b) {
    complex_t *nx = complex_sub(a->x, b->x);
    complex_t *ny = complex_sub(a->y, b->y);
    complex_t *nz = complex_sub(a->z, b->z);
    vec3_t *ret = vec3_make(nx,ny,nz);
    return ret;
}

//TODO: implement vec3_smul
//scalar multiplication, e.g. smul(2, [1, 2, 3]) = [2, 4, 6]
vec3_t *vec3_smul(complex_t *s, vec3_t *a) {
    complex_t *nx = complex_mul(s, a->x);
    complex_t *ny = complex_mul(s, a->y);
    complex_t *nz = complex_mul(s, a->z);
    vec3_t *ret = vec3_make(nx,ny,nz);
    return ret;
}

//TODO: implement vec3_prod
//inner product, e.g. prod([1, 2, 3], [4, 5, 6]) = 1*4 + 2*5 + 3*6 = 32
complex_t *vec3_prod(vec3_t *a, vec3_t *b) {
    complex_t *nx = complex_mul(b->x, a->x);
    complex_t *ny = complex_mul(b->y, a->y);
    complex_t *nz = complex_mul(b->z, a->z);
    complex_t *ret = complex_add(complex_add(nx, ny),nz);
    return ret;
}
