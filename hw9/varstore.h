#ifndef __VARSTORE__
#define __VARSTORE__

extern void var_store_init();
extern void var_store_destroy();
extern int  var_store_get(char *name);
extern void var_store_set(char *name, int value);
#endif