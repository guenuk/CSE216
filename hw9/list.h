//
// list.h
//
#ifndef __LIST__
#define __LIST__

//offset of m in st
#define offsetof(st, m)         ( (size_t) &(((st *)0)->m) )

//container address when the address of m in st is ptr
#define containerof(ptr, st, m) ((st *) (((char*)(ptr)) - offsetof(st, m)))

//list_t will be embedded in a container
typedef struct list {
    struct list *next;
    struct list *prev;
} list_t;

extern void list_init_head(list_t *head);
extern int list_size(list_t *head); 
extern int list_is_empty(list_t *head);
extern void list_add_to_first(list_t *head, list_t *node);
extern void list_add_to_last(list_t *head, list_t *node);
extern void list_add_after(list_t *pos, list_t *node);
extern void list_add_before(list_t *pos, list_t *node);
extern list_t *list_remove(list_t *node);
extern list_t *list_remove_first(list_t *head);
extern list_t *list_remove_last(list_t *head);
extern list_t *list_find(list_t *head, void *data, int (*comp)(list_t*, void*));

#endif
