//
// list.c
//
#include "list.h"
#include <stdio.h>

//init head
void list_init_head(list_t *head) {
    head->next = head;
    head->prev = head;
}

//size
int list_size(list_t *head) {
    int size = 0;
    list_t *pos;
    for(pos = head->next; pos != head; pos = pos->next)
        size++;
    return size;
}

//is empty
int list_is_empty(list_t *head) {
    return head->next == head;    
}

//add methods
static void list_add_between(list_t *prev, list_t *succ, list_t *node) {
    node->prev = prev;
    node->next = succ;
    prev->next = node;
    succ->prev = node;
}

void list_add_to_first(list_t *head, list_t *node) {
    list_add_after(head, node);
}

void list_add_to_last(list_t *head, list_t *node) {
    list_add_before(head, node);
}

void list_add_after(list_t *pos, list_t *node) {
    list_add_between(pos, pos->next, node);
}

void list_add_before(list_t *pos, list_t *node) {
    list_add_between(pos->prev, pos, node);
}

//remvoe methods
list_t *list_remove(list_t *node) {
    list_t *pred = node->prev;
    list_t *succ = node->next;
    pred->next = succ;
    succ->prev = pred;
    return node;
}

list_t *list_remove_first(list_t *head) {
    return list_remove(head->next);
}

list_t *list_remove_last(list_t *head) {
    return list_remove(head->prev);
}

//find method
list_t *list_find(list_t *head, void *data, int (*comp)(list_t*, void*)) {
    list_t *pos;
    for(pos = head->next; pos != head; pos = pos->next)
        if(comp(pos, data))
            return pos;
    return NULL;
}
