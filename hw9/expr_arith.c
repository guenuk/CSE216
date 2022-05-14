//
// expr_arith.c
//  arithmetic operators
//

#include "expr.h"
#include "expr_opr.h"

//add
//
static int    int_add(int a, int b) { return a + b; }
static int   eval_add(expr_t *self) { return expr_eval_opr(self, int_add); }
static void print_add(expr_t *self) { expr_print_opr(self, " + "); }
expr_t *expr_make_add(expr_t *a, expr_t *b) {
    expr_opr_t* expr = expr_make_opr(a, b);
    expr->eval       = eval_add;
    expr->print      = print_add;
    return (expr_t*)expr;
}

//sub
//
static int    int_sub(int a, int b) { return a - b; }
static int   eval_sub(expr_t *self) { return expr_eval_opr(self, int_sub); }
static void print_sub(expr_t *self) { expr_print_opr(self, " - "); }
expr_t *expr_make_sub(expr_t *a, expr_t *b) {
    expr_opr_t* expr = expr_make_opr(a, b);
    expr->eval       = eval_sub;
    expr->print      = print_sub;
    return (expr_t*)expr;
}

//mul
//
static int    int_mul(int a, int b) { return a * b; }
static int   eval_mul(expr_t *self) { return expr_eval_opr(self, int_mul); }
static void print_mul(expr_t *self) { expr_print_opr(self, " * "); }
expr_t *expr_make_mul(expr_t *a, expr_t *b) {
    expr_opr_t* expr = expr_make_opr(a, b);
    expr->eval       = eval_mul;
    expr->print      = print_mul;
    return (expr_t*)expr;
}

//div
//
static int    int_div(int a, int b) { return a / b; }
static int   eval_div(expr_t *self) { return expr_eval_opr(self, int_div); }
static void print_div(expr_t *self) { expr_print_opr(self, " / "); }
expr_t *expr_make_div(expr_t *a, expr_t *b) {
    expr_opr_t* expr = expr_make_opr(a, b);
    expr->eval       = eval_div;
    expr->print      = print_div;
    return (expr_t*)expr;
}

