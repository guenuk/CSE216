//
// expr_comp.c
//  comparators
//

#include "expr.h"
#include "expr_opr.h"

/* TODO: add comparators: impelement*/ 
    static int int_eq(int a, int b){
        return a==b;
    }
    static int eval_eq(expr_t *self){
        return expr_eval_opr(self, int_eq);
    }
    static void print_eq(expr_t *self){
        expr_print_opr(self, " == "); 
    }

    expr_t *expr_make_eq(expr_t *a, expr_t *b){
        expr_opr_t* expr = expr_make_opr(a, b);
        expr->eval       = eval_eq;
        expr->print      = print_eq;
        return (expr_t*)expr;
    }

    static int int_ne(int a, int b){
        return a!=b; 
    }
    static int eval_ne(expr_t *self){
        return expr_eval_opr(self, int_ne);
    }
    static void print_ne(expr_t *self){
        expr_print_opr(self, " != "); 
    }
    expr_t *expr_make_ne(expr_t *a, expr_t *b){
        expr_opr_t* expr = expr_make_opr(a, b);
        expr->eval       = eval_ne;
        expr->print      = print_ne;
        return (expr_t*)expr;

    }

    static int int_ge(int a, int b){
        return a>=b;
    }
    static int eval_ge(expr_t *self){
        return expr_eval_opr(self, int_ge);
    }
    static void print_ge(expr_t *self){
        expr_print_opr(self, " >= "); 
    }
    expr_t *expr_make_ge(expr_t *a, expr_t *b){
        expr_opr_t* expr = expr_make_opr(a, b);
        expr->eval       = eval_ge;
        expr->print      = print_ge;
        return (expr_t*)expr;

    }

    static int int_gt(int a, int b){
        return a>b;
    }
    static int eval_gt(expr_t *self){
        return expr_eval_opr(self, int_gt);
    }
    static void print_gt(expr_t *self){
        expr_print_opr(self, " > "); 
    }
    expr_t *expr_make_gt(expr_t *a, expr_t *b){
        expr_opr_t* expr = expr_make_opr(a, b);
        expr->eval       = eval_gt;
        expr->print      = print_gt;
        return (expr_t*)expr;
    }

    static int int_le(int a, int b){
        return a<=b;
    }
    static int eval_le(expr_t *self){
        return expr_eval_opr(self, int_le);
    }
    static void print_le(expr_t *self){
        expr_print_opr(self, " <= "); 
    }
    expr_t *expr_make_le(expr_t *a, expr_t *b){
        expr_opr_t* expr = expr_make_opr(a, b);
        expr->eval       = eval_le;
        expr->print      = print_le;
        return (expr_t*)expr;
    }

    static int int_lt(int a, int b){
        return a<b;
    }
    static int eval_lt(expr_t *self){
        return expr_eval_opr(self, int_lt);
    }
    static void print_lt(expr_t *self){
        expr_print_opr(self, " < "); 
    }
    expr_t *expr_make_lt(expr_t *a, expr_t *b){
        expr_opr_t* expr = expr_make_opr(a, b);
        expr->eval       = eval_lt;
        expr->print      = print_lt;
        return (expr_t*)expr;
    }

