#include "common.h"
#include "scanner.h"
#include "expr.h"
#include "stmt.h"
#include "parser.h"
#include <stdio.h>
#include <string.h>

//forward definition
static void match(int expected);
static stmt_t *parse_program();
static stmt_t *parse_stmt();
static stmt_t *parse_stmt_assignment();
static stmt_t *parse_stmt_read();
static stmt_t *parse_stmt_write();
static stmt_t *parse_stmt_compound();
static stmt_t *parse_stmt_list();
static expr_t *parse_expr();
static expr_t *parse_expr_add();
static expr_t *parse_expr_mul();
static expr_t *parse_expr_factor();
//TODO: implement
static stmt_t *parse_stmt_if();
static stmt_t *parse_stmt_while();
static expr_t *parse_expr_comp();

//scanner state
static scan_t sc;

static void match(int expected) {
    ON_FALSE_EXIT( sc.token == expected,
                   strmsg("syntax error: in %d, expected %d, but has %d (%s)", sc.line, expected, sc.token, sc.text));
    read_token(&sc);
}

static stmt_t *parse_program() {
    stmt_t *pgm = parse_stmt();
    match(TOK_EOF);
    return pgm;
}

static stmt_t *parse_stmt() {
    stmt_t *stmt = NULL;
    if(sc.token == TOK_ID)
        stmt = parse_stmt_assignment();
    else if(sc.token == TOK_READ)
        stmt = parse_stmt_read();
    else if(sc.token == TOK_WRITE)
        stmt = parse_stmt_write();
    else if(sc.token == '{')
        stmt = parse_stmt_compound();
    //TODO: add if staement
    //TODO: add while statement        
    else if(sc.token == TOK_IF)
        stmt = parse_stmt_if();
    else if(sc.token == TOK_WHILE)
        stmt = parse_stmt_while();
    else
        ON_FALSE_EXIT(0, strmsg("syntax error: in %d near (%s), not a statement", sc.line, sc.text));
    return stmt;
}

static stmt_t *parse_stmt_assignment() {
    char *id = strdup(sc.text);
    match(TOK_ID);
    match(TOK_ASSIGN);
    expr_t *expr = parse_expr();

    stmt_t *stmt = stmt_make_assignment(id, expr);

    expr->ref.release(&expr->ref);
    free(id);
    return stmt;
}

static stmt_t *parse_stmt_read() {
    match(TOK_READ);
    char *id = strdup(sc.text);
    match(TOK_ID);

    stmt_t *stmt = stmt_make_read(id);

    free(id);
    return stmt;
}

static stmt_t *parse_stmt_write() {
    match(TOK_WRITE);
    expr_t *expr = parse_expr();
    stmt_t *stmt = stmt_make_write(expr);

    expr->ref.release(&expr->ref);
    return stmt;
}

static stmt_t *parse_stmt_compound() {
    match('{');
    stmt_t *stmt = parse_stmt_list();
    match('}');

    return stmt;
}

static stmt_t *parse_stmt_list() {
    list_t *head = malloc(sizeof(list_t));
    list_init_head(head);

    while(sc.token != '}') {
        stmt_t *stmt = parse_stmt();
        stmt_list_entry_t *entry = malloc(sizeof(stmt_list_entry_t));
        entry->stmt = stmt;
        list_add_to_last(head, &entry->lst);
    }

    stmt_t *stmt = stmt_make_compound(head);
    //addref/release to the elements are skipped
    return stmt;
}

//TODO: implement static stmt_t *parse_stmt_if()

static stmt_t *parse_stmt_if() {
    match(TOK_IF);
    match('(');
    expr_t *expr = parse_expr();
    match(')');
    stmt_t * then_stmt = parse_stmt();
    match(TOK_ELSE);
    stmt_t * else_stmt = parse_stmt();
    stmt_t *stmt = stmt_make_if(expr, then_stmt, else_stmt);
    expr->ref.release(&expr->ref);
    then_stmt->ref.release(&then_stmt->ref);
    else_stmt->ref.release(&else_stmt->ref);
    return stmt;
}
//TODO: implement static stmt_t *parse_stmt_while()
static stmt_t *parse_stmt_while() {
    match(TOK_WHILE);
    match('(');
    expr_t *expr = parse_expr();
    match(')');
    stmt_t *loop_stmt = parse_stmt();
    stmt_t *stmt = stmt_make_while(expr, loop_stmt);
    expr->ref.release(&expr->ref);
    loop_stmt->ref.release(&loop_stmt->ref);
    return stmt;
}
static expr_t *parse_expr() {
    //TODO: switch to parse_expr_comp()
    return parse_expr_comp();
}

//TODO: implement static expr_t *parse_expr_comp()
static expr_t *parse_expr_comp() {
    expr_t *op1 = parse_expr_add();
    expr_t *ret = op1;
    if(sc.token == TOK_EQ || sc.token == TOK_NE ||
       sc.token == TOK_LE || sc.token == '<' ||
       sc.token == TOK_GE || sc.token == '>') {
           int tok = sc.token;
           match(sc.token);
           expr_t *op2 = parse_expr_add();

           expr_t *tmp;
           if(tok == TOK_EQ)
                tmp = expr_make_eq(op1, op2);
           else if(tok == TOK_NE)
                tmp = expr_make_ne(op1, op2);
           else if(tok == TOK_LE)
                tmp = expr_make_le(op1, op2);
           else if(tok == TOK_GE)
                tmp = expr_make_ge(op1, op2);
           else if(tok == '<')
                tmp = expr_make_lt(op1, op2);
           else if(tok == '>')
                tmp = expr_make_gt(op1, op2);

           op1->ref.release(&op1->ref);
           op2->ref.release(&op2->ref);

           ret = op1 = tmp; 
       }
    return ret;
}

static expr_t *parse_expr_add() {
    expr_t *op1 = parse_expr_mul();
    expr_t *ret = op1;
    while(sc.token == '+' || sc.token == '-') {
        int tok = sc.token;
        match(sc.token);
        expr_t *op2 = parse_expr_mul();
        
        expr_t *tmp;
        if(tok == '+')
            tmp = expr_make_add(op1, op2);
        else
            tmp = expr_make_sub(op1, op2);

        op1->ref.release(&op1->ref);
        op2->ref.release(&op2->ref);

        ret = op1 = tmp;    //op1 is the resulting expr
    }
    return ret;
}

static expr_t * parse_expr_mul() {
    expr_t *op1 = parse_expr_factor();
    expr_t *ret = op1;
    while(sc.token == '*' || sc.token == '/') {
        int tok = sc.token;
        match(sc.token);
        expr_t *op2 = parse_expr_factor();
        
        expr_t *tmp;
        if(tok == '*')
            tmp = expr_make_mul(op1, op2);
        else
            tmp = expr_make_div(op1, op2);

        op1->ref.release(&op1->ref);
        op2->ref.release(&op2->ref);

        ret = op1 = tmp;    //op1 is the resulting expr
    }
    return ret;
}

static expr_t *parse_expr_factor() {
    expr_t *ret = NULL;
    if(sc.token == TOK_NUM) {
        int n = atoi(sc.text);      //get int value from the scanner text
        match(TOK_NUM);
        ret = expr_make_num(n);
    }
    else if(sc.token == TOK_ID) {
        char *id = strdup(sc.text); //get var name from the scanner text
        match(TOK_ID);
        ret = expr_make_var(id);
        free(id);
    }
    else if(sc.token == '(') {
        match('(');
        ret = parse_expr();
        match(')');
    }
    return ret;
}

stmt_t *parse(char *fname) {
    open_scan(&sc, fname);
    read_token(&sc);        //load the first token for the parser
    stmt_t *stmt = parse_program();
    close_scan(&sc);
    return stmt;
}

#define PARSER_APP 0
#if PARSER_APP
int main(int argc, char **argv) {
    if(argc == 1)
        parse(NULL);
    else if(argc == 2)
        parse(argv[1]);
    else
        ON_FALSE_EXIT(0, "Usage: a.exe [program_file]")

    return 0;
}
#endif
