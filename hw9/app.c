#include "refobj.h"
#include "stmt.h"
#include "varstore.h"
#include "parser.h"

int main(int argc, char **argv) {
    char *fname = NULL;
    if(argc == 1)
        fname = NULL;
    else if(argc == 2)
        fname = argv[1];
    else
        ON_FALSE_EXIT(0, strmsg("Usage: %s [program_file]", argv[0]));

    //parse program
    stmt_t *stmt = parse(fname);
    stmt->print(stmt);

    //init variable and execute stmt
    var_store_init();
    stmt->exec(stmt);
    var_store_destroy();

    //release statement
    stmt->ref.release(&stmt->ref);
    refobj_check_dealloc();
    
    return 0;
}
