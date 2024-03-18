/*===========================
   Atakan Kurt
   g++ interpreter
===========================*/
%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "symbol_table.h"
    symbol_table* table;
    extern int yylex();
    void yyerror (char *s);
    int yy_scan_string(const char *str);
%}

/* Data types for the parser */
%union {
    char* str;
    char* valuef;
    char* identifier;
    struct ast_node* node;
    struct function_def* function;
}

/* Tokens for the lexer */
%token <valuef> VALUEF
%token <identifier> IDENTIFIER
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEF KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISPLAY KW_TRUE KW_FALSE 
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_COMMA COMMENT

/* The starting symbol and types for parsing */
%start START
%type <str> INPUT
%type <node> EXP
%type <function> FUNCTION

/* The grammar rules */
%% 
START: | START INPUT 

INPUT: EXP { printf("%s\n", eval($1, table)); }
    | FUNCTION { add_function_to_symbol_table(table, $1); printf("#function\n"); }
    | OP_OP KW_EXIT OP_CP {exit(EXIT_SUCCESS);}
    | COMMENT { printf("Result: COMMENT\n"); }
    ;

/* Rules for mathematical expressions */
EXP: OP_OP OP_PLUS EXP EXP OP_CP { $$ = create_node_binary(NODE_ADD, $3, $4); }
    | OP_OP OP_MINUS EXP EXP OP_CP { $$ = create_node_binary(NODE_SUBTRACT, $3, $4); }
    | OP_OP OP_MULT EXP EXP OP_CP { $$ = create_node_binary(NODE_MULTIPLY, $3, $4); }
    | OP_OP OP_DIV EXP EXP OP_CP { $$ = create_node_binary(NODE_DIVIDE, $3, $4); }
    | OP_OP IDENTIFIER EXP OP_CP { $$ = create_node_function_call(create_node_identifier($2), (AstNode*[]){$3}, 1); }
    | OP_OP IDENTIFIER EXP EXP OP_CP { $$ = create_node_function_call(create_node_identifier($2), (AstNode*[]){$3, $4}, 2); }
    | IDENTIFIER { $$ = create_node_identifier($1); }
    | VALUEF { $$ = create_node_constant($1); }
    ;

/* Rules for function definitions */
FUNCTION: OP_OP KW_DEF IDENTIFIER EXP OP_CP { $$ = create_function(create_node_identifier($3), NULL, 0, $4); }
        | OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP { $$ = create_function(create_node_identifier($3), (AstNode*[]){create_node_identifier($4)}, 1, $5); }
        | OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP { $$ = create_function(create_node_identifier($3), (AstNode*[]){create_node_identifier($4), create_node_identifier($5)}, 2, $6); }
    ;

%%

/* Error handling function */
void yyerror(char *s) {
    fprintf(stderr, "Error: %s\n", s);
}

/* Main function */
int main(int argc, char *argv[]) {
    table = create_symbol_table();
    FILE *file;
    if (argc > 1) {
        file = fopen(argv[1], "r");
        if (file == NULL) {
            printf("Cannot open file\n");
            exit(0);
        }
    } else {
        file = stdin;
    }
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        yy_scan_string(line);
        yyparse();
    }
    if (argc > 1) {
        fclose(file);
    }
    return 0;
}