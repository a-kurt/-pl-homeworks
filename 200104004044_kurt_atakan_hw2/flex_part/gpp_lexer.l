
%{
    #include <stdio.h>
%}

%%


"and"       { printf("KW_AND\n"); }
"or"        { printf("KW_OR\n"); }
"not"       { printf("KW_NOT\n"); }
"equal"     { printf("KW_EQUAL\n"); }
"less"      { printf("KW_LESS\n"); }
"nil"       { printf("KW_NIL\n"); }
"list"      { printf("KW_LIST\n"); }
"append"    { printf("KW_APPEND\n"); }
"concat"    { printf("KW_CONCAT\n"); }
"set"       { printf("KW_SET\n"); }
"def"       { printf("KW_DEF\n"); }
"for"       { printf("KW_FOR\n"); }
"if"        { printf("KW_IF\n"); }
"exit"      { printf("KW_EXIT\n"); return 0; }
"load"      { printf("KW_LOAD\n"); }
"display"   { printf("KW_DISPLAY\n"); }
"true"      { printf("KW_TRUE\n"); }
"false"     { printf("KW_FALSE\n"); }


"+"         { printf("OP_PLUS\n"); }
"-"         { printf("OP_MINUS\n"); }
"/"         { printf("OP_DIV\n"); }
"*"         { printf("OP_MULT\n"); }
"("         { printf("OP_OP\n"); }
")"         { printf("OP_CP\n"); }
","         { printf("OP_COMMA\n"); }


";;".*      { printf("COMMENT\n"); }
[0-9]+b[0-9]+ { printf("VALUEF\n"); }
[a-zA-Z][a-zA-Z0-9]* { printf("IDENTIFIER\n"); }

[a-zA-Z][a-zA-Z0-9]*[+\-*/] { printf("SYNTAX_ERROR: No space between identifier and operator: '%s'\n", yytext); return 0; }
[+\-*/][a-zA-Z][a-zA-Z0-9]* { printf("SYNTAX_ERROR: No space between operator and identifier: '%s'\n", yytext); return 0; }


"\n"        {;}
" "         {;}
"\t"        {;}

.           { printf("SYNTAX_ERROR '%s' cannot be tokenized\n", yytext); return 0; }

%%

int yywrap() {
    return 0;
}

int main() {
    FILE *fp;
    fp = fopen("./input.txt", "r");
    if (!fp) {
        printf("Failed to open input file.");
        return 0;
    }
    yyin = fp;

    yylex();
    fclose(fp);
    return 0;
}
