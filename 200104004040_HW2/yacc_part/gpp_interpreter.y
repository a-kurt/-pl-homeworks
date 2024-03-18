

%{
	#include <stdio.h>
	#include <string.h>
    #include <stdlib.h>

    #include "yacc.h"
%}

%union 
{
	char *string;
	int value;
	int values[1000];
}


%token <string> IDENTIFIER FILENAME VALUEF
%token <value> VALUE
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEF KW_DEFVAR KW_FOR KW_IF KW_EXIT KW_LOAD
%token  KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_OC OP_CC COMMENT
%token OP_COMMA 

%start START

%type <value> INPUT
%type <string> FUNCPARAM
%type <value> EXPI

%%

START: | START INPUT {var1 = 0; var2 = 0; sum = 0;};

INPUT:
	EXPI {      printSelector(&$$, type, var2, isNil); if (isNil == 1) isNil = 0; $$ = $1; } |
	COMMENT { printf("Result: COMMENT\n"); }|

	OP_OP OP_MINUS VALUEF VALUEF OP_CP { 
		int resultNumerator, resultDenominator;
        char* result = operateFractions($3, $4, &resultNumerator, &resultDenominator, '-');
        printf("Addition Result: %s\n", result);
	}|

    OP_OP OP_PLUS VALUEF VALUEF OP_CP { 
        int resultNumerator, resultDenominator;
        char* result = operateFractions($3, $4, &resultNumerator, &resultDenominator, '+');
        printf("Addition Result: %s\n", result);
    }|
    
    OP_OP OP_MULT VALUEF VALUEF OP_CP { 
        int resultNumerator, resultDenominator;
        char* result = operateFractions($3, $4, &resultNumerator, &resultDenominator, '*');
        printf("Multiplication Result: %s\n", result);
    }|

    OP_OP OP_DIV VALUEF VALUEF OP_CP { 
        int resultNumerator, resultDenominator;
        char* result = operateFractions($3, $4, &resultNumerator, &resultDenominator, '/');
        printf("Division Result: %s\n", result);
    };



FUNCPARAM:
	IDENTIFIER { addIdentifier($1, 0); $$ = $1; type = STRING; };

EXPI:
	OP_OP OP_PLUS EXPI EXPI OP_CP           {$$ = $3 + $4; type = INT;} |
	OP_OP OP_MINUS EXPI EXPI OP_CP          {$$ = $3 - $4; type = INT;} |
	OP_OP OP_MULT EXPI EXPI OP_CP           {$$ = $3 * $4; type = INT;} |
	OP_OP OP_DIV EXPI EXPI OP_CP            {checkZeroDivision($4); $$ = $3 / $4; type = INT; } |

	OP_OP KW_DEF IDENTIFIER OP_OP FUNCPARAM OP_CP EXPI OP_CP { addIdentifier($3, $7); $<string>$ = $3; type =  STRING; } | 

	OP_OP KW_EXIT OP_CP {printf("Exiting.\n"); exit(0);} ;




%%

int main(int argc, char *argv[])
{
    
    if (argc == 1)
    {
        printf("Type (exit) for exit\n");
        printf("Enter your input\n");
    }
    else if (argc == 2)
    {
        yyin = fopen(argv[1], "r");
        if (yyin == NULL)
        {
            printf("File not found\n");
            return 0;
        }
    }
    else 
    {
        printf("Too many arguments\n");
        printf("Usage: ./gpp_interpreter || ./gpp_interpreter <filename>");
        return 0;
    }

    yyparse();
}