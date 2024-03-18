/*==========================
    File: symbol_table.h
    Description: Helper functions and data structures.
==========================*/

#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// Enum defining different types of nodes in our Abstract Syntax Tree (AST)
typedef enum
{
    NODE_VALUEF,
    NODE_ADD,
    NODE_SUBTRACT,
    NODE_MULTIPLY,
    NODE_DIVIDE,
    NODE_FUNCTION_CALL,
    NODE_IDENTIFIER
} NodeType;

// Structure representing a node in the Abstract Syntax Tree (AST)
struct ast_node
{
    NodeType type;
    union
    {
        char *valuef;     // for constant nodes
        char *identifier; // for identifier nodes
        struct
        { // for binary operation nodes
            struct ast_node *left;
            struct ast_node *right;
        } binary;
        struct
        { // for function call nodes
            struct ast_node *name;
            struct ast_node **args;
            int arg_count;
        } function_call;
    } data;
};
typedef struct ast_node AstNode;

// Structure representing a function definition
struct function_def
{
    AstNode *name;
    AstNode *function_body;
    AstNode **params;
    int param_count;
};
typedef struct function_def function_def;

// Structure representing the symbol table that stores function definitions
struct symbol_table
{
    function_def **functions;
    int function_count;
};
typedef struct symbol_table symbol_table;

// Function to create an empty symbol table
symbol_table *create_symbol_table()
{
    symbol_table *table = (symbol_table *)malloc(sizeof(symbol_table));
    table->functions = NULL;
    table->function_count = 0;
    return table;
}

// Function to add a function definition to the symbol table
void add_function_to_symbol_table(symbol_table *table, function_def *function)
{
    table->function_count++;
    table->functions = (function_def **)realloc(table->functions, sizeof(function_def *) * table->function_count);
    table->functions[table->function_count - 1] = function;
}

// Function to look up a function in the symbol table by its name
function_def *lookup_function_in_symbol_table(symbol_table *table, AstNode *name)
{
    for (int i = 0; i < table->function_count; i++)
    {
        if (strcmp(table->functions[i]->name->data.identifier, name->data.identifier) == 0)
        {
            return table->functions[i];
        }
    }
    return NULL;
}

// Function to create a function definition
function_def *create_function(AstNode *name, AstNode **params, int param_count, AstNode *function_body)
{
    function_def *function = malloc(sizeof(function_def));
    if (function == NULL)
    {
        fprintf(stderr, "Error: Out of memory\n");
        exit(1);
    }
    function->name = name;
    function->params = params;
    function->param_count = param_count;
    function->function_body = function_body;
    return function;
}

// Function to create a constant value node in the AST
AstNode *create_node_constant(char *value)
{
    AstNode *node = (AstNode *)malloc(sizeof(AstNode));
    if (node == NULL)
    {
        fprintf(stderr, "Error: Out of memory\n");
        exit(1);
    }
    node->type = NODE_VALUEF;
    node->data.valuef = strdup(value);
    return node;
}

// Function to create an identifier node in the AST
AstNode *create_node_identifier(char *identifier)
{
    AstNode *node = (AstNode *)malloc(sizeof(AstNode));
    if (node == NULL)
    {
        fprintf(stderr, "Error: Out of memory\n");
        exit(1);
    }
    node->type = NODE_IDENTIFIER;
    node->data.identifier = strdup(identifier);
    return node;
}

// Function to create a binary operation node in the AST
AstNode *create_node_binary(NodeType type, AstNode *left, AstNode *right)
{
    AstNode *node = (AstNode *)malloc(sizeof(AstNode));
    if (node == NULL)
    {
        fprintf(stderr, "Error: Out of memory\n");
        exit(1);
    }
    node->type = type;
    node->data.binary.left = left;
    node->data.binary.right = right;
    return node;
}

// Function to create a function call node in the AST
AstNode *create_node_function_call(AstNode *name, AstNode **args, int arg_count)
{
    AstNode *node = (AstNode *)malloc(sizeof(AstNode));
    if (node == NULL)
    {
        fprintf(stderr, "Error: Out of memory\n");
        exit(1);
    }
    node->type = NODE_FUNCTION_CALL;
    node->data.function_call.name = name;
    node->data.function_call.args = args;
    node->data.function_call.arg_count = arg_count;
    return node;
}

// Function to calculate the greatest common divisor of two integers
int gcd(int a, int b)
{
    if (b == 0)
    {
        return a;
    }
    return gcd(b, a % b);
}

// Function to simplify a fraction represented by its numerator and denominator
void simplifyFraction(int *numerator, int *denominator)
{
    int common = gcd(*numerator, *denominator);
    *numerator /= common;
    *denominator /= common;
}

// Function to perform fractional arithmetic based on the operator type
char *operationVALUEF(char operatorType, char *a, char *b)
{
    int lenA = strlen(a);
    int lenB = strlen(b);

    int posBInA = strchr(a, 'b') - a;
    int posBInB = strchr(b, 'b') - b;

    char beforeDecimalA[posBInA + 1];
    char afterDecimalA[lenA - posBInA];
    char beforeDecimalB[posBInB + 1];
    char afterDecimalB[lenB - posBInB];

    strncpy(beforeDecimalA, a, posBInA);
    beforeDecimalA[posBInA] = '\0';
    strcpy(afterDecimalA, a + posBInA + 1);

    strncpy(beforeDecimalB, b, posBInB);
    beforeDecimalB[posBInB] = '\0';
    strcpy(afterDecimalB, b + posBInB + 1);

    int intBeforeA = atoi(beforeDecimalA);
    int intAfterA = atoi(afterDecimalA);
    int intBeforeB = atoi(beforeDecimalB);
    int intAfterB = atoi(afterDecimalB);

    int resultBefore = 0;
    int resultAfter = 0;

    switch (operatorType)
    {
    case '+':
        resultBefore = intBeforeA * intAfterB + intBeforeB * intAfterA;
        resultAfter = intAfterA * intAfterB;
        break;
    case '-':
        resultBefore = intBeforeA * intAfterB - intBeforeB * intAfterA;
        resultAfter = intAfterA * intAfterB;
        break;
    case '*':
        resultBefore = intBeforeA * intBeforeB;
        resultAfter = intAfterA * intAfterB;
        break;
    case '/':
        if (intBeforeB == 0 || intAfterB == 0)
        {
            fprintf(stderr, "Division by zero is not allowed.\n");
            return NULL;
        }

        resultBefore = intBeforeA * intAfterB;
        resultAfter = intAfterA * intBeforeB;

        break;
    default:
        fprintf(stderr, "Invalid operator type: %c\n", operatorType);
        return NULL;
    }

    simplifyFraction(&resultBefore, &resultAfter);

    char *result = (char *)malloc(20);
    snprintf(result, 20, "%db%d", resultBefore, resultAfter);
    return result;
}

// Function to evaluate an AST node and return the result as a string (2b4)
char *eval(AstNode *node, symbol_table *table)
{
    if (node == NULL)
    {
        fprintf(stderr, "Error: Null node encountered.\n");
        exit(EXIT_FAILURE);
    }

    switch (node->type)
    {
    case NODE_VALUEF:
        return node->data.valuef;
    case NODE_IDENTIFIER:
    {
        function_def *function = lookup_function_in_symbol_table(table, node);
        if (function == NULL)
        {
            return node->data.identifier;
        }

        return eval(function->function_body, table);
    }
    case NODE_ADD:
    {
        char *left_value = eval(node->data.binary.left, table);
        char *right_value = eval(node->data.binary.right, table);
        char *result = operationVALUEF('+', left_value, right_value);
        free(left_value);
        free(right_value);
        return result;
    }
    case NODE_SUBTRACT:
    {
        char *left_value = eval(node->data.binary.left, table);
        char *right_value = eval(node->data.binary.right, table);
        char *result = operationVALUEF('-', left_value, right_value);
        free(left_value);
        free(right_value);
        return result;
    }
    case NODE_MULTIPLY:
    {
        char *left_value = eval(node->data.binary.left, table);
        char *right_value = eval(node->data.binary.right, table);
        char *result = operationVALUEF('*', left_value, right_value);
        free(left_value);
        free(right_value);
        return result;
    }
    case NODE_DIVIDE:
    {
        char *left_value = eval(node->data.binary.left, table);
        char *right_value = eval(node->data.binary.right, table);
        char *result = operationVALUEF('/', left_value, right_value);
        free(left_value);
        free(right_value);
        return result;
    }
    case NODE_FUNCTION_CALL:
    {
        function_def *function = lookup_function_in_symbol_table(table, node->data.function_call.name);

        if (function == NULL)
        {
            fprintf(stderr, "Error: Undefined function '%s'\n", node->data.function_call.name->data.identifier);
            exit(EXIT_FAILURE);
        }

        if (node->data.function_call.arg_count != function->param_count)
        {
            fprintf(stderr, "Error: Function '%s' expects %d arguments, but %d were provided\n",
                    node->data.function_call.name->data.identifier, function->param_count, node->data.function_call.arg_count);
            exit(EXIT_FAILURE);
        }

        char *evaluated_args[100];
        for (int i = 0; i < function->param_count; i++)
        {
            evaluated_args[i] = eval(node->data.function_call.args[i], table);
        }
        if (function->param_count == 1)
        {
            function->function_body->data.binary.left = create_node_constant(evaluated_args[0]);
            function->function_body->data.binary.right = create_node_constant(evaluated_args[0]);
        }
        else if (function->param_count == 2)
        {
            function->function_body->data.binary.left = create_node_constant(evaluated_args[0]);
            function->function_body->data.binary.right = create_node_constant(evaluated_args[1]);
        }
        char *result = eval(function->function_body, table);

        return result;
    }
    default:
        fprintf(stderr, "Error: Unsupported node type\n");
        exit(EXIT_FAILURE);
    }
}

#endif // SYMBOL_TABLE_H

/*
    Parse tree yi hayal et.
*/