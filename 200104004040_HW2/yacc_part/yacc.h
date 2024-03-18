#ifndef __YACC_H__
#define __YACC_H__

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <string.h>

extern FILE *yyin; 

int yylex(); 
int yyerror(const char *error)
{
    printf("Syntax Error\n");
    exit(1);
}


typedef enum {
    INT,
    STRING,
    BOOL,
    LIST
} Type;

Type type = INT;

struct IdentifierStruct
{ 
    char identifier[1000][200]; 
    int values[1000]; 
    int count;
};

struct IdentifierStruct identifiers = {"", 0, 0};


char* operateFractions(char* fraction1, char* fraction2, int* resultNumerator, int* resultDenominator, char operation) {
    int numerator1, denominator1, numerator2, denominator2;

    // Parse the input strings
    sscanf(fraction1, "%df%d", &numerator1, &denominator1);
    sscanf(fraction2, "%df%d", &numerator2, &denominator2);

    // Perform the specified operation
    switch (operation) {
        case '+':
            *resultNumerator = numerator1 * denominator2 + numerator2 * denominator1;
            *resultDenominator = denominator1 * denominator2;
            break;
        case '-':
            *resultNumerator = numerator1 * denominator2 - numerator2 * denominator1;
            *resultDenominator = denominator1 * denominator2;
            break;
        case '*':
            *resultNumerator = numerator1 * numerator2;
            *resultDenominator = denominator1 * denominator2;
            break;
        case '/':
            *resultNumerator = numerator1 * denominator2;
            *resultDenominator = denominator1 * numerator2;
            break;
        default:
            printf("Unsupported operation\n");
            return NULL;
    }

    // Allocate memory for the result string
    char* resultString = (char*)malloc(20); // Adjust the size as needed

    // Format the result as a string
    sprintf(resultString, "%df%d", *resultNumerator, *resultDenominator);

    return resultString;
}

void addFunction(char *funcName, char *param1, char *param2, char *expression) {

    printf("Adding function: %s(%s, %s) => %s\n", funcName, param1, param2, expression);
}

int callFunction(char *funcName, int arg1, int arg2) {

    printf("Calling function: %s(%d, %d)\n", funcName, arg1, arg2);


    return 0;
}

int getIDentifierIndex(char *searchStr)
{
    for(int i = 0; i < identifiers.count; i++)
        if (strcmp(searchStr, identifiers.identifier[i]) == 0)
            return i;

    return -1;
}


void addIdentifier(char *identifier, int value)
{
    int index = getIDentifierIndex(identifier);
    // if given identifier does not exist, add
    if(index == -1)
    {
        strcpy(identifiers.identifier[identifiers.count], identifier);
        identifiers.values[identifiers.count] = value;
        identifiers.count += 1;
    }
    // if already exist, update value
    else
        identifiers.values[index] = value;

}



// open file
int loadFile(char *fileName)
{
    printf("Loading file: %s\n", fileName);
    FILE *file = fopen(fileName, "r");
    if(file == NULL)
    {
        printf("Error: File %s not found.\n", fileName);
        return 0;
    }
    return 1;
}


void printSelector(void* param, Type type, int size, int isList)
{
    printf("Result: ");
    if (type == INT)
    {
        printf("%d", *(int*)param);
    }
    else if (type == STRING)
    {
        char **temp = (char*)param;
        char *str = (char*)temp[0];
        while (*str != '\0')
        {
            printf("%c", *str);
            str++;
        }
    }
    else if (type == LIST)
    {
        printf("(");
        for (int i = 0; i < size; i++)
        {
            printf("%d", *(int*)((char*)param + i * sizeof(int)));
            if (i != size - 1)
            {
                printf(" ");
            }
        }
        printf(")");
    }
    
    printf("\n");
}

void checkZeroDivision(int _value)
{
    if (_value == 0)
    {
        printf("Zero Division Error.\n");
        exit(1);
    }
}


int var1 = 0;
int var2 = 0;
int sum = 0;
int isNil = 0;

#endif